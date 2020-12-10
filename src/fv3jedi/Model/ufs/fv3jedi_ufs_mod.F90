! (C) Copyright 2020 NOAA
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module fv3jedi_ufs_mod
  
  ! oops
  use datetime_mod
  use duration_mod

  ! fckit
  use fckit_configuration_module, only: fckit_configuration
  
  ! fv3jedi
  use fv3jedi_geom_mod,      only: fv3jedi_geom
  use fv3jedi_state_mod,     only: fv3jedi_state
  
  ! ufs
  use ESMF
  use NUOPC
  use NUOPC_Driver
  use module_EARTH_GRID_COMP, only: esmSS => EARTH_REGISTER
  !use module_nems_utils, only: message_check
  use mpp_mod,            only: read_input_nml,mpp_pe
  

  implicit none
  private
  
  public :: model_ufs
  
  !> Fortran derived type to hold model definition
  type :: model_ufs
     type(ESMF_GridComp) :: esmComp
     type(ESMF_State) :: toJedi, fromJedi 
     type(esmf_Clock) :: clock
   contains
     procedure :: create
     procedure :: delete
     procedure :: initialize
     procedure :: step
     procedure :: finalize
  end type model_ufs
  
  character(len=*), parameter :: modname='fv3jedi_ufs_mod'
  
  ! --------------------------------------------------------------------------------------------------

contains
  
  ! --------------------------------------------------------------------------------------------------
  
  subroutine create(self, conf, geom)
    
    implicit none
    class(model_ufs),          intent(inout) :: self
    type(fckit_configuration), intent(in)    :: conf
    type(fv3jedi_geom),        intent(in)    :: geom
    
    integer :: rc, urc, phase
    character(len=20) :: cdate_start, cdate_stop

    type(ESMF_Time)         :: startTime, stopTime
    type(ESMF_TimeInterval) :: timeStep

    character(len=*),parameter :: subname = modname//' (create)'
    
    ! Initialize ESMF
    call ESMF_Initialize(logkindflag=esmf_LOGKIND_MULTI, &
         defaultCalkind=esmf_CALKIND_GREGORIAN, &
         mpiCommunicator=geom%f_comm%communicator(), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! Flush log output while debugging
    call ESMF_LogSet(flush=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    call ESMF_LogWrite("ESMF Initialized in "//subname, ESMF_LOGMSG_INFO)

    !self%cf_main=esmf_configcreate(rc=rc)   
    !call ESMF_ConfigLoadFile(config=self%cf_main, &
    !     filename='model_configure', &
    !     rc=rc)
    
    ! This call to read_input_nml() seems to be required
    ! for CCPP.  However, it does not belong at this level
    ! but should be handled inside the model itself
    call read_input_nml()
    call ESMF_LogWrite("done reading input nml", ESMF_LOGMSG_INFO)

    ! Create the ESM component
    self%esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! SetServices for the ESM component
    call ESMF_GridCompSetServices(self%esmComp, esmSS, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! Set ESM's Verbosity  - 32513
    call NUOPC_CompAttributeSet(self%esmComp, name="Verbosity", &
         value="32513", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    
    ! Initialize the clock

    ! hard code 1 hour for now
    call ESMF_TimeIntervalSet(timeStep, s=3600, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! hard code for testing
    cdate_start = "2019-08-29T03:00:00Z"
    call ESMF_TimeSet(startTime, timeString=cdate_start,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! hard code for testing
    cdate_stop = "2019-08-29T04:00:00Z"
    call ESMF_TimeSet(stopTime, timeString=cdate_stop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    self%clock = ESMF_ClockCreate(name="ESM Clock", &
         startTime=startTime, stopTime=stopTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! Create import and export states from
    ! perspective of the exernal system:
    !   toJedi is an IMPORT into Jedi and an EXPORT from ESM
    !   fromJedi is an EXPORT from Jedi and an IMPORT into ESM

    self%toJedi = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    
    self%fromJedi = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    call ESMF_LogWrite("Exit "//subname, ESMF_LOGMSG_INFO)

  end subroutine create
  
! --------------------------------------------------------------------------------------------------

  subroutine delete(self)
    
    implicit none
    class(model_ufs), intent(inout) :: self
    integer :: rc
    character(len=*),parameter :: subname = modname//' (delete)'

    call ESMF_LogWrite("Enter "//subname, ESMF_LOGMSG_INFO)

    call ESMF_GridCompDestroy(self%esmComp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
   
    call ESMF_LogWrite("About to destroy toJedi state "//subname, ESMF_LOGMSG_INFO)

    call ESMF_StateDestroy(self%toJedi, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    call ESMF_LogWrite("About to destroy fromJedi state "//subname, ESMF_LOGMSG_INFO)

    call ESMF_StateDestroy(self%fromJedi, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    ! TODO:  the finalization process does not seem to get this far.
    ! Something might be killing the MPI processes before it gets here....

    call ESMF_LogWrite("About to finalize ESMF "//subname, ESMF_LOGMSG_INFO)

    ! Finalize ESMF
    ! -------------
    call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=rc)
    if (rc /= ESMF_SUCCESS) then
       print *, "ERROR FINALIZING ESMF"
    else
       print *, "SUCCESSFULLY FINALIZED ESMF"
    endif

  end subroutine delete

! --------------------------------------------------------------------------------------------------

  subroutine initialize(self, state, vdate)
    
    implicit none

    class(model_ufs),    intent(inout) :: self
    type(fv3jedi_state), intent(in)    :: state
    type(datetime),      intent(in)    :: vdate

    integer :: rc, urc, phase, i, cnt
    type(ESMF_CplComp),  pointer       :: connectors(:)
    character(len=128) :: name, msg

    type(ESMF_Field),       pointer :: stateFieldList(:)

    character(len=*),parameter :: subname = modname//' (initialize)'

    call ESMF_LogWrite("Enter "//subname, ESMF_LOGMSG_INFO)


#define ADVERTISE_EXPORTS
#ifdef ADVERTISE_EXPORTS
   
    call ESMF_LogWrite("Advertising export from ESM", ESMF_LOGMSG_INFO)
    ! Advertise fields on the exportState, for data coming out of ESM component
    call NUOPC_Advertise(self%toJedi, &
         StandardName="inst_zonal_wind_levels", &
         SharePolicyField="share", &
         TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

#endif
    
#define ADVERTISE_IMPORTS__off
#ifdef ADVERTISE_IMPORTS
    
    call ESMF_LogWrite("Advertising imports to ESM", ESMF_LOGMSG_INFO)
    ! Advertise fields on the importState, for data going into ESM component
    call NUOPC_Advertise(self%fromJedi, &
         StandardNames=(/ &
         "inst_down_lw_flx              ", &
         "inst_down_sw_flx              ", &
         "inst_temp_height2m            "/), &
         TransferOfferGeomObject="cannot provide", rc=rc)
    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    
#endif

    call ESMF_StateGet(self%toJedi, itemCount=cnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    write(msg, "(I2)") cnt
    call ESMF_LogWrite("After filling advertise toJedi state has "//trim(msg)//" items.", &
         ESMF_LOGMSG_INFO)


    ! call ExternalAdvertise phase
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
         methodflag=ESMF_METHOD_INITIALIZE, &
         phaseLabel=label_ExternalAdvertise, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    
    call ESMF_GridCompInitialize(self%esmComp, phase=phase, &
         importState=self%fromJedi, exportState=self%toJedi, &
         clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    !call ESMF_StateGet(self%toJedi, itemCount=cnt, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !     line=__LINE__, file=__FILE__)) then
    !   call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !   return
    !endif
    !write(msg, "(I2)") cnt
    !call ESMF_LogWrite("After calling advertise toJedi state has "//trim(msg)//" items.", &
    !     ESMF_LOGMSG_INFO)

    
    ! Set verbosity flag on connectors
    
    call NUOPC_DriverGetComp(self%esmComp, & 
         compList=connectors, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    
    call ESMF_LogWrite("About to set connector verbosity", ESMF_LOGMSG_INFO)
    do i=lbound(connectors,1), ubound(connectors,1)
       call ESMF_CplCompGet(connectors(i), name=name, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) then
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
          return
       endif
       call NUOPC_CompAttributeSet(connectors(i), name="Verbosity", & 
            value="max", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) then
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
          return
       endif
       call ESMF_LogWrite(" --> Set verbosity on connector: "//trim(name), & 
            ESMF_LOGMSG_INFO)
    enddo
     
    deallocate(connectors)

    ! call ExternalRealize phase
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
         methodflag=ESMF_METHOD_INITIALIZE, &
         phaseLabel=label_ExternalRealize, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    call ESMF_GridCompInitialize(self%esmComp, phase=phase, &
         importState=self%fromJedi, exportState=self%toJedi, &
         clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    call ESMF_StateGet(self%toJedi, itemCount=cnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    write(msg, "(I2)") cnt

    call ESMF_LogWrite("Dumping toJedi state with "//trim(msg)//" items", & 
         ESMF_LOGMSG_INFO)

#if 0
    call NUOPC_Write(self%toJedi, &
         fileNamePrefix="diagnostic_postrealize_toJedi_", &
         overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
#endif

    ! call ExternalDataInit phase
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
         methodflag=ESMF_METHOD_INITIALIZE, &
         phaseLabel=label_ExternalDataInit, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    call ESMF_GridCompInitialize(self%esmComp, phase=phase, &
         importState=self%fromJedi, exportState=self%toJedi, &
         clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

#if 0
    call NUOPC_Write(self%toJedi, &
         fileNamePrefix="diagnostic_postdatainit_toJedi_", &
         overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
#endif

    call ESMF_LogWrite("Exit "//subname, ESMF_LOGMSG_INFO)
    
  end subroutine initialize

! --------------------------------------------------------------------------------------------------

  subroutine step(self, state, vdate_start, vdate_final)

    implicit none
    
    class(model_ufs),    intent(inout) :: self
    type(fv3jedi_state), intent(inout) :: state
    type(datetime),      intent(in)    :: vdate_start
    type(datetime),      intent(in)    :: vdate_final
        
    ! local variables
    integer :: rc, urc
    character(len=20) :: strStartTime, strStopTime
    type(ESMF_Time) :: startTime, stopTime
    type(ESMF_TimeInterval) :: timeStep
    !integer, save     :: tstep=1
    !character(len=80) :: fileName
    

!-----------------------------------------------------------------------------

    character(len=*),parameter :: subname = modname//' (step)'
    call ESMF_LogWrite("Enter "//subname, ESMF_LOGMSG_INFO)

    call datetime_to_string(vdate_start, strStartTime)
    call datetime_to_string(vdate_final, strStopTime)
    
    call ESMF_LogWrite(" --> REQUESTED START TIME:"//trim(strStartTime), ESMF_LOGMSG_INFO)
    call ESMF_LogWrite(" --> REQUESTED STOP  TIME:"//trim(strStopTime), ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(self%clock, startTime=startTime, & 
         stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    !call SetTimeFromString(strStartTime, startTime, rc=rc)
    call ESMF_TimeSet(startTime, timeString=strStartTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

!    call SetTimeFromString(strStopTime, stopTime, rc=rc)
    call ESMF_TimeSet(stopTime, timeString=strStopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    timeStep = stopTime - startTime

    call ESMF_ClockSet(self%clock, startTime=startTime, &
         stopTime=stopTime, currTime=startTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
  
    ! step the model forward
    call ESMF_GridCompRun(self%esmComp, &
         importState=self%fromJedi, exportState=self%toJedi, &
         clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif

    call ESMF_LogWrite("Exit "//subname, ESMF_LOGMSG_INFO)
  end subroutine step
  
  ! --------------------------------------------------------------------------------------------------
  
  subroutine finalize(self, state, vdate)
    
    implicit none
    class(model_ufs),    intent(inout) :: self
    type(fv3jedi_state), intent(in)    :: state
    type(datetime),intent(in)    :: vdate
    
    integer :: rc, urc
    character(len=*),parameter :: subname = modname//' (finalize)'

    call ESMF_LogWrite("Enter "//subname, ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(gridcomp=self%esmComp, &
         importstate=self%fromJedi, & 
         exportstate=self%toJedi, & 
         clock=self%clock, userrc=urc, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) then
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    endif
    
    call ESMF_LogWrite("Exit "//subname, ESMF_LOGMSG_INFO)

  end subroutine finalize
  
  ! subroutine SetTimeFromString(cdate, time, rc)

  !   implicit none
  !   character(len=20),  intent(in)  :: cdate
  !   type(ESMF_Time),    intent(out) :: time
  !   integer,            intent(out) :: rc
    
  !   integer :: yy,mm,dd,hh,mn
    
  !   !Convert character dates to integers
  !   read(cdate(1:4),'(i4)') yy
  !   read(cdate(6:7),'(i2)') mm
  !   read(cdate(9:10),'(i2)') dd
  !   read(cdate(12:13),'(i2)') hh
  !   read(cdate(15:16),'(i2)') mn
    
  !   call ESMF_TimeSet(time, yy=yy, mm=mm, & 
  !        dd=dd, h=hh, m=mn, calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
  !   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !        line=__LINE__, file=__FILE__)) &
  !        return
    
  ! end subroutine SetTimeFromString
  
end module fv3jedi_ufs_mod
