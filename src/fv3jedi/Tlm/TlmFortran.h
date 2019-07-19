/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3JEDI_TLM_TLMFORTRAN_H_
#define FV3JEDI_TLM_TLMFORTRAN_H_

#include "fv3jedi/Utilities/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
  class Duration;
}

namespace fv3jedi {

extern "C" {

  void fv3jedi_tlm_create_f90(const eckit::Configuration * const *,
                              const F90geom &,
                              F90tlm &,
                              const eckit::Configuration * const *);
  void fv3jedi_tlm_delete_f90(F90tlm &);


  void fv3jedi_tlm_initialize_tl_f90(const F90geom &,
                                     const F90tlm &,
                                     const F90inc &);
  void fv3jedi_tlm_initialize_ad_f90(const F90geom &,
                                     const F90tlm &,
                                     const F90inc &);

  void fv3jedi_tlm_step_tl_f90(const F90geom &,
                               const F90tlm &,
                               const F90inc &,
                               const F90traj &);
  void fv3jedi_tlm_step_ad_f90(const F90geom &,
                               const F90tlm &,
                               const F90inc &,
                               const F90traj &);

  void fv3jedi_tlm_finalize_tl_f90(const F90geom &,
                                   const F90tlm &,
                                   const F90inc &);
  void fv3jedi_tlm_finalize_ad_f90(const F90geom &,
                                   const F90tlm &,
                                   const F90inc &);

  void fv3jedi_traj_wipe_f90(F90traj &);

}  // extern "C"
// -----------------------------------------------------------------------------

}  // namespace fv3jedi
#endif  // FV3JEDI_TLM_TLMFORTRAN_H_