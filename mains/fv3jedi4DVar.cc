/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "FV3JEDITraits.h"
#include "oops/runs/Variational.h"
#include "RunFV3JEDI.h"

int main(int argc,  char ** argv) {
  lfirc::RunFV3JEDI run(argc, argv);
  oops::Variational<fv3jedi::FV3JEDITraits> var;
  run.execute(var);
  return 0;
};
