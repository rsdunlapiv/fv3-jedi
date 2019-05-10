/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "RunFV3JEDI.h"
#include "instantiateObsFilterFactory.h"
#include "oops/runs/HofX3D.h"
#include "FV3JEDITraits.h"

int main(int argc,  char ** argv) {
  fv3jedi::RunFV3JEDI run(argc, argv);
  fv3jedi::instantiateObsFilterFactory();
  oops::HofX3D<fv3jedi::FV3JEDITraits> hofx;
  run.execute(hofx);
  return 0;
}