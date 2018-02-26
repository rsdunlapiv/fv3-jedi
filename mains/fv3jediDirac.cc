/*
 * (C) Copyright 2009-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "FV3JEDITraits.h"
#include "oops/runs/Dirac.h"
#include "RunFV3JEDI.h"

int main(int argc,  char ** argv) {
  fv3jedi::RunFV3JEDI run(argc, argv);
  oops::Dirac<fv3jedi::FV3JEDITraits> dir;
  run.execute(dir);
  return 0;
};