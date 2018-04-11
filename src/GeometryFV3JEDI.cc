/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "GeometryFV3JEDI.h"
#include "util/Logger.h"
#include "Fortran.h"
#include "eckit/config/Configuration.h"

// -----------------------------------------------------------------------------
namespace fv3jedi {
// -----------------------------------------------------------------------------
GeometryFV3JEDI::GeometryFV3JEDI(const eckit::Configuration & conf) {
  const eckit::Configuration * configc = &conf;

  std::remove("input.nml");
  std::remove("field_table");
  nml_file = conf.getString("nml_file");
  trc_file = conf.getString("trc_file");
  std::cout << "dh: geom 1" << nml_file.c_str() << std::endl;
  std::cout << "dh: geom 2" << trc_file.c_str() << std::endl;
  symlink(nml_file.c_str(), "./input.nml");
  symlink(trc_file.c_str(), "./field_table");

  fv3jedi_geo_setup_f90(keyGeom_, &configc);

  std::remove("input.nml");
  std::remove("field_table");
}
// -----------------------------------------------------------------------------
GeometryFV3JEDI::GeometryFV3JEDI(const GeometryFV3JEDI & other) {
  const int key_geo = other.keyGeom_;
  fv3jedi_geo_clone_f90(key_geo, keyGeom_);
}
// -----------------------------------------------------------------------------
GeometryFV3JEDI::~GeometryFV3JEDI() {
  fv3jedi_geo_delete_f90(keyGeom_);
}
// -----------------------------------------------------------------------------
void GeometryFV3JEDI::print(std::ostream & os) const {
  fv3jedi_geo_info_f90(keyGeom_);
  os << "fv3jedi::GeometryFV3JEDI::print not implemented yet";
}
// -----------------------------------------------------------------------------
}  // namespace fv3jedi
