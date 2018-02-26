/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "StateFV3JEDI.h"

#include <algorithm>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "oops/base/Variables.h"
#include "oops/generic/UnstructuredGrid.h"
#include "util/Logger.h"
#include "ufo/GeoVaLs.h"
#include "ufo/Locations.h"
#include "ModelBiasFV3JEDI.h"
#include "FieldsFV3JEDI.h"
#include "GeometryFV3JEDI.h"
#include "IncrementFV3JEDI.h"
#include "ModelFV3JEDI.h"
#include "util/DateTime.h"
#include "util/Duration.h"

namespace fv3jedi {

// -----------------------------------------------------------------------------
/// Constructor, destructor
// -----------------------------------------------------------------------------
StateFV3JEDI::StateFV3JEDI(const GeometryFV3JEDI & resol, const oops::Variables & vars,
                         const util::DateTime & vt)
  : fields_(new FieldsFV3JEDI(resol, vars, vt)), stash_()
{
  oops::Log::trace() << "StateFV3JEDI::StateFV3JEDI created." << std::endl;
}
// -----------------------------------------------------------------------------
StateFV3JEDI::StateFV3JEDI(const GeometryFV3JEDI & resol, const eckit::Configuration & file)
  : fields_(), stash_()
{
// Should get variables from file. YT
  const std::vector<std::string> vv{"cv"};
  oops::Variables vars(vv);
  fields_.reset(new FieldsFV3JEDI(resol, vars, util::DateTime()));
  fields_->read(file);

  ASSERT(fields_);
  oops::Log::trace() << "StateFV3JEDI::StateFV3JEDI created and read in." << std::endl;
}
// -----------------------------------------------------------------------------
StateFV3JEDI::StateFV3JEDI(const GeometryFV3JEDI & resol, const StateFV3JEDI & other)
  : fields_(new FieldsFV3JEDI(*other.fields_, resol)), stash_()
{
  ASSERT(fields_);
  oops::Log::trace() << "StateFV3JEDI::StateFV3JEDI created by interpolation." << std::endl;
}
// -----------------------------------------------------------------------------
StateFV3JEDI::StateFV3JEDI(const StateFV3JEDI & other)
  : fields_(new FieldsFV3JEDI(*other.fields_)), stash_()
{
  ASSERT(fields_);
  oops::Log::trace() << "StateFV3JEDI::StateFV3JEDI copied." << std::endl;
}
// -----------------------------------------------------------------------------
StateFV3JEDI::~StateFV3JEDI() {
  oops::Log::trace() << "StateFV3JEDI::StateFV3JEDI destructed." << std::endl;
}
// -----------------------------------------------------------------------------
/// Basic operators
// -----------------------------------------------------------------------------
StateFV3JEDI & StateFV3JEDI::operator=(const StateFV3JEDI & rhs) {
  ASSERT(fields_);
  *fields_ = *rhs.fields_;
  return *this;
}
// -----------------------------------------------------------------------------
/// Interpolate to observation location
// -----------------------------------------------------------------------------
void StateFV3JEDI::interpolate(const ufo::Locations & locs, const oops::Variables & vars, ufo::GeoVaLs & cols) const {
  fields_->interpolate(locs, vars, cols);
}
// -----------------------------------------------------------------------------
/// Interpolate full fields
// -----------------------------------------------------------------------------
void StateFV3JEDI::changeResolution(const StateFV3JEDI & other) {
  fields_->changeResolution(*other.fields_);
  oops::Log::trace() << "StateFV3JEDI interpolated" << std::endl;
}
// -----------------------------------------------------------------------------
/// Interactions with Increments
// -----------------------------------------------------------------------------
StateFV3JEDI & StateFV3JEDI::operator+=(const IncrementFV3JEDI & dx) {
  ASSERT(this->validTime() == dx.validTime());
  ASSERT(fields_);
  fields_->add(dx.fields());
  return *this;
}
// -----------------------------------------------------------------------------
/// Convert to/from unstructured grid
// -----------------------------------------------------------------------------
void StateFV3JEDI::convert_to(oops::UnstructuredGrid & ug) const {
  fields_->convert_to(ug);
}
// -----------------------------------------------------------------------------
void StateFV3JEDI::convert_from(const oops::UnstructuredGrid & ug) {
  fields_->convert_from(ug);
}
// -----------------------------------------------------------------------------
/// I/O and diagnostics
// -----------------------------------------------------------------------------
void StateFV3JEDI::read(const eckit::Configuration & files) {
  fields_->read(files);
}
// -----------------------------------------------------------------------------
void StateFV3JEDI::write(const eckit::Configuration & files) const {
  fields_->write(files);
}
// -----------------------------------------------------------------------------
void StateFV3JEDI::print(std::ostream & os) const {
  os << std::endl << "  Valid time: " << validTime();
  os << *fields_;
}
// -----------------------------------------------------------------------------
/// For accumulator
// -----------------------------------------------------------------------------
void StateFV3JEDI::zero() {
  fields_->zero();
}
// -----------------------------------------------------------------------------
void StateFV3JEDI::accumul(const double & zz, const StateFV3JEDI & xx) {
  fields_->axpy(zz, *xx.fields_);
}
// -----------------------------------------------------------------------------

}  // namespace fv3jedi