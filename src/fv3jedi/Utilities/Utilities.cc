/*
 * (C) Copyright 2018 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <mpi.h>
#include <unistd.h>

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "oops/mpi/mpi.h"
#include "oops/util/abor1_cpp.h"
#include "oops/util/Logger.h"

#include "fv3jedi/Utilities/Utilities.h"

namespace eckit {
  class Configuration;
}

namespace fv3jedi {

// -----------------------------------------------------------------------------
void stageFMSFiles(const eckit::Configuration & conf, const eckit::mpi::Comm & comm) {
  // Get processor ID
  int world_rank = oops::mpi::world().rank();

  if (world_rank == 0) {
    // User provided input files for this geom/state/model etc
    delete_file("input.nml");
    if (conf.has("nml_file_mpp")) {
      oops::Log::debug() << "Staging input.nml" << std::endl;
      std::string nml_file = conf.getString("nml_file_mpp");
      if (symlink(nml_file.c_str(), "./input.nml"))
        ABORT("Unable to symlink input.nml");
    } else {
      ABORT("nml_file_mpp not in configuration");
    }
  }
  // Nobody moves until files are in place
  oops::mpi::world().barrier();
}
// -----------------------------------------------------------------------------

void stageFv3Files(const eckit::Configuration &conf, const eckit::mpi::Comm & comm) {
  // Get processor ID
  int world_rank = oops::mpi::world().rank();

  // Only one processor needs to move the files
  // When we use several backgrounds, this lines will have to change
  if (world_rank == 0) {
    // User provided input files for this geom/state/model etc
    delete_file("input.nml");
    if (conf.has("nml_file")) {
      oops::Log::debug() << "Staging input.nml" << std::endl;
      std::string nml_file = conf.getString("nml_file");

      // Create empty file
      std::ifstream startNml(nml_file);
      std::ofstream finalNml("./input.nml");

      std::string startStr("layout = $LAYOUTX,$LAYOUTY");
      std::string finalStr("layout = 1,1");

      std::string line;
      std::size_t len = startStr.length();
      while (getline(startNml, line))
      {
          while (true)
          {
              size_t pos = line.find(startStr);
              if (pos != std::string::npos)
                  line.replace(pos, len, finalStr);
              else
                  break;
          }

          finalNml << line << '\n';
      }

      finalNml.close();

      //  if (symlink(nml_file.c_str(), "./input.nml"))
      //  ABORT("Unable to symlink input.nml");
    }

    // User may also be requesting the field_table to be staged
    delete_file("field_table");
    if (conf.has("trc_file")) {
      oops::Log::debug() << "Staging field_table" << std::endl;
      std::string trc_file = conf.getString("trc_file");
      if (symlink(trc_file.c_str(), "./field_table"))
        ABORT("Unable to symlink field_table");
    }

    // User may also be requesting the tlm/adm nml file
    delete_file("inputpert.nml");
    if (conf.has("nml_file_pert")) {
      oops::Log::debug() << "Staging inputpert.nml" << std::endl;
      std::string nml_file_pert = conf.getString("nml_file_pert");
      if (symlink(nml_file_pert.c_str(), "./inputpert.nml"))
        ABORT("Unable to symlink inputpert.nml");
    }

    // User may also want the INPUT directory linked (regional grid for example)
    delete_file("INPUT");
    if (conf.has("fv3_input_dir")) {
      oops::Log::debug() << "Staging INPUT directory" << std::endl;
      std::string fv3_input_dir = conf.getString("fv3_input_dir");
      oops::Log::debug() << "INPUT/ --> " << fv3_input_dir << std::endl;
      if (symlink(fv3_input_dir.c_str(), "./INPUT"))
        ABORT("Unable to symlink INPUT");
    }
  }

  // Nobody moves until files are in place
  oops::mpi::world().barrier();
}

// -----------------------------------------------------------------------------

void removeFv3Files(const eckit::mpi::Comm & comm) {
  // Get processor ID
  int world_rank = oops::mpi::world().rank();

  // No file deletion until everyone catches up
  oops::mpi::world().barrier();

  // Only one processor needs to move the files
  if (world_rank == 0) {
    delete_file("input.nml");
    delete_file("field_table");
    delete_file("inputpert.nml");
  }
  // Nobody moves until files are deleted
  oops::mpi::world().barrier();
}

// -----------------------------------------------------------------------------

void delete_file(const char *fileName)
{
  std::ifstream infile(fileName);
  if (infile.good()) {
    oops::Log::debug() << "Removing: " << fileName << std::endl;
    std::remove(fileName);
  }
}

// -------------------------------------------------------------------------------------------------

void generateGeomFv3Conf(const eckit::Configuration & conf, const eckit::mpi::Comm & comm) {
  // Trace
  // -----
  oops::Log::trace() << "generateGeomFv3Conf starting" << std::endl;

  // Barrier
  // -------
  oops::mpi::world().barrier();

  // Only root
  // ---------
  if (oops::mpi::world().rank() == 0) {
    // Delete the file if it exists
    // ----------------------------
    delete_file("input.nml");

    // Parse config
    // ------------
    std::vector<std::string> layout = conf.getStringVector("layout", {"1", "1"});
    std::vector<std::string> io_layout = conf.getStringVector("io_layout", {"1", "1"});
    std::string npx = conf.getString("npx");
    std::string npy = conf.getString("npy", npx);
    std::string npz = conf.getString("npz");
    std::string ntiles = conf.getString("ntiles", "6");

    // Vector of config
    // ----------------
    std::vector<std::string> inputnml;

    inputnml.push_back("&fv_core_nml\n");
    inputnml.push_back("\tlayout = "+layout[0]+","+layout[1]+"\n");
    inputnml.push_back("\tio_layout = "+io_layout[0]+","+io_layout[1]+"\n");
    inputnml.push_back("\tnpx = "+npx+"\n");
    inputnml.push_back("\tnpy = "+npy+"\n");
    inputnml.push_back("\tnpz = "+npz+"\n");
    inputnml.push_back("\tntiles = "+ntiles+"\n");

    // Parse config for optional fields
    // --------------------------------
    if (conf.has("regional")) {
      bool regional = conf.getBool("regional");
      if (regional) {
        inputnml.push_back("\tregional = .T.\n");
      }
    }

    if (conf.has("nested")) {
      bool nested = conf.getBool("nested");
      if (nested) {
        inputnml.push_back("\tnested = .T.\n");
      }
    }

    if (conf.has("do_schmidt")) {
      bool do_schmidt = conf.getBool("do_schmidt");
      if (do_schmidt) {
        inputnml.push_back("\tdo_schmidt = .T.\n");
      }
    }

    if (conf.has("target_lat")) {
      std::string target_lat = conf.getString("target_lat");
      inputnml.push_back("\ttarget_lat = "+target_lat+"\n");
    }

    if (conf.has("target_lon")) {
      std::string target_lon = conf.getString("target_lon");
      inputnml.push_back("\ttarget_lon = "+target_lon+"\n");
    }

    if (conf.has("stretch_fac")) {
      std::string stretch_fac = conf.getString("stretch_fac");
      inputnml.push_back("\tstretch_fac = "+stretch_fac+"\n");
    }

    if (conf.has("hydrostatic")) {
      bool hydrostatic = conf.getBool("hydrostatic");
      if (!hydrostatic) {
        inputnml.push_back("\thydrostatic = .false.\n");
      }
    }

    if (conf.has("nwat")) {
      std::string nwat = conf.getString("nwat");
      inputnml.push_back("\tnwat = "+nwat+"\n");
    }

    // End of section marker
    // ---------------------
    inputnml.push_back("/");

    // Write the config to the input.nml file
    // --------------------------------------
    std::string file = "input.nml";
    std::ofstream out(file);
    for (int km = 0; km < static_cast<int>(inputnml.size()); ++km) {
      out << inputnml[km];
    }
    out.close();
  }

  // Barrier
  // -------
  oops::mpi::world().barrier();

  // Trace
  // -----
  oops::Log::trace() << "generateGeomFv3Conf done" << std::endl;
}

// -----------------------------------------------------------------------------

}  // namespace fv3jedi
