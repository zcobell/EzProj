//------------------------------GPL---------------------------------------//
// This file is part of EZPROJ.
//
// (c) 2015-2018 Zachary Cobell
//
// EZPROJ is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// EZPROJ is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with EZPROJ.  If not, see <http://www.gnu.org/licenses/>.
//------------------------------------------------------------------------//
#include "ezproj.h"
#include <cassert>
#include <cmath>
#include <limits>
#include "proj.h"

/** \brief Constructor for the proj4 wrapper class
 *
 * Constructs an object used to convert coordinates. The initialization function
 *is run at startup which parses the included EPSG file (:/rsc/epsg) to
 *determine the parameters to pass to the Proj4 API
 *
 **/
Ezproj::Ezproj() { this->_initialize(); }

/** \brief Function to execute a coordinate system transformation using Proj4
 *
 * @param[in]  inputEPSG  EPSG that coordinates are currently in
 * @param[in]  outputEPSG EPSG that the coordinates will be converted to
 * @param[in]  input      std::pair<double,double> object containing the
 *location to be converted
 * @param[out] output     std::pair<double,double> object containing the
 *converted coordiantes
 * @param[out] isLatLon   Bool that determine if the coordinates are lat/lon or
 *otherwise
 *
 * Function to execute a coordinate system transformation using Proj4
 *
 **/
int Ezproj::transform(int inputEPSG, int outputEPSG,
                      std::pair<double, double> &input,
                      std::pair<double, double> &output, bool &isLatLon) {
  std::vector<std::pair<double, double>> in, out;
  in.push_back(input);
  int ierr = this->transform(inputEPSG, outputEPSG, in, out, isLatLon);
  if (ierr != Ezproj::NoError) return ierr;
  output = out.at(0);
  return ierr;
}

int Ezproj::transform(int inputEPSG, int outputEPSG, double x, double y,
                      double &outx, double &outy, bool &isLatLon) {
  std::pair<double, double> in(x, y), out;
  int ierr = this->transform(inputEPSG, outputEPSG, in, out, isLatLon);
  if (ierr != Ezproj::NoError) return ierr;
  outx = out.first;
  outy = out.second;
  return ierr;
}

/** \brief Function to execute a coordinate system transformation using Proj4
 *
 * @param[in]  inputEPSG  EPSG that coordinates are currently in
 * @param[in]  outputEPSG EPSG that the coordinates will be converted to
 * @param[in]  input      vector of pairs containing the locations
 *                        to be converted
 * @param[out] output     vector of pairs containing the converted
 *                        coordiantes
 * @param[out] isLatLon   Bool that determine if the coordinates are lat/lon or
 *                        otherwise
 *
 * Function to execute a coordinate system transformation using Proj4
 *
 **/
int Ezproj::transform(int inputEPSG, int outputEPSG,
                      std::vector<std::pair<double, double>> &input,
                      std::vector<std::pair<double, double>> &output,
                      bool &isLatLon) {
  assert(input.size() > 0);
  if (input.size() <= 0) return Ezproj::NoData;

  if (this->m_epsgMapping->find(inputEPSG) == this->m_epsgMapping->end()) {
    return NoSuchProjection;
  }

  if (this->m_epsgMapping->find(outputEPSG) == this->m_epsgMapping->end()) {
    return NoSuchProjection;
  }

  std::string p1 = this->projInitializationString(inputEPSG);
  std::string p2 = this->projInitializationString(outputEPSG);

  PJ *pj1 = proj_create(PJ_DEFAULT_CTX, p1.c_str());
  if (pj1 == nullptr) {
    return Proj4InternalError;
  }

  PJ *pj2 = proj_create(PJ_DEFAULT_CTX, p2.c_str());
  if (pj2 == nullptr) {
    proj_destroy(pj1);
    return Proj4InternalError;
  }

  output.resize(input.size());

  for (size_t i = 0; i < input.size(); ++i) {
    PJ_COORD c, o;

    if (proj_angular_input(pj1, PJ_INV)) {
      c.lp.lam = proj_torad(input[i].first);
      c.lp.phi = proj_torad(input[i].second);
    } else {
      c.xy.x = input[i].first;
      c.xy.y = input[i].second;
    }

    o = proj_trans(pj2, PJ_FWD, proj_trans(pj1, PJ_INV, c));

    if (proj_angular_output(pj2, PJ_FWD)) {
      output[i].first = proj_todeg(o.lp.lam);
      output[i].second = proj_todeg(o.lp.phi);
      isLatLon = true;
    } else {
      output[i].first = o.xy.x;
      output[i].second = o.xy.y;
      isLatLon = false;
    }
  }

  proj_destroy(pj1);
  proj_destroy(pj2);
  return NoError;
}

bool Ezproj::containsEpsg(int epsg) {
  return this->m_epsgMapping->find(epsg) == this->m_epsgMapping->end() ? false
                                                                       : true;
}

int Ezproj::cpp(double lambda0, double phi0, double x, double y, double &outx,
                double &outy) {
  std::pair<double, double> i(x, y), o;
  int ierr = Ezproj::cpp(lambda0, phi0, i, o);
  if (ierr != Ezproj::NoError) return ierr;
  outx = o.first;
  outy = o.second;
  return Ezproj::NoError;
}

int Ezproj::cpp(double lambda0, double phi0, std::pair<double, double> &input,
                std::pair<double, double> &output) {
  std::vector<std::pair<double, double>> in, out;
  in.push_back(input);
  int ierr = Ezproj::cpp(lambda0, phi0, in, out);
  if (ierr == Ezproj::NoError) output = out.at(0);
  return ierr;
}

int Ezproj::cpp(double lambda0, double phi0,
                std::vector<std::pair<double, double>> &input,
                std::vector<std::pair<double, double>> &output) {
  assert(input.size() > 0);
  if (input.size() <= 0) return Ezproj::NoData;

  double slam0 = proj_torad(lambda0);
  double sfea0 = proj_torad(phi0);
  double r = Ezproj::radiusEarth(phi0);
  output.reserve(input.size());
  for (auto &p : input) {
    double x = r * proj_torad(p.first - slam0) * cos(sfea0);
    double y = r * proj_torad(p.second);
    output.push_back(std::pair<double, double>(x, y));
  }
  return Ezproj::NoError;
}

int Ezproj::inverseCpp(double lambda0, double phi0, double x, double y,
                       double &outx, double &outy) {
  std::pair<double, double> i(x, y), o;
  int ierr = Ezproj::inverseCpp(lambda0, phi0, i, o);
  if (ierr != Ezproj::NoError) return ierr;
  outx = o.first;
  outy = o.second;
  return Ezproj::NoError;
}

int Ezproj::inverseCpp(double lambda0, double phi0,
                       std::pair<double, double> &input,
                       std::pair<double, double> &output) {
  std::vector<std::pair<double, double>> in, out;
  in.push_back(input);
  int ierr = Ezproj::inverseCpp(lambda0, phi0, in, out);
  if (ierr == Ezproj::NoError) output = out.at(0);
  return ierr;
}

int Ezproj::inverseCpp(double lambda0, double phi0,
                       std::vector<std::pair<double, double>> &input,
                       std::vector<std::pair<double, double>> &output) {
  assert(input.size() > 0);
  if (input.size() <= 0) return Ezproj::NoData;

  double slam0 = proj_torad(lambda0);
  double sfea0 = proj_torad(phi0);
  double r = Ezproj::radiusEarth(phi0);
  output.reserve(input.size());
  for (auto &p : input) {
    double x = proj_todeg(slam0 + p.first / (r * cos(sfea0)));
    double y = proj_todeg(p.second / r);
    output.push_back(std::pair<double, double>(x, y));
  }
  return Ezproj::NoError;
}

std::string Ezproj::projInitializationString(int epsg) {
  size_t i = this->position(epsg);
  if (i == std::numeric_limits<size_t>::max()) {
    return "No EPSG found";
  } else {
    return std::string(this->m_epsgInit->at(i));
  }
}

std::string Ezproj::description(int epsg) {
  size_t i = this->position(epsg);
  if (i == std::numeric_limits<size_t>::max()) {
    return "No EPSG found";
  } else {
    return std::string(this->m_epsgDescriptions->at(i));
  }
}

std::string Ezproj::projVersion() {
  return std::to_string(PROJ_VERSION_MAJOR) + "." +
         std::to_string(PROJ_VERSION_MINOR) + "." +
         std::to_string(PROJ_VERSION_PATCH);
}

size_t Ezproj::position(int epsg) {
  auto it = this->m_epsgMapping->find(epsg);
  if (it == this->m_epsgMapping->end()) {
    return std::numeric_limits<size_t>::max();
  } else {
    return this->m_epsgMapping->at(epsg);
  }
}

double Ezproj::radiusEarth(double latitude) {
  double l = proj_torad(latitude);
  return sqrt((pow(Ezproj::equitoralRadius(), 4.0) * cos(l) * cos(l) +
               pow(Ezproj::polarRadius(), 4.0) * sin(l) * sin(l)) /
              (pow(Ezproj::equitoralRadius(), 2.0) * cos(l) * cos(l) +
               pow(Ezproj::polarRadius(), 2.0) * sin(l) * sin(l)));
}
