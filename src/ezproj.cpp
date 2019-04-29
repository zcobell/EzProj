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
#include <algorithm>
#include <cassert>
#include <cmath>
#include <limits>
#include "proj.h"

constexpr double s_pi = 3.14159265358979323846;
constexpr double s_torad = s_pi / 180.0;
constexpr double s_todeg = 180.0 / s_pi;

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
int Ezproj::transform(int inputEPSG, int outputEPSG, Point &input,
                      Point &output, bool &isLatLon) {
  std::vector<Point> in, out;
  in.push_back(input);
  int ierr = this->transform(inputEPSG, outputEPSG, in, out, isLatLon);
  if (ierr != Ezproj::NoError) return ierr;
  output = out.at(0);
  return ierr;
}

int Ezproj::transform(int inputEPSG, int outputEPSG, double x, double y,
                      double &outx, double &outy, bool &isLatLon) {
  Point in(x, y), out;
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
int Ezproj::transform(int inputEPSG, int outputEPSG, std::vector<Point> &input,
                      std::vector<Point> &output, bool &isLatLon) {
  assert(input.size() > 0);
  if (input.size() <= 0) return Ezproj::NoData;

  if (!this->containsEpsg(inputEPSG)) {
    return NoSuchProjection;
  }

  if (!this->containsEpsg(outputEPSG)) {
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
      c.lp.lam = input[i].first * s_torad;
      c.lp.phi = input[i].second * s_torad;
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
  return std::find(this->m_epsgMapping->begin(), this->m_epsgMapping->end(),
                   epsg) == this->m_epsgMapping->end()
             ? false
             : true;
}

int Ezproj::cpp(double lambda0, double phi0, double x, double y, double &outx,
                double &outy) {
  Point i(x, y), o;
  int ierr = Ezproj::cpp(lambda0, phi0, i, o);
  if (ierr != Ezproj::NoError) return ierr;
  outx = o.first;
  outy = o.second;
  return Ezproj::NoError;
}

int Ezproj::cpp(double lambda0, double phi0, Point &input, Point &output) {
  std::vector<Point> in, out;
  in.push_back(input);
  int ierr = Ezproj::cpp(lambda0, phi0, in, out);
  if (ierr == Ezproj::NoError) output = out.at(0);
  return ierr;
}

int Ezproj::cpp(double lambda0, double phi0, std::vector<Point> &input,
                std::vector<Point> &output) {
  assert(input.size() > 0);
  if (input.size() <= 0) return Ezproj::NoData;

  double slam0 = lambda0 * s_torad;
  double sfea0 = phi0 * s_torad;
  double r = Ezproj::radiusEarth(phi0);
  output.reserve(input.size());
  for (auto &p : input) {
    double x = r * (s_torad * p.first - slam0) * cos(sfea0);
    double y = r * (s_torad * p.second);
    output.push_back(Point(x, y));
  }
  return Ezproj::NoError;
}

int Ezproj::inverseCpp(double lambda0, double phi0, double x, double y,
                       double &outx, double &outy) {
  Point i(x, y), o;
  int ierr = Ezproj::inverseCpp(lambda0, phi0, i, o);
  if (ierr != Ezproj::NoError) return ierr;
  outx = o.first;
  outy = o.second;
  return Ezproj::NoError;
}

int Ezproj::inverseCpp(double lambda0, double phi0, Point &input,
                       Point &output) {
  std::vector<Point> in, out;
  in.push_back(input);
  int ierr = Ezproj::inverseCpp(lambda0, phi0, in, out);
  if (ierr == Ezproj::NoError) output = out.at(0);
  return ierr;
}

int Ezproj::inverseCpp(double lambda0, double phi0, std::vector<Point> &input,
                       std::vector<Point> &output) {
  assert(input.size() > 0);
  if (input.size() <= 0) return Ezproj::NoData;

  double slam0 = s_torad * lambda0;
  double sfea0 = s_torad * phi0;
  double r = Ezproj::radiusEarth(phi0);
  output.reserve(input.size());
  for (auto &p : input) {
    double x = s_todeg * (slam0 + p.first / (r * cos(sfea0)));
    double y = s_todeg * (p.second / r);
    output.push_back(Point(x, y));
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
  return std::to_string(static_cast<unsigned long long>(PROJ_VERSION_MAJOR)) + "." +
         std::to_string(static_cast<unsigned long long>(PROJ_VERSION_MINOR)) + "." +
         std::to_string(static_cast<unsigned long long>(PROJ_VERSION_PATCH));
}

size_t Ezproj::position(int epsg) {
  auto it =
      std::find(this->m_epsgMapping->begin(), this->m_epsgMapping->end(), epsg);
  if (it == this->m_epsgMapping->end()) {
    return std::numeric_limits<size_t>::max();
  } else {
    return static_cast<size_t>(it - this->m_epsgMapping->begin());
  }
}

double Ezproj::radiusEarth(double latitude) {
  double l = s_torad * latitude;
  return sqrt((pow(Ezproj::equitoralRadius(), 4.0) * cos(l) * cos(l) +
               pow(Ezproj::polarRadius(), 4.0) * sin(l) * sin(l)) /
              (pow(Ezproj::equitoralRadius(), 2.0) * cos(l) * cos(l) +
               pow(Ezproj::polarRadius(), 2.0) * sin(l) * sin(l)));
}
