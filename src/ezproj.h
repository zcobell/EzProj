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
/**
 * \class Ezproj
 *
 * \brief Class that serves as an interface to the standard C PROJ library
 *
 * This function serves as the C++ interface to the C library PROJ (which is
 * compiled into this code)
 *
 * \author Zach Cobell
 *
 * Contact: zcobell@gmail.com
 *
 */
#ifndef EZPROJ_H
#define EZPROJ_H

#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>
#include "point.h"

class Ezproj {
 public:
  enum _errors { NoError, NoSuchProjection, NoData, Proj4InternalError };

  explicit Ezproj();

  std::string projVersion();

  int transform(int inputEPSG, int outputEPSG, double x, double y, double &outx,
                double &outy, bool &isLatLon);

  int transform(int inputEPSG, int outputEPSG, Point &input, Point &output,
                bool &isLatLon);

  int transform(int inputEPSG, int outputEPSG, std::vector<Point> &input,
                std::vector<Point> &output, bool &isLatLon);

  std::string description(int epsg);
  std::string projInitializationString(int epsg);

  bool containsEpsg(int epsg);

  static int cpp(double lambda0, double phi0, double x, double y, double &outx,
                 double &outy);
  static int cpp(double lambda0, double phi0, Point &input, Point &output);
  static int cpp(double lambda0, double phi0, std::vector<Point> &input,
                 std::vector<Point> &output);

  static int inverseCpp(double lambda0, double phi0, double x, double y,
                        double &outx, double &outy);
  static int inverseCpp(double lambda0, double phi0, Point &input,
                        Point &output);
  static int inverseCpp(double lambda0, double phi0, std::vector<Point> &input,
                        std::vector<Point> &output);

 private:
  void _initialize();

  size_t position(int epsg);

  static double deg2rad();
  static double rad2deg();
  static double toRadians(double degrees);
  static double toDegrees(double radians);
  static double radiusEarth(double latitude);
  static double radiusEarth();
  static double equitoralRadius();
  static double polarRadius();

  const std::vector<const char *> *m_epsgDescriptions;
  const std::vector<const char *> *m_epsgInit;
  const std::unordered_map<int, size_t> *m_epsgMapping;
};

#endif  // EZPROJ_H
