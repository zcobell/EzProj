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
#include <iostream>
#include <cmath>
#include <vector>

int main() {

    Ezproj p;
    std::cout << "INFO: Using proj version: " << p.projVersion() << std::endl;


    //...Project a single point
    double x_original = 753922.922116;
    double y_original = 3328065.712818;
    const double x_expected = -90.3661965220;
    const double y_expected = 30.0573591369;
    const double tol = 0.000001;
    const int cs1 = 26915;
    const int cs2 = 4326;
    double x_utm15,y_utm15;
    bool isLatLon;

    p.transform(cs1,cs2,x_original,y_original,x_utm15,y_utm15,isLatLon);

    char buf[50];
    sprintf(buf, "Original X coordinate: %f", x_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected X coordinate: %f", x_utm15);
    std::cout << buf << std::endl;
    sprintf(buf, "Original Y coordinate: %f", y_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected Y coordinate: %f", y_utm15);
    std::cout << buf << std::endl;

    std::cout << "Projecting a single x, y" << std::endl;
    if (std::abs(x_utm15 - x_expected) > tol ||
        std::abs(y_utm15 - y_expected) > tol) {
      std::cout << "Expected: " << x_expected << " " << y_expected << std::endl;
      printf("Got: %14.6f, %14.6f\n", x_utm15, y_utm15);
      return 1;
    }

    //...Project a pair (Point)
    std::cout << "Projecting a pair" << std::endl;
    Point pt(x_original,y_original);
    Point pt_out;
    p.transform(cs1,cs2,pt,pt_out,isLatLon);
    if (std::abs(pt_out.first - x_expected) > tol ||
        std::abs(pt_out.second - y_expected) > tol) {
      std::cout << "Expected: " << x_expected << " " << y_expected << std::endl;
      printf("Got: %14.6f, %14.6f\n", pt_out.first, pt_out.second);
      return 1;
    }

    //...Project a vector of pairs (Point)
    std::cout << "Projecting a vector of Points" << std::endl;
    std::vector<Point> vec_original;
    std::vector<Point> vec_utm15;
    vec_original.push_back(std::pair<double,double>(x_original,y_original));
    vec_original.push_back(std::pair<double,double>(x_original,y_original));
    vec_original.push_back(std::pair<double,double>(x_original,y_original));

    p.transform(cs1,cs2,vec_original,vec_utm15,isLatLon);
    for( size_t i=0;i<vec_original.size();++i){
        if(std::abs(vec_utm15[i].first-x_expected)>tol || std::abs(vec_utm15[i].second-y_expected)>tol){
            std::cout << "Expected: " << x_expected << " " << y_expected << std::endl;
            printf("Got: %14.6f, %14.6f\n", vec_utm15[i].first, vec_utm15[i].second);
            return 1;
        }
    }

    return 0;

}
