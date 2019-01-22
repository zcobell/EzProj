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
    const double x_original = -35625.789763;
    const double y_original = 3343350.030257;
    const double x_expected = -90.3661965220;
    const double y_expected = 30.0573591369;
    const double lambda0 = -90.0;
    const double phi0 = 29.0;
    const double tol = 0.000001;
    double x_invcpp,y_invcpp;

    std::cout << "Projecting a single x, y" << std::endl;
    p.inverseCpp(lambda0,phi0,x_original,y_original,x_invcpp,y_invcpp);

    char buf[50];
    sprintf(buf, "Original X coordinate: %f", x_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected X coordinate: %f", x_invcpp);
    std::cout << buf << std::endl;
    sprintf(buf, "Original Y coordinate: %f", y_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected Y coordinate: %f", y_invcpp);
    std::cout << buf << std::endl;

    if (std::abs(x_invcpp - x_expected) > tol ||
        std::abs(y_invcpp - y_expected) > tol) {
      std::cout << "Expected: " << x_expected << " " << y_expected << std::endl;
      printf("Got: %14.6f, %14.6f\n", x_invcpp, y_invcpp);
      return 1;
    }

    //...Project a pair (Point)
    std::cout << "Projecting a pair" << std::endl;
    Point pt(x_original,y_original);
    Point pt_out;
    p.inverseCpp(lambda0,phi0,pt,pt_out);
    if (std::abs(pt_out.first - x_expected) > tol ||
        std::abs(pt_out.second - y_expected) > tol) {
      std::cout << "Expected: " << x_expected << " " << y_expected << std::endl;
      printf("Got: %14.6f, %14.6f\n", pt_out.first, pt_out.second);
      return 1;
    }

    //...Project a vector of pairs (Point)
    std::cout << "Projecting a vector of Points" << std::endl;
    std::vector<Point> vec_original;
    std::vector<Point> vec_invcpp;
    vec_original.push_back(std::pair<double,double>(x_original,y_original));
    vec_original.push_back(std::pair<double,double>(x_original,y_original));
    vec_original.push_back(std::pair<double,double>(x_original,y_original));

    p.inverseCpp(lambda0,phi0,vec_original,vec_invcpp);
    for( size_t i=0;i<vec_original.size();++i){
        if(std::abs(vec_invcpp[i].first-x_expected)>tol || std::abs(vec_invcpp[i].second-y_expected)>tol){
            std::cout << "Expected: " << x_expected << " " << y_expected << std::endl;
            printf("Got: %14.6f, %14.6f\n", vec_invcpp[i].first, vec_invcpp[i].second);
            return 1;
        }
    }

    return 0;

}
