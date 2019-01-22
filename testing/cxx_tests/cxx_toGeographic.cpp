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

int main() {

    Ezproj p;
    std::cout << "INFO: Using proj version: " << p.projVersion() << std::endl;

    double x_original = 753922.922116;
    double y_original = 3328065.712818;
    double x_geographic,y_geographic;
    bool isLatLon;

    p.transform(26915,4326,x_original,y_original,x_geographic,y_geographic,isLatLon);

    char buf[50];
    sprintf(buf, "Original X coordinate: %f", x_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected X coordinate: %f", x_geographic);
    std::cout << buf << std::endl;
    sprintf(buf, "Original Y coordinate: %f", y_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected Y coordinate: %f", y_geographic);
    std::cout << buf << std::endl;

    if (std::abs(x_geographic - -90.3661965220) > 0.000001 ||
        std::abs(y_geographic - 30.0573591369) > 0.000001) {
      std::cout << "Expected: -90.3661965220, 30.0573591369" << std::endl;
      printf("Got: %14.6f, %14.6f\n", x_geographic, y_geographic);
      return 1;
    }

    return 0;

}
