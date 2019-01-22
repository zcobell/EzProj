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

    double x_original = -90.3661965220;
    double y_original = 30.0573591369;
    double x_utm15,y_utm15;
    bool isLatLon;

    p.transform(4326,26915,x_original,y_original,x_utm15,y_utm15,isLatLon);

    char buf[50];
    sprintf(buf, "Original X coordinate: %f", x_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected X coordinate: %f", x_utm15);
    std::cout << buf << std::endl;
    sprintf(buf, "Original Y coordinate: %f", y_original);
    std::cout << buf << std::endl;
    sprintf(buf, "Projected Y coordinate: %f", y_utm15);
    std::cout << buf << std::endl;

    if (std::abs(x_utm15 - 753922.922116) > 0.000001 ||
        std::abs(y_utm15 - 3328065.712818) > 0.000001) {
      std::cout << "Expected: 753922.922116, 3328065.712818" << std::endl;
      printf("Got: %14.6f, %14.6f\n", x_utm15, y_utm15);
      return 1;
    }

    return 0;

}
