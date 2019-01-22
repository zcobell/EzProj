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

extern "C" {
void *ezproj_create_();
void ezproj_delete_(void *ptr);
void ezproj_project_(void *ptr, int *epsg_from, int *epsg_to, double *x,
                     double *y, double *out_x, double *out_y, int *ierr);
void ezproj_cpp_(double *lambda0, double *phi0, double *x, double *y,
                 double *out_x, double *out_y, int *ierr);
void ezproj_inverseCpp_(double *lambda0, double *phi0, double *x, double *y,
                        double *out_x, double *out_y, int *ierr);
}

void *ezproj_create_() {
  Ezproj *transformer = new Ezproj();
  void *ptr = static_cast<void *>(transformer);
  return ptr;
}

void ezproj_delete_(void *ptr) {
  Ezproj *transformer = reinterpret_cast<Ezproj *>(ptr);
  delete transformer;
  return;
}

void ezproj_project_(void *ptr, int *epsg_from, int *epsg_to, double *x,
                     double *y, double *out_x, double *out_y, int *ierr) {
  bool isLatLon;
  Ezproj *transformer = reinterpret_cast<Ezproj *>(ptr);
  *ierr = transformer->transform(*(epsg_from), *(epsg_to), *(x), *(y), *(out_x),
                                 *(out_y), isLatLon);
  return;
}

void ezproj_cpp_(double *lambda0, double *phi0, double *x, double *y,
                 double *out_x, double *out_y, int *ierr) {
  *ierr = Ezproj::cpp(*(lambda0), *(phi0), *(x), *(y), *(out_x), *(out_y));
}

void ezproj_inverseCpp_(double *lambda0, double *phi0, double *x, double *y,
                        double *out_x, double *out_y, int *ierr) {
  *ierr =
      Ezproj::inverseCpp(*(lambda0), *(phi0), *(x), *(y), *(out_x), *(out_y));
}
