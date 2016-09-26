//-----------------------------------------------------------------------------
// Nek5000/cxx-driver/main.cpp
//
// This demonstrates linking to a subroutine and common block from Nek5000.  It
// just initializes Nek and prints a few variables from a common block.
//
// The corresponding Makefile (Nek5000/cxx-driver/Makefile) works with GNU and
// compiles/links the driver with something like:
//   g++ -Wall main.cpp obj/<case_name>.o -lnek5000 -lgfortran
// But you need to include the right LDFLAGS (such as -L<case_name>/obj)
//
// requires:
//   libnek5000.a
//   <case_name>.o
//   SESSION.NAME
// The first two can be satisfied by running:
//   makenek <case_name> && make lib
// The SESSION file must be written manually for now.  The file format is the
// same as you would use for any other Nek5000 problem: 
//   <case_name>
//   <working_directory>
//
//-----------------------------------------------------------------------------

#include <iostream>
using namespace std;

// String length for file paths in Nek5000
#define PATHLEN 132

// Nek Fortran interface
extern "C" {

  // CFILES common block
  extern struct {
    char \
      reafle[PATHLEN], \
      fldfle[PATHLEN], \
      dmpfle[PATHLEN], \
      hisfle[PATHLEN], \
      schfle[PATHLEN], \
      orefle[PATHLEN], \
      nrefle[PATHLEN];
  } cfiles_;

  // DIMN common block
  extern struct {
    int \
      nelv,  \
      nelt,  \
      nx1,   \
      ny1,   \
      nz1,   \
      nx2,   \
      ny2,   \
      nz2,   \
      nx3,   \
      ny3,   \
      nz3,   \
      ndim,  \
      nfield,\
      npert, \
      nid,   \
      nxd,   \
      nyd,   \
      nzd;
  } dimn_;

  // subroutine nek_init(intracomm)
  void nek_init_(int &);

}

int main() {
  cout << "Hello, Nek!" << endl;

  int intracomm=0;
  nek_init_(intracomm);

  cout << "nelv = " << dimn_.nelv << endl;
  cout << "nx1  = " << dimn_.nx1 << endl;
  cout << "ny1  = " << dimn_.ny1 << endl;
  cout << "nz1  = " << dimn_.nz1 << endl;

  cout << "Goodbye, Nek!" << endl;
  return 0;
}
