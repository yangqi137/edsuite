#include "edsolver.hpp"
#include <arpack_drv/basic_drv.hpp>

int main(int argc, char *argv[]) {
  return edsolver::EDSolverMain<unsigned long,
				double,
				arpack::BasicDrv>(argc, argv);
}
