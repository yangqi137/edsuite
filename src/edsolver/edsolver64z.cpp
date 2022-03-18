#include "edsolver.hpp"
#include <arpack_drv/herm_drv.hpp>

int main(int argc, char *argv[]) {
  return edsolver::EDSolverMain<unsigned long,
				std::complex<double>,
				arpack::HermDrv>(argc, argv);
}
