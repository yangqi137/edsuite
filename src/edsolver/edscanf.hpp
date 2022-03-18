#ifndef EDSUITE_EDSOLVER_EDSCANF_HPP
#define EDSUITE_EDSOLVER_EDSCANF_HPP

#include "av_reader.hpp"
#include "pl_reader.hpp"
#include "spmat.hpp"
#include "spmat_av.hpp"
#include "result_writer.hpp"

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>

#include <iostream>
#include <exception>
#include <complex>

namespace edsolver {
  template <typename index_type,
	    typename param_type,
	    class drv_type> 
  int EDScanfMain(int argc, char *argv[]) {
    namespace pt = boost::property_tree;
    namespace ublas = boost::numeric::ublas;

    if (argc != 4) {
      std::cerr<<"Useage: "<<argv[0]
	       <<" param_file hamiltonian_file"
	       <<" output_file"<<std::endl;
      return 1;
    }

    try {
      pt::ptree intree;
      read_json(argv[1], intree);
      
      AVReader avReader(argv[2]);
      ResultWriter rw(argv[3]);
      SpMat<index_type, param_type> A;
      unsigned n = intree.get<unsigned>("runs");
      auto plrange = 
	PRangeReader<param_type>(avReader.readParamNames(),
				 intree.get_child("model"), n);

      SpMat<index_type, param_type> A;
      std::cout<<"Reading matrix..."<<std::endl;
      avReader.readMatrix(A, plrange(0));
      std::cout<<"Done."<<std::endl;
      std::cout<<"N = "<<A.vector_size()<<std::endl;
      unsigned nev = intree.get("arpack.nev", 2u);
      drv_type drv(A.vector_size(),
		   nev,
		   intree.get("arpack.ncv", 5u));
      drv.set_maxiter(intree.get("arpack.maxiter", 500u));

      typedef ublas::vector<std::complex<double> > WFVec;
      WFVec f0(A.vector_size());

      std::vector<double> fugacities(n-1);
      ublas::matrix<double> evals(n, nev);

      for (unsigned i=0; i<n; i++) {
	std::cout<<i<<"/"<<n<<std::endl;
	if (i != 0) {
	  std::cout<<"Reading matrix..."<<std::endl;
	  avReader.readMatrix(A, plrange(i));
	  std::cout<<"Done."<<std::endl;
	}

	std::cout<<"Diagonalizing..."<<std::endl;
	drv.solve(MakeSpMatAv(A));
	std::cout<<"Done."<<std::endl;
	std::cout<<"# conv: "<<drv.get_nconv()<<std::endl;
	std::cout<<"E0: "<<drv.evals()(0)<<std::endl;

	if (i != 0) {
	  std::complex<double> overlap 
	    = ublas::inner_prod(f0, drv.evec(0));
	  fugacities[i-1] = std::abs(overlap);
	  f0 = drv.evec(0);
	}

	for (unsigned j=0; j<drv.get_nconv(); j++)
	  evals(i, j) = drv.evals()(j);
	
      }      
      
      // rw.writeSln(drv);
    }
    catch (std::exception& e) {
      std::cerr<<"[Exception]: "<<e.what()<<std::endl;
      return 1;
    }
    return 0;

  }
}

#endif
