#ifndef EDSUITE_EDSOLVER_HPP
#define EDSUITE_EDSOLVER_HPP

#include "av_reader.hpp"
#include "pl_reader.hpp"
#include "spmat.hpp"
#include "spmat_av.hpp"
#include "result_writer.hpp"

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#include <iostream>
#include <exception>

namespace edsolver {
  template <typename index_type,
	    typename param_type,
	    class drv_type>
  int EDSolverMain(int argc, char *argv[]) {
    namespace pt = boost::property_tree;

    if (argc!=4) {
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
      std::cout<<"Reading matrix..."<<std::endl;
      avReader.readMatrix(A, 
			  ReadPL<param_type>(avReader.readParamNames(),
					     intree.get_child("model")));
      std::cout<<"Done."<<std::endl;
      std::cout<<"N = "<<A.vector_size()<<std::endl;
      
      drv_type drv(A.vector_size(),
		   intree.get("arpack.nev", 2u),
		   intree.get("arpack.ncv", 5u));
      drv.set_maxiter(intree.get("arpack.maxiter", 500u));
      //SpMatAv<SpMat<index_type, complex_d> > Av(A);
      std::cout<<"Diagonalizing..."<<std::endl;
      drv.solve(MakeSpMatAv(A));
      std::cout<<"Done."<<std::endl;
      
      std::cout<<"# conv: "<<drv.get_nconv()<<std::endl;
      std::cout<<"E0: "<<drv.evals()(0)<<std::endl;
      
      rw.writeSln(drv);
    }
    catch (std::exception& e) {
      std::cerr<<"[Exception]: "<<e.what()<<std::endl;
      return 1;
    }
    return 0;
  }
}

#endif
