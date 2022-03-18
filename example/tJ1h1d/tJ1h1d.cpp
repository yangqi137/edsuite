#include <edsuite/hgen/mp_spmat.hpp>
#include <edsuite/hgen/mph5writer.hpp>

#include <edtools/bitop/swap_bits.hpp>
#include <edtools/fermionhash/encoder.hpp>
#include <edtools/fermionhash/config_gen.hpp>

#include <boost/numeric/ublas/vector.hpp>

#include <iostream>

int main(void) {
  std::cout<<"t-J Model one hole problem (1D) Hamiltonian generator.\n"
	   <<"Please enter nSite:";
  unsigned nsite;
  std::cin>>nsite;

  typedef unsigned long config_type;
  typedef unsigned long code_type;
  const config_type one = 1u;
  const edtools::fermionhash::ConfigGen<config_type> 
    config_gen(nsite-1, nsite/2-1);
  edtools::fermionhash::Encoder<config_type, code_type> encoder(nsite);
  code_type N = encoder(*config_gen.end());

  static const unsigned nParam = 3;
  std::vector<std::string> pnames(nParam);
  pnames[0]="t";
  pnames[1]="t_conj";
  pnames[2]="J";

  boost::numeric::ublas::unit_vector<double>
    t(nParam, 0), t_conj(nParam, 1), J(nParam, 2);
  edsuite::hgen::MPSpMat<nParam, double, code_type> mat(N);
  
  for (config_type cx : config_gen) {
    // First, calculate the J term.
    double diag = 0;
    for (unsigned v1_i=0;v1_i<nsite-2;v1_i++) {
      unsigned v2_i = v1_i+1;
      unsigned szsz = ((cx>>v1_i)^(cx>>v2_i)) & one;
      diag += (0.5-szsz)/2.;
      
      config_type cy=cx;
      edtools::bitop::swap_bits(cy, v1_i, v2_i);
      if (cx!=cy) {
	mat.insert(encoder(cx), encoder(cy), 0.5*J);
      }
    }
    mat.insert(encoder(cx), encoder(cx), diag*J);

    {
      // s s ... s <-> h s ...
      config_type cy=cx;
      edtools::bitop::swap_bits(cy, 0, nsite-1); // cy[n-1]=cy[0]
      cy >>= 1;
      //std::cout<<"cy="<<edtools::ConfigToString(cy)<<std::endl;
      mat.insert(encoder(cx), encoder(cy), -t);
    }
    {
      // s s ... s h <-> s s...
      config_type cy=cx << 1;
      edtools::bitop::swap_bits(cy, 0, nsite-1); // cy[0]=0 <-> cy[n-1]
      //std::cout<<"cy="<<edtools::ConfigToString(cy)<<std::endl;
      mat.insert(encoder(cx), encoder(cy), -t_conj);
    }
  }

  edsuite::hgen::MPH5Writer writer("output.hdf5");
  writer.writeParameters(pnames);
  writer.writeMat(mat);
}
