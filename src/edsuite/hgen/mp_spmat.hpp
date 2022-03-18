#ifndef EDSUITE_HGEN_MULTIPARAM_SPARSE_MATRIX_HPP
#define EDSUITE_HGEN_MULTIPARAM_SPARSE_MATRIX_HPP

#include <boost/array.hpp>
#include <vector>

namespace edsuite {
  namespace hgen {
    template <unsigned NPARAM,
	      typename VALUE_T=double,
	      typename INDEX_T=unsigned long>
    class MPSpMat {
    public:
      typedef VALUE_T value_type;
      typedef INDEX_T index_type;
      static const unsigned nParam = NPARAM;

      typedef boost::array<value_type, nParam> coeff_type;
      MPSpMat(index_type n)
      : entries(n) {}

      template <typename CoeffVecType>
      void insert(index_type i, index_type j,
		  const CoeffVecType& coeff) {
	coeff_type c;
	for (unsigned ci=0;ci<nParam;ci++)
	  c[ci]=coeff[ci];
	entries[i].push_back(std::make_pair(j, c));
	//columns.insert(columns.begin()+rowIndex[i+1], j);
	//coeffs.insert(coeffs.begin()+rowIndex[i+1], c);
	//for (index_type ii=i+1;ii<rowIndex.size();ii++)
	//  rowIndex[ii]++;
      }

      void addEntry() { 
	entries.push_back(std::vector<std::pair<index_type, coeff_type> >() );
      }

      const std::vector<std::vector<std::pair<index_type, coeff_type>>>&
      getEntries() const {return entries;}

    private:
      std::vector<std::vector<std::pair<index_type, coeff_type>>> entries;
      //std::vector<index_type> rowIndex;
      //std::vector<index_type> columns;
      //std::vector<coeff_type> coeffs;
    };
  }
}

#endif
