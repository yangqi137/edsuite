#ifndef EDSUITE_EDSOLVER_AV_READER_HPP
#define EDSUITE_EDSOLVER_AV_READER_HPP

#include <h5generic/reader/readers.hpp>
#include <h5generic/buffer/buffer.hpp>
#include <h5generic/types/type.hpp>
#include <H5Cpp.h>
#include <boost/numeric/bindings/traits/type_traits.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <string>
#include <memory>
#include <vector>
#include <iostream>

namespace edsolver {
  class AVReader {
  public:
    AVReader(const std::string& filename);

  public:
    std::vector<std::string> readParamNames() const;
    template <typename SpMatType, typename PLType>
    void readMatrix(SpMatType& mat, const PLType& pl) const {
      using namespace h5generic;
      using namespace boost::numeric;
      typedef typename SpMatType::index_type index_type;
      typedef typename SpMatType::value_type value_type;
      typedef typename bindings::traits::type_traits<value_type>::real_type
	real_type;
      H5::Group matrix = root->openGroup("matrix");
      ReadDataSet(matrix.openDataSet("rowIndex"),
		  MakeVectorBuffer(mat.rowIndex));
      ReadDataSet(matrix.openDataSet("columns"),
		  MakeVectorBuffer(mat.columns));

      hsize_t N = mat.columns.size();
      mat.entries.resize(N);
      hsize_t n = pl.size();
      ublas::vector<real_type> buf(n);
      H5::ArrayType bufDT(types::H5TypeFactory<real_type>::build(),
			  1, &n);
      H5::DataSpace bufDSpace; // Scalar
      H5::DataSet coeffDSet = matrix.openDataSet("coeffs");
      H5::DataSpace coeffDSpace = coeffDSet.getSpace();
      for (hsize_t i=0;i<N;i++) {
	coeffDSpace.selectElements(H5S_SELECT_SET, 1, &i);
	coeffDSet.read(&buf[0], bufDT, bufDSpace, coeffDSpace);

	mat.entries[i] = ublas::inner_prod(buf, pl);
	//std::cout<<mat.entries[i]<<std::endl;
      }
    }

  private:
    std::auto_ptr<H5::CommonFG> root;
  };
}

#endif
