#ifndef EDSUITE_RESULTWRITER_HPP
#define EDSUITE_RESULTWRITER_HPP

#include <h5generic/buffer/buffer.hpp>
#include <h5generic/buffer/bnb.hpp>
#include <h5generic/writer/writers.hpp>

#include <boost/numeric/bindings/ublas.hpp>
#include <H5Cpp.h>

#include <memory>

namespace edsolver {
  class ResultWriter {
  public:
    ResultWriter(const std::string& filename)
      : root(new H5::H5File(filename, H5F_ACC_TRUNC)) {}
    ResultWriter(const H5::H5File& root)
      : root(new H5::H5File(root)) {}
    ResultWriter(const H5::Group& root)
      : root(new H5::Group(root)) {}

  public:

    template <class DrvType>
    void writeSln(const DrvType& drv) {
      using namespace h5generic;
      H5::Group slns = root->createGroup("slns");
      for (unsigned i=0;i<drv.get_nconv();i++) {
	std::ostringstream oss;
	oss<<i<<std::ends;

	H5::DataSet ds =
	  CreateDataSet(slns, oss.str(),
			MakeBNBVectorBuffer<const typename DrvType::evec_return_type>(drv.evec(i)));
	CreateAttribute(ds, "E", MakeScalarBuffer(drv.evals()[i]));
      }
    }

  private:
    std::unique_ptr<H5::Group> root;
  };
}

#endif
