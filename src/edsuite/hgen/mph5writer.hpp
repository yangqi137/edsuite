#ifndef EDSUITE_HGEN_MULTIPARAM_HDF5_WRITTER_HPP
#define EDSUITE_HGEN_MULTIPARAM_HDF5_WRITTER_HPP

#include <h5generic/buffer/buffer.hpp>
#include <h5generic/writer/writers.hpp>
#include <h5generic/types/fl_str.hpp>
#include <H5Cpp.h>
#include <memory>
#include <iostream>

namespace edsuite {
  namespace hgen {
    class MPH5Writer {
    public:
      MPH5Writer(const std::string& filename)
	: root(new H5::H5File(filename, H5F_ACC_TRUNC)) {}
      MPH5Writer(const H5::H5File& root_)
	: root(new H5::H5File(root_)) {}
      MPH5Writer(const H5::Group& root_)
	: root(new H5::Group(root_)) {}

    public:
      void writeParameters(const std::vector<std::string>& param_names) {
	using namespace h5generic;
	typedef types::FLStr<256> flstr_type;
	std::size_t nparam = param_names.size();
	std::vector<flstr_type> buf(nparam);
	for (std::size_t i=0;i<nparam;i++)
	  buf[i]=param_names[i];
	CreateDataSet(*root, "param_names", MakeVectorBuffer(buf));
      }
      template <class MpSpMatType>
      void writeMat(const MpSpMatType& mat) {
	typedef typename MpSpMatType::index_type index_type;

	using namespace h5generic;
	H5::Group mat_group = root->createGroup("matrix");

	const auto& entries = mat.getEntries();
	const index_type n = entries.size();
	std::vector<index_type> rowIndex(n+1);
	std::vector<index_type> columns;
	std::vector<typename MpSpMatType::coeff_type> coeffs;
	rowIndex[0]=0;
	std::cout<<"Creating sparse matrix..."<<std::endl;
	for (index_type i=0;i<n;i++) {
	  const index_type ni = entries[i].size();
	  rowIndex[i+1] = rowIndex[i] + ni;
	  for (index_type ii=0;ii<ni;ii++) {
	    auto entry = entries[i][ii];
	    columns.push_back(entry.first);
	    coeffs.push_back(entry.second);
	  }
	}
	std::cout<<"Writing to hdf5 file"<<std::endl;
	CreateDataSet(mat_group, "rowIndex",
		      MakeVectorBuffer(rowIndex));
	CreateDataSet(mat_group, "columns",
		      MakeVectorBuffer(columns));
	CreateDataSet(mat_group, "coeffs",
		      MakeVectorBuffer(coeffs));
      }

    private:
      std::auto_ptr<H5::CommonFG> root;
    };
  }
}

#endif
