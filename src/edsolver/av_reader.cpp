#include "av_reader.hpp"
#include <h5generic/types/fl_str.hpp>
using namespace h5generic;

edsolver::AVReader::AVReader(const std::string& filename)
  : root(new H5::H5File(filename, H5F_ACC_RDONLY)) {}

std::vector<std::string> edsolver::AVReader::readParamNames() const {
  std::vector<types::FLStr<256> > plBuf;
  ReadDataSet(root->openDataSet("param_names"), MakeVectorBuffer(plBuf));

  std::vector<std::string> pl;
  for (unsigned i=0;i<plBuf.size();i++)
    pl.push_back(plBuf[i].str());
  return pl;
}
