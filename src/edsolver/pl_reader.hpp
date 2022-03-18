#ifndef EDSUITE_EDSOLVER_PARAMETER_LIST_READER_HPP
#define EDSUITE_EDSOLVER_PARAMETER_LIST_READER_HPP

#include "ptree_read_number.hpp"
#include <boost/numeric/ublas/vector.hpp>
#include <vector>
#include <complex>

namespace edsolver {
  template <typename T>
  boost::numeric::ublas::vector<T>
  ReadPL(const std::vector<std::string>& names,
	 const boost::property_tree::ptree& node) {
    boost::numeric::ublas::vector<T> result;
    result.resize(names.size());

    for (unsigned i=0;i<names.size();i++)
      result[i] = node.get<T>(names[i]);
    //result[i] = PTNodeReader<T>::read(node.get_child(names[i]));

  return result;
  }
}

#endif
