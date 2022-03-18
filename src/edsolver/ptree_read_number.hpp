#ifndef EDSUITE_EDSOLVER_PTREE_READ_NUMBER_HPP
#define EDSUITE_EDSOLVER_PTREE_READ_NUMBER_HPP

#include <boost/property_tree/ptree.hpp>
#include <complex>
#include <stdexcept>

namespace edsolver {
  template <typename T>
  struct PTNodeReader {
    typedef T value_type;
    static value_type read(const boost::property_tree::ptree& node) {
      return node.get_value<T>();
    }
  };
  template <typename T>
  struct PTNodeReader<std::complex<T> > {
    typedef std::complex<T> value_type;
    static value_type read(const boost::property_tree::ptree& node) {
      if (node.size()==0)
	return node.get_value<T>();
      else if (node.size()==2) {
	T re = node.front().second.get_value<T>();
	T im = node.back().second.get_value<T>();
	return std::complex<T>(re, im);
      }
      else
	throw std::invalid_argument("Parameter format is incorrect");
    }
  };
}

#endif
