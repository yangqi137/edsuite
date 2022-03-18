#ifndef EDSUITE_EDSOLVER_PARAMETER_RANGE_LIST_READER_HPP
#define EDSUITE_EDSOLVER_PARAMETER_RANGE_LIST_READER_HPP

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/matrix_proxy.hpp>
#include <boost/property_tree/ptree.hpp>
#include <stdexcept>

namespace edsolver {
  namespace impl_ {
    template <typename T>
    class ParamRangeList {
    public:
      typedef T value_type;

      ParamRangeList(unsigned np, unsigned n)
	: data(np, n) {}
      
      boost::numeric::ublas::column<DataType>
      operator() (unsigned i) {
	return column(data, i);
      }

      boost::numeric::ublas::row<DataType>
      paramRange(unsigned i) {
	return row(data, i);
      }

    private:
      typedef boost::numeric::ublas::matrix<value_type> DataType;
      boost::numeric::ublas::matrix<value_type> data;
    };

    template <typename VE>
    void ReadParamRange(VE pr, const boost::property_tree::ptree& node) {
      typedef typename VE::value_type value_type;
      unsigned n = pr.size();
      if (node.size() == 0) {
	value_type x = node.get_value<value_type>();
	for (auto & p : pr)
	  p = x;
      }
      else if (node.size() == 2) {
	value_type x0 = node.front().second.get_value<value_type>();
	value_type x1 = node.back().second.get_value<value_type>();
	value_type dx = (x1-x0) / (n-1);
	for (unsigned i=0; i<n; i++)
	  pr(i) = x0 + x1*n;
      }
      else if (node.size() == n) {
	for (unsigned i=0; i<n; i++)
	  pr(i) = node[i].second.get_value<value_type>();
      }
      else
	throw std::invalid_argument("Parameter format is incorrect.");
    }
  }
  
  template <typename T>
  impl_::ParamRangeList<T>
  ReadParamRangeList(const std::vector<std::string>& names,
		     const boost::property_tree::ptree& node,
		     unsigned n) {
    unsigned np = names.size();
    impl_::ParamRangeList<t> list(np, n);
    for (unsigned i=0; i<np; i++)
      ReadParamRange(list.paramRange(i), node[i]);
    return list;
  }
  
}

#endif
