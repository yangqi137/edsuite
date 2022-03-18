#ifndef EDSUITE_EDSOLVER_SPMAT_HPP
#define EDSUITE_EDSOLVER_SPMAT_HPP

#include <vector>

namespace edsolver {
  template <typename IndexType, typename ValueType>
  class SpMat {
  public:
    typedef IndexType index_type;
    typedef ValueType value_type;

  public:
    SpMat() {}
    
    void swap(SpMat& other) {
      rowIndex.swap(other.rowIndex);
      columns.swap(other.columns);
      entries.swap(other.entries);
    }

    index_type vector_size() const {return rowIndex.size()-1;}

  public:
    std::vector<index_type> rowIndex;
    std::vector<index_type> columns;
    std::vector<value_type> entries;
  };
}

#endif
