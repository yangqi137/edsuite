#ifndef EDSUITE_EDSOLVER_SPMAT_AV_HPP
#define EDSUITE_EDSOLVER_SPMAT_AV_HPP

#include <cassert>

namespace edsolver {
  template <class SpMatType>
  class SpMatAv {
  public:
    typedef SpMatType matrix_type;
    SpMatAv(const matrix_type& A_)
    : A(A_) {}

    template <typename x_type, typename y_type>
    void operator () (x_type x, y_type y) const {
      typedef typename matrix_type::index_type index_type;
      assert(A.columns.size()==A.entries.size());
      index_type n = A.vector_size();

      assert(A.rowIndex[0]==0);
      #pragma omp parallel for schedule(static)
      for (index_type i=0;i<n;i++) {
	y[i]=0;
	assert(A.rowIndex[i+1]<=A.columns.size());
	for (index_type j=A.rowIndex[i];j<A.rowIndex[i+1];j++) {
	  assert(A.columns[j]<n);
	  y[i] += x[A.columns[j]] * A.entries[j];
	}
      }
    }

  private:
    const matrix_type& A;
  };

  template <class SpMatType>
  SpMatAv<SpMatType> MakeSpMatAv(const SpMatType& mat) {
    return SpMatAv<SpMatType>(mat);
  }
}

#endif
