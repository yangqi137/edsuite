MODULE ReadHam
  IMPLICIT NONE
  USE hdf5

  INTEGER, SAVE :: nParam
  INTEGER, PARAMETER :: ParamLen = 256
  CHARACTER(LEN=ParamLen), DIMENSION(:), ALLOCATABLE, SAVE :: ParamNames

  INTEGER(HID_T), PRIVATE :: fid

CONTAINS

  SUBROUTINE OpenHamFile(fn)
    CHARACTER(LEN=*), INTENT(IN) :: fn

    INTEGER :: hdferr
    INTEGER(HID_T) :: dset_id, dspace_id
    INTEGER(HSIZE_T), DIMENSION(1) :: dims

    CALL h5fopen_f(fn, H5F_ACC_RONLY, fid, hdferr)
    CALL h5dopen_f(fid, 'param_names', dset_id, hdferr)
    CALL h5dget_space_f(dset_id, dspace_id, hdferr)

    CALL h5sget_simple_extent_dims_f(dspace_id, dims, NULL, hdferr)
    nParam = dims(1)
    ALLOCATE(ParamNames(nParam))

    CALL h5sclose_f(dspace_id, hdferr)
    CALL h5dclose_f(dset_id, hdferr)
  END SUBROUTINE OpenHamFile

  SUBROUTINE CloseHamFile()
    INTEGER :: hdferr
    CALL h5fclose_f(fid, hdferr)
  END SUBROUTINE CloseHamFile

END MODULE ReadHam
