PROGRAM fsolver
  IMPLICIT NONE
  INTEGER, PARAMETER :: OptLen = 32
  CHARACTER(LEN=OptLen) :: paramFn, hamFn, outputFn
  INTEGER :: status

  CALL get_command_argument(1, paramFn, status=status)
  IF (status .EQ. 0) THEN
     CALL get_command_argument(2, hamFn, status=status)
  END IF
  IF (status .EQ. 0) THEN
     CALL get_command_argument(3, outputFn, status=status)
  END IF

  IF (status .EQ. 0) THEN
     WRITE (*,*) paramFn
     WRITE (*,*) hamFn
     WRITE (*,*) outputFn
  ELSE
     WRITE (*,*) 'Useage: fsolver param_file hamiltonian_file output_file'
  END IF

END PROGRAM fsolver
