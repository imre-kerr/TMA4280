PROGRAM EX4_MPI
	INCLUDE 'mpif.h'
	INTEGER*8 :: K, DIM
	INTEGER :: RANK, SIZE, IERROR, TAG
	INTEGER :: STATUS(MPI_STATUS_SIZE)
	REAL*8, PARAMETER :: S = (4.D0 * DATAN(1.D0))**2 / 6
	REAL*8 :: SK

	CALL MPI_INIT(IERROR)
	CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERROR)
	CALL MPI_COMM_SIZE(MPI_COMM_WORLD, SIZE, IERROR)

	WRITE (*, "('S = PI^2/6 = 'F7.5)") S

	DO K = 27, 27
		SK = S_N(2**K, RANK, SIZE)
		IF (RANK .EQ. 0) THEN
			WRITE (*, *)
			WRITE (*, "('SN FOR N = 2^'I0.2' = 'F7.5)") K, SK
			WRITE (*, "('S - SN = 'ES13.7)") S-SK
		END IF
	END DO

	CALL MPI_FINALIZE(IERROR)
END PROGRAM

REAL FUNCTION S_N(N, RANK, SIZE)
	INTEGER*8 :: N, I
	REAL*8, DIMENSION(N) :: V

	DO I = 1, N
		V(I) = 1.0 / I**2
	END DO
	S_N = 0.0
	DO I = N, 1, -1
		S_N = S_N + V(I)
	END DO
	RETURN
END FUNCTION