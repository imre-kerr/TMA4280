PROGRAM EX4_MPI
    USE MPI
    INTEGER :: K, DIM
    INTEGER :: RANK, SIZE, IERROR, TAG
    INTEGER :: STATUS(MPI_STATUS_SIZE)
    REAL, PARAMETER :: S = (4.D0 * DATAN(1.D0))**2 / 6
    REAL :: SK, S_N

    CALL MPI_INIT(IERROR)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, SIZE, IERROR)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERROR)

    IF (RANK .EQ. 0) THEN
        WRITE (*, "('S = PI^2/6 = 'F7.5)") S
    END IF  
    DO K = 4,14
        SK = S_N(2**K, RANK, SIZE)
        IF (RANK .EQ. 0) THEN
            WRITE (*, *)
            WRITE (*, "('SN FOR N = 2^'I0.2' = 'F7.5)") K, SK
            WRITE (*, "('S - SN = 'ES13.7)") S-SK
        END IF
    END DO

    CALL MPI_FINALIZE(IERROR)
END PROGRAM

REAL FUNCTION S_N(N, RANK, SIZ)
    USE MPI
    INTEGER :: N, I, J, RANK, SIZ, IERROR
    REAL :: SUM
    REAL, DIMENSION(N/SIZ) :: V
      PRINT *, SIZE(V)
      PRINT *, N/SIZ

    IF(RANK .EQ. 0) THEN
        DO I = SIZ-1, 1, -1
            DO J = 1, N/SIZ
                V(J) = 1.0/(J+(I*N)/SIZ)**2
            END DO
            CALL MPI_SEND(V, N/SIZ, MPI_REAL, I, 0, MPI_COMM_WORLD, IERROR)
        END DO
        DO J = 1, N/SIZ
            V(J) = 1.0/J**2
        END DO
        SUM = 0.0
        DO J = N/SIZ, 1, -1
            SUM = SUM + V(J)
        END DO
    ELSE
        CALL MPI_RECV(V, N/SIZ, MPI_REAL, 0, 0, MPI_COMM_WORLD, IERROR)
        SUM = 0.0
        DO I = N/SIZ, 1, -1
            SUM = SUM + V(I)
        END DO
    END IF
    CALL MPI_REDUCE(SUM, S_N, N/SIZ, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, IERROR)
    RETURN
END FUNCTION
