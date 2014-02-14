PROGRAM EX4_MPI
    USE MPI
    INTEGER*8 :: K
    INTEGER :: RANK, SIZ, TERROR
    INTEGER :: STATUS(MPI_STATUS_SIZE)
    REAL*8, PARAMETER :: S = (4.D0 * DATAN(1.D0))**2 / 6
    REAL*8 :: SK, S_N

    CALL MPI_INIT(TERROR)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, SIZ, TERROR)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, TERROR)

    IF (RANK .EQ. 0) THEN
        WRITE (*, "('S = PI^2/6 = 'F7.5)") S
    END IF  
    DO K = 3, 14
        SK = S_N(2**K, RANK, SIZ)
        IF (RANK .EQ. 0) THEN
            WRITE (*, *)
            WRITE (*, "('SN FOR N = 2^'I0.2' = 'F7.5)") K, SK
            WRITE (*, "('S - SN = 'ES13.7)") S-SK
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, TERROR)
    END DO

    CALL MPI_FINALIZE(TERROR)
END PROGRAM

REAL*8 FUNCTION S_N(N, RANK, SIZ)
    USE MPI
    INTEGER*8 :: N, I, J, CNT
    INTEGER :: RANK, SIZ, TERROR
    INTEGER :: STATUS(MPI_STATUS_SIZE)
    REAL*8 :: SUM
    REAL*8, DIMENSION(N/SIZ+1) :: V

    IF(RANK .EQ. 0) THEN
        DO I = SIZ-1, 0, -1
            CNT = 0
            DO J = I+1, N, SIZ

                CNT = CNT + 1
                V(CNT) = 1.0/(J**2)
            END DO
    
            IF (I .NE. 0) THEN
                CALL MPI_SEND(V, CNT, MPI_REAL8, I, 0, MPI_COMM_WORLD, TERROR)
            END IF
        END DO
    ELSE
        CNT = N/SIZ
        IF (RANK .LT. MOD(N, SIZ)) THEN
            CNT = CNT + 1
        END IF
        CALL MPI_RECV(V, CNT, MPI_REAL8, 0, 0, MPI_COMM_WORLD, STATUS, TERROR)
    END IF
    
    SUM = 0.0
    DO I = CNT, 1, -1
        SUM = SUM + V(I)
    END DO
    S_N = 0.0
    CALL MPI_REDUCE(SUM, S_N, 1, MPI_REAL8, MPI_SUM, 0, MPI_COMM_WORLD, TERROR)
    RETURN
END FUNCTION