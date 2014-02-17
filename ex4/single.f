PROGRAM EX4_SINGLE
    INTEGER*8 :: K
    REAL*8, PARAMETER :: S = (4.D0 * DATAN(1.D0))**2 / 6
    REAL*8 :: SK, S_N
    WRITE (*, "('S = PI^2/6 = 'F7.5)") S

    DO K = 3, 14
        SK = S_N(2**K)
        WRITE (*, *)
        WRITE (*, "('SN FOR N = 2^'I0.2' = 'F7.5)") K, SK
        WRITE (*, "('S - SN = 'ES21.14)") S-SK
    END DO
END PROGRAM

REAL*8 FUNCTION S_N(N)
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