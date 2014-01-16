PROGRAM FIRST
	IMPLICIT NONE
	INTEGER, PARAMETER :: N = 5
	REAL, PARAMETER :: PI = 3.1415926535
	REAL, DIMENSION(N) :: rs
	REAL :: a
	INTEGER :: i


	DO i=1,n
		PRINT *, 'Enter a radius, you cunt!'
		READ *, rs(i)
	END DO
	DO i=1,n
		IF (rs(i).LT.0) THEN
			CYCLE
		ENDIF
		a = PI*rs(i)**2
		PRINT *, 'The fucking area is', a 
	END DO
END PROGRAM FIRST