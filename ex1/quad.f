PROGRAM QUADRATIC_EQUATION
	IMPLICIT NONE
	REAL :: A, B, C
	REAL :: DISC
	REAL :: X1, X2
	COMPLEX :: X1C, X2C

	WRITE (*,*) "ENTER COEFFICIENTS ""A"", ""B"" AND ""C"":"
	READ (*,*) A, B, C

	DISC = B**2 - 4*A*C
	IF (DISC .GT. 0) THEN
		X1 = (-B + DISC**0.5) / (2*A)
		X2 = (-B - DISC**0.5) / (2*A)
		PRINT *, "SOLUTIONS ARE:", X1, ",", X2
	ELSEIF (DISC .EQ. 0) THEN
		X1 = (-B) / (2*A)
		WRITE (*,*) "ONE SOLUTION:", X1
	ELSE
		X1C = CMPLX((-B)/(2*A), ((-DISC)**0.5/(2*A)))
		X2C = CMPLX((-B)/(2*A), ((-DISC)**0.5/(2*A)))
		WRITE (6,*) "SOLUTIONS ARE:", X1C, ",", X2C
	ENDIF
END PROGRAM