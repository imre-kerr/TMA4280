program ex4_single
	integer :: k
	real, parameter :: s = (4.D0 * datan(1.D0))**2 / 6
	real :: sk
	write (*, "('S = pi^2/6 = 'F7.5)") s
	do k = 4, 24
		sk = s_n(2**k)
		write (*, *)
		write (*, "('Sn for n = 2^'I2' = 'F7.5)") k, sk
		write (*, "('S - Sn = 'F7.5)") s-sk
	end do
end program

real function s_n(n)
	integer :: n, i
	real, dimension(n) :: v
	s_n = 0.0
	!$omp parallel do reduction(+:s_n)
		do i = 1, n
			v(i) = 1.0 / i**2
			s_n = s_n + v(i)
		end do
	!$omp end parallel do
	return
end function