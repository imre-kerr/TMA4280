program ex2
	integer, parameter :: n = 4096
	integer :: i, j, seed
	real :: gamma, alpha = 0
	real, dimension(n) :: a, b, x, y
	real, dimension(n,n) :: mat

	character(len=10) :: arg

	call system_clock(seed)
	write(*,*) seed
	call random_seed(seed)

	call getarg(1, arg)
	read(arg, *) gamma

	call random_number(a)
	call random_number(b)
	call random_number(mat)

	do i = 1, n
		x(i) = a(i) + gamma * b(i)
		y(i) = a(i)
		do j = 1, n
			y(i) =  y(i) + mat(j, i) * b(j)
		end do
		alpha = alpha + x(i) * y(i)
	end do

	write(*, *) alpha
end program ex2