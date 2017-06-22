module quaternions

  implicit none

  type quaternion
    double precision :: r
    double precision :: i
    double precision :: j
    double precision :: k
  end type quaternion


interface operator(+)
  module procedure qadd
end interface 

interface operator(-)
  module procedure qsub
end interface 

interface operator(*)
  module procedure qmul
end interface 

interface operator(*)
  module procedure qscal
end interface 

contains

function quat(r, i, j, k) result(q)

  implicit none
  double precision, intent(in) :: r, i, j, k
  type(quaternion) :: q

  q%r = r
  q%i = i
  q%j = j
  q%k = k

end function quat

function qconst(c) result(q)

  implicit none
  double precision, intent(in) :: c
  type(quaternion) :: q

  q%r = c
  q%i = c
  q%j = c
  q%k = c

end function qconst

function qadd(x, y) result(q)

  implicit none
  type(quaternion), intent(in) :: x, y
  type(quaternion) :: q

  q%r = x%r + y%r
  q%i = x%i + y%i
  q%j = x%j + y%j
  q%k = x%k + y%k

end function qadd


function qsub(x, y) result(q)

  implicit none
  type(quaternion), intent(in) :: x, y
  type(quaternion) :: q

  q%r = x%r - y%r
  q%i = x%i - y%i
  q%j = x%j - y%j
  q%k = x%k - y%k

end function qsub


function qscal(a, x) result(q)

  implicit none
  type(quaternion), intent(in) :: x
  double precision, intent(in) :: a
  type(quaternion) :: q

  q%r = a * x%r
  q%i = a * x%i
  q%j = a * x%j
  q%k = a * x%k

end function qscal


function qconj(x) result(q)

  implicit none
  type(quaternion), intent(in) :: x
  type(quaternion) :: q

  q%r =  x%r
  q%i = -x%i
  q%j = -x%j
  q%k = -x%k

end function qconj


function qnorm(x) result(res)

  implicit none
  type(quaternion), intent(in) :: x
  double precision :: res

  res = sqrt(x%r * x%r + x%i * x%i + x%j * x%j + x%k * x%k)

end function qnorm


function qnorm2(x) result(res)

  implicit none
  type(quaternion), intent(in) :: x
  double precision :: res

  res = x%r * x%r + x%i * x%i + x%j * x%j + x%k * x%k

end function qnorm2


function qinv(x) result(q)

  implicit none
  type(quaternion), intent(in) :: x
  type(quaternion) :: q

  q = qscal(1.0d0/qnorm2(x) , qconj(x))

end function qinv


function qmul(x, y) result(q)

  implicit none
  type(quaternion), intent(in) :: x, y
  type(quaternion) :: q

  q%r = x%r*y%r - x%i*y%i - x%j*y%j - x%k*y%k
  q%i = x%r*y%i + x%i*y%r + x%j*y%k - x%k*y%j
  q%j = x%r*y%j - x%i*y%k + x%j*y%r + x%k*y%i
  q%k = x%r*y%k + x%i*y%j - x%j*y%i + x%k*y%r

end function qmul


subroutine qsetzero(q)

  implicit none
  type(quaternion) :: q

  q%r = 0.0d0
  q%i = 0.0d0
  q%j = 0.0d0
  q%k = 0.0d0
    
end subroutine qsetzero


subroutine qset(r, i, j, k, q)

  implicit none
  double precision, intent(in) :: r, i, j, k
  type(quaternion) :: q

  q%r = r
  q%i = i
  q%j = j
  q%k = k
    
end subroutine qset


subroutine qprint(q)

  implicit none
  type(quaternion) :: q

  write(*,'(4ES18.9)'), q%r, q%i, q%j, q%k
    
end subroutine qprint


end module quaternions
