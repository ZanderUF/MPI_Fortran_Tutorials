!***Calculates u_new based on finite difference scheme
!***Input lower_bnd, upper_bnd, f, u_new
!***Output: u_new
Subroutine Calculate_u_new(lower_bnd, upper_bnd,h, u, f, u_new)

    Implicit None
    
    !***Read in variables
    Integer, intent(in) :: lower_bnd
    Integer, intent(in) :: upper_bnd
    Double precision, intent(in) :: h
    Double precision, intent(in)    :: u(:,:)
    Double precision, intent(in)    :: f(:,:)
    Double precision, intent(inout) :: u_new(:,:)
    
    !***Local variables
    Integer :: i, j

    !***
    
        do j=lower_bnd, upper_bnd 
            do i=lower_bnd, upper_bnd 
                u_new(i,j) = 0.25*( u(i-1, j) + u(i, j+1) + u(i, j-1) + u(i+1, j) - &
                             h*h *f(i,j) )
            end do
        end do
    
End subroutine Calculate_u_new
