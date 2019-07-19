! Intialize RHS of pdf and the initial solution guess

    
Subroutine Init_2d(start_x, start_y, end_x, end_y, nx, ny)
   
    Use Global_vars
    
    Implicit none
   
    !***Read in variables
    Integer, intent(in) :: start_x, start_y, end_x, end_y, nx, ny
 
    !***Local variables
    Integer :: i, j 
    
    !***Set all values equal to zero
    do j = start_y , end_y 
        do i=start_x, end_x 
            u_new(i,j) = 0.0d0
            u(i,j)     = 0.0d0
            f(i,j)     = 0.0d0
        end do
    end do

    !***Apply x BC South
    if(start_x .eq. 1) then
        do i = 1, end_x
            u_new(i,1) = 1.0d0
            u    (i,1) = 1.0d0
        end do
    end if
    
    !***Apply x BC North 
    if(end_x .eq. nx) then
        do i = start_x, end_x
            u_new(i,1) = 1.0d0
            u    (i,1) = 1.0d0
        end do
    end if
    
    
    !***Apply y BC West 
    if(start_y .eq. 1) then
        do j = 1, end_y
            u_new(1,j) = 1.0d0
            u    (1,j) = 1.0d0
        end do
    end if
    
    !***Apply y BC East
    if(end_y .eq. ny) then
        do j = start_y, end_y
            u_new(1,j) = 1.0d0
            u    (1,j) = 1.0d0
        end do
    end if
    
    Return
    
End subroutine Init_2d 