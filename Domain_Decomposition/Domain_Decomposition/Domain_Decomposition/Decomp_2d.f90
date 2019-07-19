! Subroutine to handle to decomposition of the domain
! Returns start_x, start_y, end_x, end_y
    
Subroutine Decomp_2d(nx, ny, virtual_coords, start_x, start_y, end_x, end_y)

    Use MPI
    Use Global_vars
    
    Implicit None

    !***Read in variables
    Integer, intent(in) :: nx, ny
    Integer, intent(in) :: virtual_coords(2)
    Integer, intent(inout) :: start_x, start_y, end_x, end_y
     
    !***Local Variables
    Integer :: nx_local, ny_local
    Integer :: deficit_x, deficit_y
    Integer :: ierr
    
    nx_local = nx / dimensions(1) ! dimensions(1) indicates the # of processors in x-direction 
    ny_local = ny / dimensions(2)  
   
    start_x = virtual_coords(1)*nx_local - 1*virtual_coords(1) + 1
    start_y = virtual_coords(2)*ny_local - 1*virtual_coords(2) + 1
   
    end_x = start_x + nx_local 
    end_y = start_y + ny_local  

    Return
    
End subroutine Decomp_2d
