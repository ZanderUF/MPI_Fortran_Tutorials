! Subroutine to handle to decomposition of the domain
! Returns start_x, start_y, end_x, end_y
   
!    *_ _ _ _ **_ _ _ _ *
!    |        ||        |
!    |        ||        |
!    |  P0    ||  P2    |
!    |        ||        |
!    *_ _ _ _ **_ _ _ _ *
!    *_ _ _ _ **_ _ _ _ *
!    |        ||        |  
!    |  P1    ||  P3    |  
!    |        ||        |  
!    |        ||        |  
!    *_ _ _ _ **_ _ _ _ *  
    
Subroutine Decomp_2d(nx, ny, start_x, start_y, end_x, end_y)

    Use MPI
    Use Global_vars
    
    Implicit None

    !***Read in variables
    Integer, intent(in) :: nx, ny
    Integer, intent(inout) :: start_x, start_y, end_x, end_y
     
    !***Local Variables
    Integer :: nx_local, ny_local
    Integer :: deficit_x, deficit_y
    Integer :: ierr
    Integer :: diff_x, diff_y
    
    nx_local = nx / dimensions(1)   ! dimensions(1) indicates the # of processors in x-direction 
    ny_local = ny / dimensions(2)  
  
    diff_x = nx_local - 1
    diff_y = ny_local - 1
    start_x = virtual_coords(1)*(nx_local)  + 1 
    start_y = virtual_coords(2)*(ny_local)  + 1 
   
    end_x = start_x + diff_x  
    end_y = start_y + diff_y
    
    !if(process_id == 3) then
    !    start_x =  
    !    start_y = 
    !    end_x   = 
    !    end_y   = 
    !end if  
    
    Return
    
End subroutine Decomp_2d
