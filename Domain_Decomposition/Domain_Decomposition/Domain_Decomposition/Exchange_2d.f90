!***Should exchange row and column data between ghost cells
    
Subroutine Exchange_2d(row_type, start_x, end_x, start_y, end_y)
        
        Use MPI
        Use Global_vars
        
        Implicit none
        
        !***Read in variables
        Integer, intent(in) :: row_type, start_x, end_x, start_y, end_y
        
        !***Local variables
        Integer :: nx, ny, ierr 
        Integer :: nbr_west, nbr_east, nbr_north, nbr_south
        Integer :: s_local_x, s_local_y
        
        !***Number of values in a column 
        ny = end_y - start_y -1 
        !***Get the East and West neighbors in addition to the north and south neighbors
        Call MPI_Cart_Shift(comm_2d, 0, 1, nbr_west,  nbr_east,  ierr) 
        Call MPI_Cart_Shift(comm_2d, 1, 1, nbr_south, nbr_north, ierr)

        if(nbr_west  < 0) nbr_west  = MPI_Proc_Null
        if(nbr_east  < 0) nbr_east  = MPI_Proc_Null
        if(nbr_north < 0) nbr_north = MPI_Proc_Null
        if(nbr_south < 0) nbr_south = MPI_Proc_Null
       
        !***Send columns West --> East
        s_local_x = start_x + start_x*virtual_coords(1)
        s_local_y = start_y + start_y*virtual_coords(2)
         
        !if(process_id == 0 .or. process_id == 2) then
            Call MPI_Sendrecv(u_new(start_y+1, end_x),     ny, MPI_Double_Precision, &
                              nbr_east, 0, &
                              u_new(start_y+1, start_x+1), ny, MPI_Double_Precision, &
                              nbr_west, 0, comm_2d, MPI_Status_Ignore, ierr)
        !end if
        
        !***Send columns East --> West
        !if( nbr_west .ne. MPI_Proc_Null) then
            Call MPI_sendrecv(u_new(start_y+1, start_x), ny, MPI_Double_Precision, &
                          nbr_west, 1, &
                          u_new(start_y+1 , end_x+1), ny, MPI_Double_Precision, &
                          nbr_east, 1, comm_2d, MPI_status_Ignore, ierr)
        !end if
                          
        !***Send rows North --> South
        !   Call MPI_sendrecv(u_new(end_y,     start_x-1), 1, row_type, nbr_north, 1, &
        !                     u_new(start_y+1, start_x-1), 1, row_type, nbr_south, 1, &
        !                     comm_2d, MPI_Status_Ignore, ierr)
        
        !***Send rows South --> North
        !    Call MPI_sendrecv(u_new(start_y,  start_x-1), 1, row_type, nbr_south, 1, &
        !                      u_new(end_y-1, start_x-1), 1, row_type,  nbr_north, 1, &
        !                     comm_2d, MPI_Status_Ignore, ierr)
        
End subroutine Exchange_2d