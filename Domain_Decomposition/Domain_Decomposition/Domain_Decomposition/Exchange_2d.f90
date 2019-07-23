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
        
        !***Number of values in a column 
        ny = end_y - start_y - 1 
        !***Get the East and West neighbors in addition to the north and south neighbors
        Call MPI_Cart_Shift(comm_2d, 0, 1, nbr_west,  nbr_east,  ierr) 
        Call MPI_Cart_Shift(comm_2d, 1, 1, nbr_south, nbr_north, ierr)

        if(nbr_west  < 0) nbr_west  = MPI_Proc_Null
        if(nbr_east  < 0) nbr_east  = MPI_Proc_Null
        if(nbr_north < 0) nbr_north = MPI_Proc_Null
        if(nbr_south < 0) nbr_south = MPI_Proc_Null
       
        print *,'proc', process_id,'nbr_west',nbr_west, 'nbr_east',nbr_east
        print *,'proc ',process_id, u_new(start_x:end_x, start_y) 
        !***Send columns West --> East
        !if(nbr_east .ne. MPI_Proc_Null) then
            Call MPI_Sendrecv(u_new(start_x, end_y), ny, MPI_Double_Precision, &
                              nbr_west, 0, &
                              u_new(start_x, start_y-1), ny, MPI_Double_Precision, &
                              nbr_east, 0, comm_2d, MPI_Status_Ignore, ierr)
        !end if
        !***Send columns East --> West
        !if( nbr_west .ne. MPI_Proc_Null) then
        !    Call MPI_sendrecv(u_new(start_x, start_y), ny, MPI_Double_Precision, &
        !                  nbr_west, 1, &
        !                  u_new(end_x+1 , end_y+1), ny, MPI_Double_Precision, &
        !                  nbr_east, 1, comm_2d, MPI_status_Ignore, ierr)
        !end if
                          
        !***Send rows North --> South
        !   Call MPI_sendrecv(u_new(start_x,start_y),   1, row_type, nbr_south, 1, &
        !                     u_new(start_x,start_y-1), 1, row_type, nbr_north, 1, &
        !                     comm_2d, MPI_Status_Ignore, ierr)
        
        !***Send rows South --> North

        
End subroutine Exchange_2d