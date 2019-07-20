!***Should exchange row and column data between ghost cells
    
Subroutine Exchange_2d(row_type, start_x, end_x, start_y, end_y, &
                       nbr_south, nbr_north, nbr_east, nbr_west)

        Use MPI
        Use Global_vars
        
        Implicit none
        
        !***Read in variables
        Integer, intent(in) :: row_type, start_x, end_x, start_y, end_y, &
                               nbr_south, nbr_north, nbr_east, nbr_west
        
        !***Local variables
        Integer :: nx, ny, ierr 
                               
        !***Number of values in a column 
        ny = end_y - start_y 
        !if(process_id == 0) then
        !   print *,'nbr_west: ', nbr_west, 'proc: ',process_id
        !   print *,'nbr_east: ', nbr_east, 'proc: ',process_id
        !end if
       
        !***Send columns West --> East
        !if( nbr_east > 0 ) then
            Call MPI_Sendrecv(u_new(end_x, start_y), ny, MPI_Double_Precision, &
                          nbr_west, 0, &
                          u_new(start_x+1, start_y), ny, MPI_Double_Precision, &
                          nbr_east, 0, comm_2d, MPI_Status_Ignore, ierr)
        !end if
        !***Send columns East --> West
        !if( nbr_west > 0) then
            Call MPI_sendrecv(u_new(end_x, start_y), ny, MPI_Double_Precision, &
                          nbr_west, 0, &
                          u_new(end_x , start_y), ny, MPI_Double_Precision, &
                          nbr_east, 0, comm_2d, MPI_status_Ignore, ierr)
        !end if
                          
        !***Send rows North --> South
        
        
        !***Send rows South --> North

        
End subroutine Exchange_2d