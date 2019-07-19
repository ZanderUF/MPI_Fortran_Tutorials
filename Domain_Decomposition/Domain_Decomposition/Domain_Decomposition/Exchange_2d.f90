!***Should exchange row and column data between ghost cells
    
Subroutine Exchange_2d(u_new, row_type, start_x, end_x, start_y, end_y, &
                       nbr_south, nbr_north, nbr_east, nbr_west)

        Use MPI
                       
        Implicit none
        
        !***Read in variables
        Double precision, intent(inout) :: u_new(:,:)
        Integer, intent(in) :: row_type, start_x, end_x, start_y, end_y, &
                               nbr_south, nbr_north, nbr_east, nbr_west
        
        !***Local variables
           
        

End subroutine Exchange_2d