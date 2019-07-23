!***Create MPI data type for passing rows of ghost cells
    
Subroutine Create_Row_Data_Type(new_data_type, start_x, end_x, start_y, end_y, ny) 

    Use MPI

    Implicit none
  
    !***Read in variables
    Integer, intent(inout) :: new_data_type
    Integer, intent(in) :: end_x, start_x, end_y, start_y, ny
     
    !***Local variables
    Integer :: ierr
    
    !***Create MPI data type to get rows of the 2d matrix
    Call MPI_Type_Vector(end_x - start_x+1 , 1, ny, MPI_Double_Precision, &
                         new_data_type, ierr)
    
    Call MPI_Type_Commit(new_data_type, ierr)

    
End subroutine Create_Row_Data_Type