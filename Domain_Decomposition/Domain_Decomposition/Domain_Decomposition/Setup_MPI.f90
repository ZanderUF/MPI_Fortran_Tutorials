Subroutine Setup_MPI
    
    Use MPI
    Use Global_vars
    
    Implicit None
    
    !***Local
    Integer :: ierr
    
    Call MPI_Init(ierr)
    
    !***Determine the rank of the calling process in the communicator
    Call MPI_COMM_RANK(mpi_comm_world, process_id, ierr)
    !***Get total number of processors available
    Call MPI_COMM_SIZE(mpi_comm_world,num_proc, ierr) 
    
End subroutine Setup_MPI