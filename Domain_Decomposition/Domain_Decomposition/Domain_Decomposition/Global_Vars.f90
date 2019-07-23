Module Global_vars
    
    Implicit none
    
    !***
    Integer, dimension(2) :: virtual_coords
    Integer :: process_id, master_id, num_proc
    Integer :: comm_2d ! Integer communicator
    Integer :: ndim    ! Dimension of the problem
    Integer, dimension(2)  :: dimensions  ! keeps track of dimensions of the problem 
    Logical, dimension(2)  :: is_periodic ! tells you if one of the directions is periodic
    Logical :: reorder  ! Tells MPI to reorder and find a good way to assign 
                        ! the process to the elements of the de  
     
    Double precision, allocatable, dimension(:,:) :: u, u_new, f
    
End module Global_vars