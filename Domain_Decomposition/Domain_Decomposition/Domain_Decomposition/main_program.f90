!***This program highlights how to solve the poisson PDE using a domain
!   decomposition approach defined on a square mesh.   

#ifdef normal
Program domain_decomposition

    Use MPI    
    Use Global_vars
    
    Implicit None
   
    Integer :: i, j, n, ierr, nx, ny, k, max_iter
    Double precision :: h
    Integer :: nbr_west, nbr_east, nbr_north, nbr_south
    Integer :: start_x, start_y, end_x, end_y ! Bounds for each processors subdomain
    Integer, dimension(2) :: virtual_coords
    Integer :: row_type 
    !***Call MPI Initialization
    
    Call Setup_MPI(process_id, master_id, num_proc)
    
    !***Size of global domain
    nx = 8  
    ny = nx  ! Square for now
    max_iter = 100 ! max # of iterations to prevent from never finishing a loop
    ndim = 2 ! # of dimensions
    
    !***Setup sizing of each dimensions x,y -
    dimensions(1) = 2 
    dimensions(2) = 2 
    if(process_id == 0) then
        print *,'Dimensions: ', dimensions
        print *,'Global X:   ', nx
        print *,'Global Y:   ', ny
        print *,'Ndim:       ', ndim
    end if
    
    is_periodic(1) = .false. ! Specify if the dimension is periodic
    is_periodic(2) = .false. 
    !***Size of domain - dependent on processor
    
    !***Tells MPI to possible reorder ranks to better overlay onto hardware.
    reorder = .TRUE.
    
    !***Create MPI cartesian grid
    ! This also creates a new communicator - comm_2d
    Call MPI_Cart_Create(MPI_Comm_World, ndim, dimensions, is_periodic, reorder, comm_2d, ierr ) 
   
    !***Get rank of the process within the 'new' communicator comm_2d
    Call MPI_Comm_Rank(comm_2d, process_id, ierr)  ! process_id becomes 'new id'
    
    !***Get the East and West neighbors in addition to the north and south neighbors
    Call MPI_Cart_Shift(comm_2d, 0, 1, nbr_west, nbr_east, ierr) 
    Call MPI_Cart_Shift(comm_2d, 1, 1, nbr_south, nbr_north, ierr)
   
    Call MPI_Cart_Coords(comm_2d, process_id, 2, virtual_coords)

    Call MPI_Barrier(comm_2d, ierr)
    
    !***Computes the decomposition
    Call Decomp_2d(nx, ny, virtual_coords, start_x, start_y, end_x, end_y) 
   
    
    !print *,'Start x: ', start_x, 'proc=',process_id
    !print *,'Start y: ', start_y, 'proc=',process_id
    !print *,'End x:   ', end_x,   'proc=',process_id
    !print *,'End y:   ', end_y,   'proc=',process_id
    !print *,' '
   
    !***Allocate variables based on individual sizing for each array
    Allocate(u    (nx , ny)  )
    Allocate(u_new(nx , ny)  )
    Allocate(f    (nx , ny)  )
    
    u(:,:)     = 0.0d0
    u_new(:,:) = 0.0d0
    f(:,:)     = 0.0d0
    
    !***Initialize RHS u_new and guess 'u'
    Call Init_2d(start_x, start_y, end_x, end_y, nx, ny)

    !***Main calculation loop
    do k = 1, max_iter
    
        !***Exchange boundary data for U
        Call Exchange_2d(u, row_type, start_x, end_x, start_y, end_y, &
                         nbr_south, nbr_north, nbr_east, nbr_west  )
        !***Calculate U using previous
         
    
        !***Calculate U_new
    
    
        !***Compare difference
    
    end do
   
    Call MPI_Type_Free(row_type, ierr) 
    Call MPI_Barrier(comm_2d, ierr)
    Call MPI_Finalize(ierr)
   
End program domain_decomposition
#endif 