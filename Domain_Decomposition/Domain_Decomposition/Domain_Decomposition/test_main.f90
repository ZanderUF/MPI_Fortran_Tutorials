
#ifdef testing
! subroutine to test various things within the domain decomposition program
Program test_harness

    Use unit_test
    Use MPI
    Use Global_vars
    
    Implicit None
    
    !***Local Variables
    Type(test_suite_type) :: test_suite_domain_decomp    
    Type(test_suite_type) :: test_suite_mpi_exchange 
    
    Integer :: nx, ny 
    Integer, dimension(2) :: virtual_coords
    Integer :: row_type, start_x, end_x, start_y, end_y, &
               nbr_south, nbr_north, nbr_east, nbr_west 
    Integer :: expected_size_row_vec, size_row_type
    Integer :: ierr 
    
    !***Test 2d decomposition
    Call test_case_create('2D Domain Decomposition Routine', test_suite_domain_decomp)
    nx = 8 
    ny = 8
    ndim = 2
    is_periodic(1) = .false.
    is_periodic(2) = .false.
    dimensions(1) = 2
    dimensions(2) = 2
    reorder = .false.
    
    Call Setup_MPI !***Need to setup MPi environment
    
    Call MPI_Cart_Create(MPI_Comm_World, ndim, dimensions, is_periodic, reorder, comm_2d, ierr )
    
    !***Get rank of the process within the 'new' communicator comm_2d
    Call MPI_Comm_Rank(comm_2d, process_id, ierr)  ! process_id becomes 'new id'
    
    Call MPI_Cart_Coords(comm_2d, process_id, 2, virtual_coords)

    Call Decomp_2d(nx, ny, virtual_coords, start_x, start_y, end_x, end_y) 
   
    !***Test return values from decomp_2d
    if( virtual_coords(1) == 0 .and. virtual_coords(2) == 0 ) then
        call assert_equal(start_x, 1, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_x,   5, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(start_y, 1, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_y,   5, __FILE__, __LINE__, test_suite_domain_decomp)
    elseif (virtual_coords(1) == 0 .and. virtual_coords(2) == 1 ) then
        call assert_equal(start_x, 1, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_x,   5, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(start_y, 4, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_y,   8, __FILE__, __LINE__, test_suite_domain_decomp)
    elseif (virtual_coords(1) == 1 .and. virtual_coords(2) == 0 ) then
        call assert_equal(start_x, 4, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_x,   8, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(start_y, 1, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_y,   5, __FILE__, __LINE__, test_suite_domain_decomp)
    elseif (virtual_coords(1) == 1 .and. virtual_coords(2) == 1 ) then
        call assert_equal(start_x, 4, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_x,   8, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(start_y, 4, __FILE__, __LINE__, test_suite_domain_decomp)
        call assert_equal(end_y,   8, __FILE__, __LINE__, test_suite_domain_decomp)
    end if
   
    if(process_id == master_id) then
        Call test_suite_report(test_suite_domain_decomp)
        Call test_suite_final(test_suite_domain_decomp)
    end if
    !*********************************************************************************** 
    
    !***Test MPI Exchange routine
    Call test_case_create('2D MPI Exchange Routine', test_suite_mpi_exchange)

    Call Create_Row_Data_Type(row_type, start_x, end_x, start_y, end_y)
    !***Test if the row data type was setup correctly
    expected_size_row_vec = (end_x - start_x + 1)*sizeof(1.0d0) 
    Call MPI_Type_Size(row_type, size_row_type, ierr) !***Get size in bytes of the data type we just created
    
    Call assert_equal(expected_size_row_vec, size_row_type, __FILE__, __LINE__, test_suite_mpi_exchange)
    
    !***Get the East and West neighbors in addition to the north and south neighbors
    Call MPI_Cart_Shift(comm_2d, 0, 1, nbr_west, nbr_east, ierr) 
    Call MPI_Cart_Shift(comm_2d, 1, 1, nbr_south, nbr_north, ierr)

    Call Exchange_2d(u_new, row_type, start_x, end_x, start_y, end_y, &
               nbr_south, nbr_north, nbr_east, nbr_west ) 
           

    if(process_id == master_id) then
        !***Finalize testig and report to screen the results
        Call test_suite_report(test_suite_mpi_exchange)
        Call test_suite_final(test_suite_mpi_exchange)
   end if
   
End Program test_harness
#endif