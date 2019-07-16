!***This program highlights how to solve the poisson PDE using a domain
!   decomposition approach defined on a square mesh.   
    
Program domain_decomposition

    Use MPI    
    Use Global_vars
    
    Implicit None
   
    Integer :: i, j, n
    Integer :: lower_bnd
    Integer :: upper_bnd
    Double precision :: h
    Double precision, allocatable, dimension(:,:) :: u, u_new, f
 
    !***Call MPI Initialization
    
    
    !***Size of global domain
    n = 10
    !***Size of domain - dependent on processor
    lower_bnd = 10/num_proc  
    upper_bnd = 
    
    !***Allocate variables based on individual sizing for each array
    Allocate(u    (n+1, lower_bnd:upper_bnd)  )
    Allocate(u_new(n+1, lower_bnd:upper_bnd)  )
    Allocate(f    (n+1, lower_bnd:upper_bnd)  )
    
      
    
End program domain_decomposition