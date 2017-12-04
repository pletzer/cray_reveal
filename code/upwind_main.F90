! Converts a string into an integer
subroutine str2int(str, i, stat)
    implicit none
    ! Arguments
    character(len=*), intent(in) :: str
    integer, intent(out)         :: i
    integer, intent(out)         :: stat

    read(str,*,iostat=stat) i
end subroutine str2int

program main

    use upwind_mod
    
    implicit none

    integer, parameter :: ndims = 3
    
    integer :: argc, numCells(ndims), n, ier, numTimeSteps, i, j, & 
   &           numThreads, maxNumThreads, threadId
    logical :: doVtk
#ifdef HAVE_OPENMP
    integer :: omp_get_num_threads, omp_get_max_threads, omp_get_thread_num
#endif
    character(len=32) :: argv
    real(r8) :: velocity(ndims)
    real(r8) :: lengths(ndims)
    real(r8) :: courant, dt, dx, val, chksum
    type(upwind_type) :: up
    
    numThreads = 1
    maxNumThreads = 1
    threadId = 0
#ifdef HAVE_OPENMP    
    numThreads = omp_get_num_threads()
    maxNumThreads = omp_get_max_threads()
    threadId = omp_get_thread_num()
    if (threadId == 0) then
        write(*,'(a)') 'Running with OpenMP enabled'
    endif
#endif
    if (threadId == 0) then 
        write(*,'(a, i10, a, i10)') 'number of threads: ', numThreads, &
          & ' max number of threads: ', maxNumThreads
    endif

    numCells = -1
    doVtk = .FALSE.
    
    ! default number of steps
    numTimeSteps = 100  
    argc = 0
    do 
        call get_command_argument(argc, argv)
        if (len_trim(argv) == 0) exit
        call str2int(argv, n, ier)
        if (argc == 1) then
            numCells = n
        else if (argc == 2) then
            numTimeSteps = n
        else if (argc == 3 .and. argv == 'vtk') then
            doVtk = .TRUE.
        endif
        argc = argc + 1
    enddo

    if (argc < 2) then
        stop 'must specify number of cells in each direction.'
    endif

    write(*, '(a)', advance='no') 'number of cells: '
    do i = 1, ndims
        write(*, '(i10, a)', advance='no') numCells(i), ' '
    enddo
    write(*, *) ' ' ! new line
    write(*, '(a,i10)') 'number of time steps: ', numTimeSteps
    
    ! velocity field
    velocity = 1
    
    ! domain lengths
    lengths = 1

    ! compute time step from Courant's condition 
    courant = 0.1_r8
    dt = huge(1.0_r8)
    do j = 1, ndims
        dx = lengths(j) / real(numCells(j), r8)
        val = courant * dx / velocity(j)
        dt = min(val, dt)
    enddo

    ! instantiate up
    call upwind_new(up, velocity, lengths, numCells)
    
    ! call upwind_saveVTK(up, 'up0.vtk')
    
    ! advance 
    do i = 1, numTimeSteps
        call upwind_advect(up, dt)
    enddo
    
    ! call upwind_print(up)
    write(*,'(a, f15.9)') 'check sum: ', sum(up % f)

    if (doVtk) then 
        call upwind_saveVTK(up, 'up.vtk')
    endif
    
    ! clean up
    call upwind_del(up)

end program
