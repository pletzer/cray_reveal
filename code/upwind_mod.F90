module upwind_mod

    implicit none
    
    integer, parameter :: r8 = selected_real_kind(12, 100)
    
    type upwind_type
        ! number of space dimensions
        integer :: ndims
        
        ! total number of cells in the domain
        integer :: ntot
        
        ! number of cells in the ndims directions
        integer, allocatable :: numCells(:)
        
        ! upwind direction
        integer, allocatable :: upDirection(:)
        
        ! cell sizes in the ndims directions
        real(r8), allocatable :: deltas(:)
        
        ! velocity
        real(r8), allocatable :: v(:)
        
        ! domain lengths
        real(r8), allocatable :: lengths(:)
        
        ! field as a flat array
        real(r8), allocatable :: f(:)
        
        ! product of the dimensions, used to switch back and forth 
        ! between the flat index and the multi-index representations
        integer, allocatable :: dimProd(:)
    end type

contains

    ! Constructor
    ! @param velocity velocity field (constant)
    ! @param lengths domain lengths
    ! @param numCells number of cells in the x, y, ... directions
    subroutine upwind_new(obj, velocity, lengths, numCells)
    
        type(upwind_type) :: obj
        real(r8), intent(in) :: velocity(:)
        real(r8), intent(in) :: lengths(:)
        integer, intent(in) :: numCells(:)
        
        integer :: j
        
        ! f95 will allocate and copy
        obj % ndims = size(velocity)
        
        allocate(obj % upDirection(obj % ndims))
        allocate(obj % deltas(obj % ndims))
        allocate(obj % v(obj % ndims))
        allocate(obj % lengths(obj % ndims))
        allocate(obj % numCells(obj % ndims))
        allocate(obj % dimProd(obj % ndims))
        
        obj % v = velocity
        obj % lengths = lengths
        obj % numCells = numCells

        ! compute the total number of cells and other stuff
        obj % ntot = 1       
        do j = 1, obj % ndims
            obj % upDirection(j) = -1
            if (velocity(j) < 0.) then
                obj % upDirection(j) = 1
            endif
            obj % deltas(j) = lengths(j) / numCells(j)
            obj % ntot = obj % ntot * numCells(j)
        enddo
        
        obj % dimProd(obj % ndims) = 1
        do j = obj % ndims - 1, 1, -1
            obj % dimProd(j) =  obj % dimProd(j + 1) * obj % numCells(j + 1)
        enddo
        
        allocate(obj % f(obj % ntot))
        
        ! initialize the field, zero everywhere except for the 
        ! low corner cell where the field is one
        obj % f = 0
        obj % f(1) = 1

    end subroutine 
    
    ! Destructor
    subroutine upwind_del(obj)
    
        type(upwind_type) :: obj
        
        deallocate(obj % v)
        deallocate(obj % lengths)
        deallocate(obj % numCells)    
        deallocate(obj % upDirection)
        deallocate(obj % deltas)
        deallocate(obj % dimProd)
        deallocate(obj % f)
        
    end subroutine
    
    ! Advance by one time step
    ! @param deltaTime time step
    subroutine upwind_advect(obj, deltaTime)
    
        type(upwind_type) :: obj
        real(r8), intent(in) :: deltaTime

        real(r8), allocatable :: oldF(:)
        integer :: i, j, oldIndex, upI
        integer :: inds(obj % ndims)
        
        ! allocate and copy the field
        allocate(oldF(obj % ntot))
        oldF = obj % f

        ! iterate over the cells
        do i = 1, obj % ntot

            ! compute the index set of this cell
            call upwind_getIndexSet(obj, i, inds)

            do j = 1, obj % ndims
                
                ! cache the cell index
                oldIndex = inds(j)
                
                ! increment the cell index
                inds(j) = inds(j) + obj % upDirection(j)
                
                ! apply periodic BCs
                inds(j) = modulo(inds(j) + obj % numCells(j) - 1, obj % numCells(j)) + 1
                  
                ! compute the new flat index 
                call upwind_getFlatIndex(obj, inds, upI)
                    
                ! update the field
                obj % f(i) = obj % f(i) - &
              &   deltaTime*obj % v(j)*obj % upDirection(j)*(oldF(upI) - oldF(i))/obj % deltas(j)
                    
                ! reset the index
                inds(j) = oldIndex
           enddo
        enddo

    end subroutine

    subroutine upwind_saveVTK(obj, filename)
        type(upwind_type) :: obj
        character(len=*), intent(in) :: filename
        
        integer iunit, i
        
        ! f2008
        !open(newunit = iunit, file = filename, status = 'unknown')
        iunit = 10
        ! f95
        open(unit = iunit, file = filename, status = 'unknown')
        write(iunit, '(a)') '# vtk DataFile Version 2.0'
        write(iunit, '(a)') 'upwind.f90'
        write(iunit, '(a)') 'ASCII'
        write(iunit, '(a)') 'DATASET RECTILINEAR_GRID'

        ! in VTK the first dimension varies fastest so need 
        ! to invert the order of the dimensions
        if (obj % ndims > 2) then
            write(iunit, '(a, i10, i10, i10)') 'DIMENSIONS ', &
             & obj % numCells(3) + 1, obj % numCells(2) + 1, obj % numCells(1) + 1
        else
            if (obj % ndims > 1) then
                write(iunit, '(a, i10, i10)') 'DIMENSIONS 1', &
             & obj % numCells(2) + 1, obj % numCells(1) + 1
            else
                write(iunit, '(a, i10)') 'DIMENSIONS 1 1', obj % numCells(1) + 1
            endif 
        endif 
        
        write(iunit, '(a, i10, a)') 'X_COORDINATES ', obj % numCells(1) + 1, ' double'
        do i = 1, obj % numCells(1) + 1
            write(iunit, '(e20.7)') 0.0 + obj % deltas(1) * (i - 1)
        enddo

        write(iunit, *) 
        if (obj % ndims > 1) then
            write(iunit, '(a, i10, a)') 'Y_COORDINATES ', obj % numCells(2) + 1, ' double'
            do i = 1, obj % numCells(2) + 1
                write(iunit, '(e20.7)') 0.0 + obj % deltas(2) * (i - 1)
            enddo      
        else
            write(iunit, '(a)') 'Y_COORDINATES 1 double'
            write(iunit, '(a)') '0.0'
        endif

        write(iunit, *) 
        if (obj % ndims > 2) then
            write(iunit, '(a, i10, a)') 'Z_COORDINATES ', obj % numCells(3) + 1, ' double'
            do i = 1, obj % numCells(3) + 1
                write(iunit, '(e20.7)') 0.0 + obj % deltas(3) * (i - 1)
            enddo      
        else
            write(iunit, '(a)') 'Z_COORDINATES 1 double'
            write(iunit, '(a)') '0.0'
        endif

        write(iunit, '(a, i20)') 'CELL_DATA ', obj % ntot
        write(iunit, '(a)') 'SCALARS f double 1'
        write(iunit, '(a)') 'LOOKUP_TABLE default'
        do i = 1, obj % ntot
            write(iunit, '(e20.7)') obj % f(i)
        enddo
    
        close(iunit)
    
    end subroutine

    subroutine upwind_print(obj)
            type(upwind_type) :: obj
        
        integer :: i
        
        do i = 1, obj % ntot
            write(*, '(a, i10, a, e20.14)') 'i = ', i, ' f = ',  obj % f(i)
        enddo
 
    end subroutine

    subroutine upwind_getIndexSet(obj, flatIndex, res) 
        type(upwind_type) :: obj
        integer, intent(in) :: flatIndex
        integer, intent(out) :: res(:)
    
        integer :: i
        do i = 1, obj % ndims
            res(i) = mod((flatIndex - 1)/obj % dimProd(i), obj % numCells(i)) + 1
        enddo
    end subroutine
    
    subroutine upwind_getFlatIndex(obj, inds, res) 
        type(upwind_type) :: obj
        integer, intent(in) :: inds(:)
        integer, intent(out) :: res
        
        res = dot_product(obj % dimProd, inds - 1) + 1
        
    end subroutine

end module

