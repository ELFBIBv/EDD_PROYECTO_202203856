module module_ordinal_user
    implicit none

    type ordinal_user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    ! contains
    ! procedure :: ver_reportes_estructuras
    ! procedure :: navegacion_imgs
    ! procedure :: gestion_imgs
    end type
end module

module module_btree
    use module_ordinal_user
    implicit none

    integer, parameter :: orden =5

    !no mover lo siguiente
    integer, parameter :: MAXI = orden-1
    integer, parameter :: MINI = ceiling((dble(MAXI)-1)/2)
    
    type nodeptr
    type (BTree), pointer :: ptr => null()
    end type nodeptr
    
    type BTree
    type(ordinal_user) :: val(0:MAXI+1)
    integer :: num = 0
    type(nodeptr) :: link(0:MAXI+1)
    contains
    procedure :: insert
    procedure :: returnRoot
    procedure :: traversal
    procedure :: remove
    ! procedure :: search
    procedure :: graph
    end type BTree
    type(BTree), pointer :: root => null()

    contains

    subroutine insert(this,val)
        class(BTree), intent(inout) :: this
        type(ordinal_user), intent(in) :: val
        type(ordinal_user) :: i
        type(BTree), pointer :: child
        allocate(child)
        if (setValue(val, i, root, child)) then
            root => createNode(i, child)
        end if
    end subroutine insert

    function returnRoot(this) result(myRoot)
        class(BTree) :: this
        type(BTree), pointer :: myRoot
        myRoot => root
    end function returnRoot

    recursive function setValue(val, pval, node, child) result(res)
        type(ordinal_user), intent(in) :: val
        type(ordinal_user), intent(inout) :: pval
        type(BTree), pointer, intent(inout) :: node
        type(BTree), pointer, intent(inout) :: child
        type(BTree), pointer :: newnode        
        integer :: pos
        logical :: res
        allocate(newnode)
        if (.not. associated(node)) then            
                pval = val
                child => null()
                res = .true.
                return
        end if
        if (val%DPI < node%val(1)%DPI) then
            pos = 0
        else
            pos = node%num
            do while (val%DPI < node%val(pos)%DPI .and. pos > 1) 
            pos = pos - 1
            end do
            if (val%DPI == node%val(pos)%DPI) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
        end if
        if (setValue(val, pval, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call insertNode(pval, pos, node, child)
            else
                call splitNode(pval, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
                return
            end if
        end if
        res = .false.
    end function setValue

    subroutine insertNode(val, pos, node, child)
        type(ordinal_user), intent(in) :: val
        integer, intent(in) :: pos
        type(BTree), pointer, intent(inout) :: node
        type(BTree), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while (j > pos)
                node%val(j + 1) = node%val(j)
                node%link(j + 1)%ptr => node%link(j)%ptr
                j = j - 1
        end do
        node%val(j + 1) = val
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(val, pval, pos, node, child, newnode)
        type(ordinal_user), intent(in) :: val
        integer,intent(in):: pos
        type(ordinal_user), intent(inout) :: pval
        type(BTree), pointer, intent(inout) :: node,  newnode
        type(BTree), pointer, intent(in) ::  child
        integer :: median, i, j
        if (pos > MINI) then
                median = MINI + 1
        else
                median = MINI
        end if
        if (.not. associated(newnode)) then
            allocate(newnode)
        do i = 0, MAXI
                    newnode%link(i)%ptr => null()
            enddo
        end if
        j = median + 1
        do while (j <= MAXI)
                newnode%val(j - median) = node%val(j)
                newnode%link(j - median)%ptr => node%link(j)%ptr
                j = j + 1
        end do
        node%num = median
        newnode%num = MAXI - median
        if (pos <= MINI) then
                call insertNode(val, pos, node, child)
        else
                call insertNode(val, pos - median, newnode, child)
        end if        
        pval = node%val(node%num)        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(val, child) result(newNode)
        type(ordinal_user), intent(in) :: val
        type(BTree), pointer, intent(in) :: child
        type(BTree), pointer :: newNode
        integer :: i
        allocate(newNode)
        newNode%val(1) = val
        newNode%num = 1
        newNode%link(0)%ptr => root
        newNode%link(1)%ptr => child
        do i = 2, MAXI
                newNode%link(i)%ptr => null()
        end do
    end function createNode

    recursive subroutine traversal(this,myNode)
        class(BTree), intent(in) :: this
        type(BTree), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
                write (*, '(A)', advance='no') ' [ '
                i = 0
                do i = 0, myNode%num
                    ! write (*,'(I15)', advance='no') myNode%val(i)%DPI
                    write (*,'(I3)', advance='no') myNode%val(i)%DPI
                    ! i = i + 1
                end do
                i = 0
                do i = 0, myNode%num
                    call this%traversal(myNode%link(i)%ptr)
                end do
                write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal

    subroutine remove(this,DPI)
        class(BTree), intent(in) :: this
        integer(kind=8), intent(in) :: DPI
        class(BTree), pointer :: root2
        print *, ""
        call deletenode(DPI,root,root2)
        root => root2
    end subroutine remove

    recursive subroutine deletenode(DPI,root1,root2)
        integer(kind=8), intent(in) :: DPI
        type(BTree), pointer ,intent(inout) :: root1
        class(BTree), pointer, intent(inout) :: root2
        integer :: i 
        if (associated(root1)) then
            do i = 0, root1%num
                ! write (*,'(I15)', advance='no') myNode%val(i)%DPI
                print *, "DPI", DPI
                if (DPI /= root1%val(i)%DPI) then
                    print *, "se inserta DPI en root2", root1%val(i)%DPI
                    call root2%insert(root1%val(i))
                else
                    print *, "DPI found"
                end if 
            end do
            i = 0
            do i = 0, root1%num
                call deletenode(DPI,root1%link(i)%ptr,root2)
            end do
        end if
    end subroutine deletenode


    !no terminado
    recursive subroutine graph(this,myNode)
    class(BTree), intent(in) :: this
    type(BTree), pointer, intent(in) :: myNode
    ! integer :: i
    ! if (associated(myNode)) then
    !         write (*, '(A)', advance='no') ' [ '
    !         i = 0
    !         do while (i < myNode%num)
    !             write (*,'(1I3)', advance='no')myNode%val(i+1)
    !             i = i + 1
    !         end do
    !         do i = 0, myNode%num
    !             call this%traversal(myNode%link(i)%ptr)
    !         end do
    !         write (*, '(A)', advance='no') ' ] '
    ! end if
    end subroutine graph

end module module_btree