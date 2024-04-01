module module_Queue
    implicit none
    type :: node
        integer :: id
        integer(kind=8) ::id_8
        type(node), pointer :: next => null()
    end type node

    type :: queue
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
        contains
        procedure :: enqueue
        procedure :: enqueue_8
        procedure :: dequeue
        procedure :: dequeue_8
        procedure :: isEmpty
        procedure :: printQueue
        procedure :: printQueue_8
        procedure :: cleanQueue
        procedure :: getQueueSize
    end type queue

    contains
    subroutine enqueue(this, id)
        class(queue), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: tmp
        allocate(tmp)
        tmp%id = id
        if (.not. associated(this%head)) then
            this%head => tmp
            this%tail => tmp
        else
            this%tail%next => tmp
            this%tail => tmp
        end if
    end subroutine enqueue

    subroutine enqueue_8(this, id)
        class(queue), intent(inout) :: this
        integer(kind=8), intent(in) :: id
        type(node), pointer :: tmp
        allocate(tmp)
        tmp%id_8 = id
        if (.not. associated(this%head)) then
            this%head => tmp
            this%tail => tmp
        else
            this%tail%next => tmp
            this%tail => tmp
        end if
    end subroutine enqueue_8

    function dequeue(this) result(id)
        class(queue), intent(inout) :: this
        type(node), pointer :: tmp
        integer :: id
        if (associated(this%head)) then
            id = this%head%id
            tmp => this%head
            this%head => this%head%next
            deallocate(tmp)
        end if
    end function dequeue

    function dequeue_8(this) result(id)
        class(queue), intent(inout) :: this
        type(node), pointer :: tmp
        integer(kind=8) :: id
        if (associated(this%head)) then
            id = this%head%id_8
            tmp => this%head
            this%head => this%head%next
            deallocate(tmp)
        end if
    end function dequeue_8

    function isEmpty(this) result(res)
        class(queue), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function isEmpty

    subroutine printQueue(this)
        class(queue), intent(in) :: this
        type(node), pointer :: tmp
        tmp => this%head
        do while (associated(tmp))
            write (*, '(A,I3)', advance='no') " ",tmp%id
            tmp => tmp%next
        end do
        print *, ""
    end subroutine printQueue

    subroutine printQueue_8(this)
        class(queue), intent(in) :: this
        type(node), pointer :: tmp
        tmp => this%head
        do while (associated(tmp))
            write (*, '(A,I3)', advance='no') " ",tmp%id_8
            tmp => tmp%next
        end do
        print *, ""
    end subroutine printQueue_8

    subroutine cleanQueue(this)
        class(queue), intent(inout) :: this
        type(node), pointer :: tmp
        do while (associated(this%head))
            tmp => this%head
            this%head => this%head%next
            deallocate(tmp)
        end do
    end subroutine cleanQueue

    function getQueueSize(this) result(size)
        class(queue), intent(in) :: this
        type(node), pointer :: tmp
        integer :: size
        size = 0
        tmp => this%head
        do while (associated(tmp))
            size = size + 1
            tmp => tmp%next
        end do
    end function getQueueSize
end module module_Queue