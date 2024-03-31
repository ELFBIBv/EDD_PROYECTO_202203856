module module_Queue
    implicit none
    type :: node
        integer :: id
        type(node), pointer :: next => null()
    end type node

    type :: queue
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
        contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: isEmpty
        procedure :: printQueue
        procedure :: cleanQueue
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

    subroutine cleanQueue(this)
        class(queue), intent(inout) :: this
        type(node), pointer :: tmp
        do while (associated(this%head))
            tmp => this%head
            this%head => this%head%next
            deallocate(tmp)
        end do
    end subroutine cleanQueue
end module module_Queue