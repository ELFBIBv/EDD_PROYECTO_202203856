module module_linked_list
    implicit none
    type :: node
        integer :: id
        type(node), pointer :: next
    end type node

    type :: linked_list
        type(node), pointer :: head
        type(node), pointer :: tail
        contains
        procedure :: add_node
        procedure :: delete_node
        procedure :: print_list
    end type linked_list

    contains
    subroutine add_node(this, id)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: new_node
        allocate(new_node)
        new_node%id = id
        new_node%next => null()
        if(associated(this%head)) then
            this%tail%next => new_node
            this%tail => new_node
        else
            this%head => new_node
            this%tail => new_node
        end if
    end subroutine add_node

    subroutine delete_node(this, id)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: current_node
        type(node), pointer :: previous_node
        current_node => this%head
        previous_node => null()
        do while(associated(current_node))
            if(current_node%id == id) then
                if(associated(previous_node)) then
                    previous_node%next => current_node%next
                else
                    this%head => current_node%next
                end if
                deallocate(current_node)
                return
            end if
            previous_node => current_node
            current_node => current_node%next
        end do
    end subroutine delete_node

    subroutine print_list(this)
        class(linked_list), intent(in) :: this
        type(node), pointer :: current_node
        current_node => this%head
        do while(associated(current_node))
            print*, current_node%id
            current_node => current_node%next
        end do
    end subroutine print_list
end module module_linked_list