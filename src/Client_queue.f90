module Client_queue
    implicit none
    private

    !client
    type, public :: client
        private
        character(:), allocatable :: name
        integer, allocatable :: img_b
        integer, allocatable :: img_s
        integer, allocatable :: steps

        type(client), pointer :: next => null()
        type(client), pointer :: prev => null()
    end type client

    !list
    type, public :: linked_list_clients
        private
        type(client), pointer :: head => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: pop
        procedure :: addsteps
    end type linked_list_clients
    
    contains

    subroutine append(self, name, img_b, img_s)
        class(linked_list_clients), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(in) :: img_b
        integer, intent(in) :: img_s
        
        type(client), pointer :: current
        type(client), pointer :: temp

        allocate(temp)
        temp = client(name=name, img_b=img_b, img_s=img_s, steps=0)
        current => self%head

        if (associated(current)) then
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
            temp%prev => current
        else
            self%head => temp
        end if
    end subroutine append

    subroutine print(self)

        class(linked_list_clients), intent(in) :: self
        integer :: count = 1
        type(client), pointer :: current
        current => self%head
        
        do while (associated(current))
            print *,count ,"      ", current%name,"  No. big images:",current%img_b,"  No. small images:",current%img_s
            current => current%next
            count = count + 1
        end do
    end subroutine print

    subroutine pop(self)
        class(linked_list_clients), intent(inout) :: self

        type(client), pointer :: parka
        parka => self%head
        self%head=>self%head%next
        self%head%prev => null()
        deallocate(parka)
    end subroutine pop
    
    subroutine

end module Client_queue

