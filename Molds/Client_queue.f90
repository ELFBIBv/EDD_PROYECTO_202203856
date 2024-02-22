module linked_list_module
    implicit none
    private

    !client
    type, public :: client
        private
        character(:), allocatable :: name
        integer, allocatable :: img_b=0
        integer, allocatable :: img_s=0
        integer :: steps=0

        type(client), pointer :: next => null()
        type(client), pointer :: prev => null()
    end type client

    !list
    type, public :: linked_list
        private
        type(client), pointer :: head => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: pop
        procedure :: addsteps
    end type linked_list
    
    contains

    subroutine append(self, name, img_b, img_s)
        private
        class(linked_list), intent(inout) :: self
        character(:), allocatable :: name
        integer, intent(inout) :: img_b
        integer, intent(inout) :: img_s
        
        type(client), pointer :: current
        type(client), pointer :: temp

        allocatable(temp)
        temp => client(name, img_b, img_s)
        current => self%head

        if (asociated(current)) then
            do while (asociated(current%next))
                current => current%next
            end do
            current%next => temp
            temp%prev => current
        else
            self%head => temp
        end if
    end subroutine append

    subroutine print(self)
        private
        class(linked_list), intent(in) :: self

        type(client), pointer :: current
        current => self%head
        
        integer :: count = 0
        do while (asociated(current))
            print *,count ,"      ", current%name,"  No. big images:",current%img_b,"  No. small images:",current%img_s
            current => current%next
        end do
    end subroutine print

    subroutine pop(self)
        private
        class(linked_list), intent(inout) :: self

        type(client), pointer :: parka
        parka => self%head
        self%head=>self%head%next
        self%head%prev => none()
        deallocate(parka)
    end subroutine pop
    



