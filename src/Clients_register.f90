module clientRegisterModule
    implicit none
    private

    type, public :: clientRegister
        character(:), allocatable :: name
        integer, allocatable :: attendedWindow
        integer, allocatable :: NoImages
        integer, allocatable :: steps

        type(clientRegister), pointer :: next => null()
    end type clientRegister

    type, public :: HistoryClients
        type(clientRegister), pointer :: head => null()
        type(clientRegister), pointer :: tail => null()
    contains
        procedure :: addClient
    end type HistoryClients

    contains

    subroutine addClient(this, name, attendedWindow, NoImages, steps)
        class(HistoryClients), intent(inout) :: this
        character(*), intent(in) :: name
        integer, intent(in) :: attendedWindow
        integer, intent(in) :: NoImages
        integer, intent(in) :: steps

        type(clientRegister), pointer :: newClient
        type(clientRegister), pointer :: currentClient

        allocate(newClient)

        newClient = clientRegister(name, attendedWindow, NoImages, steps)

        if(associated(this%head)) then
            currentClient => this%head
            do while(associated(currentClient%next))
                currentClient => currentClient%next
            end do
            currentClient%next => newClient
        else
            this%head => newClient
            this%tail => newClient
        end if
    end subroutine addClient

end module
