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

    type, public :: historyClients
        type(clientRegister), pointer :: head => null()
        type(clientRegister), pointer :: tail => null()
    contains
        procedure :: addClient
        procedure :: printHistoryClients
    end type historyClients

    contains

    subroutine addClient(this, name, attendedWindow, NoImages, steps)
        class(historyClients), intent(inout) :: this
        character(*), intent(in) :: name
        integer, intent(in) :: attendedWindow
        integer, intent(in) :: NoImages
        integer, intent(in) :: steps

        type(clientRegister), pointer :: newClient, currentClient
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

    subroutine printHistoryClients(this)
        class(historyClients), intent(inout) :: this
        type(clientRegister), pointer :: currentClient
        if (.not.associated(this%head)) then
            print *, "No clients in the history"
            return
        end if
        currentClient => this%head
        print *, "name    attendedWindow    NoImages    steps"
        do while(associated(currentClient))
            write (*,fmt="(1x,a,a20)",advance="no") " ", currentClient%name
            write (*,fmt="(1x,a,i0)",advance="no") " ", currentClient%attendedWindow
            write (*,fmt="(1x,a,i3)",advance="no") " ", currentClient%NoImages
            write (*,fmt="(1x,a,i16)",advance="no") " ", currentClient%steps
            currentClient => currentClient%next
        end do
    end subroutine printHistoryClients

end module
