module windowsModule
    implicit none
    private
    use Client_queue

    ! window
    type, public :: window
        private
        integer :: windowsNumber
        Logical :: isbusy = .false.
        integer :: stepsClientneed = 0
        type(client), allocatable :: actualClient
        type(ClientQueue), allocatable :: HistoryClients
        type(window), pointer :: next => null()
    end type window

    ! window list
    type, public :: window_linked_list
        private
        integer :: windowsNumber = 1
        type(window), pointer :: first => null()
        contains
        procedure :: checkOutWindows
        procedure :: addWindow
        procedure :: addClientInWindow
        procedure :: printWindows
    end type window_linked_list

    contains

    !checkOutWindows
    subroutine checkOutWindows(self)
        class(window_linked_list), intent(inout) :: self
        type(window), pointer :: current

        current => self%first
        do while(associated(current))
            if(current%isbusy) then
                current%actualClient%steps = current%actualClient%steps + 1
                if(current%stepsClientneed == current%actualClient%steps) then
                    current%isbusy = .false.
                    current%HistoryClients%append(current%actualClient)
                end if
            end if
            current => current%next
        end do
    end subroutine checkOutWindows


    !addWindow
    subroutine addWindow(self)
        class(window_linked_list), intent(inout) :: self
        type(window), pointer :: temp
        type(window), pointer :: current

        allocate(temp)

        temp = window(windowsNumber = self%windowsNumber,HistoryClients = ClientQueue())
        self%windowsNumber = self%windowsNumber + 1

        !agrega la ventana a la lista
        current => self%first
        if(associated(current)) then
            do while(associated(current%next))
                current => current%next
            end do
            current%next => temp
        else
            self%first => temp
        end if
    end subroutine addWindow

    !addClientInWin
    subroutine addClientInWindow(self,cliente)
        class(window_linked_list), intent(inout) :: self
        type(client), pointer, intent(in) :: cliente
        type(window), pointer :: current
        logical :: found = .false.

        current => self%first
        do while(associated(current))
            if(.not. current%isbusy) then
                current
                found = .true.
                exit
            end if
            current => current%next
        end do

        if (found) then
            current%isbusy = .true.
            current%stepsClientneed = cliente%steps+cliente%img_s+cliente%img_b
            current%actualClient = cliente
        else
            print *, "No hay ventanas disponibles"
        end if

    end subroutine addClientInWindow

    !printWindows
    subroutine printWindows(self)
        class(window_linked_list), intent(inout) :: self
        type(window), pointer :: current

        current => self%first
        do while(associated(current))
            print *, current%windowsNumber, "state: ", current%isbusy, "steps: ", current%stepsClientneed
            current => current%next
        end do
    end subroutine printWindows
end module
