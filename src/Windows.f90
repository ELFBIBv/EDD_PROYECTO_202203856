module windowsModule 
    use clientQueueModule
    implicit none
    private

    ! window
    type, public :: window
        integer :: windowsNumber
        Logical :: isbusy = .false.
        integer :: stepsClientneed = 0
        type(client), allocatable :: actualClient
        type(ClientQueue), allocatable :: historyClients
        type(window), pointer :: next => null()
    end type window

    ! window list
    type, public :: window_linked_list
        integer :: windowsNumber = 1
        type(window), pointer :: first => null()
        contains
        procedure :: addWindow
        procedure :: checkOutWindows
        procedure :: addClientInWindow
        procedure :: printWindows
    end type window_linked_list

    contains
    
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
    
    !checkOutWindows
    subroutine checkOutWindows(self)
        class(window_linked_list), intent(inout) :: self
        type(window), pointer :: current
        type(client), pointer :: cliente

        current => self%first
        do while(associated(current))
            if(current%isbusy) then
                current%actualClient%steps = current%actualClient%steps + 1
                if(current%stepsClientneed == current%actualClient%steps) then
                    current%historyClients%append(uid=current%actualClient%uid,name=current%actualClient%name, img_b=current%actualClient%img_b, img_s=current%actualClient%img_s)
                    current%isbusy = .false.
                end if
            end if
            current => current%next
        end do
    end subroutine checkOutWindows


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
