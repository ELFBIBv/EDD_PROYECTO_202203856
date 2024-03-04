module windowsModule
    use clientQueueModule
    use PaperQueueModule
    use printerModule
    use waitingListModule
    implicit none
    private

    ! window
    type, public :: window
        integer :: windowNumber
        Logical :: isbusy = .false.
        integer :: stepsClientneed = 0
        integer :: img_b = 0
        integer :: img_s = 0
        type(client), allocatable :: actualClient
        type(window), pointer :: next => null()
        type(PaperQueue) :: paperList
    end type window

    ! window list
    type, public :: window_linked_list
        type(window), pointer :: first => null()
        contains
        procedure :: addWindow
        procedure :: checkWindows
        procedure :: addClientInWindow
        procedure :: printWindows
    end type window_linked_list

    contains
    
    !addWindow
    subroutine addWindow(self,num)
        class(window_linked_list), intent(inout) :: self
        type(window), pointer :: temp
        type(window), pointer :: current
        integer, intent(in) :: num

        allocate(temp)

        temp%windowNumber = num
        temp%isbusy = .false.

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
    
    !checkWindows
    subroutine checkWindows(self,clientList, printerList,waiting_List)
        class(window_linked_list), intent(inout) :: self
        type(ClientQueue), intent(inout) :: clientList
        type(listPrinter), intent(inout) :: printerList
        type(waitingList), intent(inout) :: waiting_List
        type(paperQueue), pointer :: paperList
        type(window), pointer :: current
        type(client), pointer :: cliente
        type(client), pointer :: clientWaiting

        current => self%first
        cliente => clientList%head
        do while(associated(current) .and. associated(cliente))
            if(current%isbusy) then
                current%actualClient%steps = current%actualClient%steps + 1
                current%stepsClientneed = current%stepsClientneed-1
                if (current%img_s > 0) then
                    current%img_s = current%img_s - 1
                    call current%paperList%addsmallPaper()
                    print *, "la ventanilla", current%windowNumber, " recibio un papel chico"
                else if (current%img_b > 0) then
                    current%img_b = current%img_b - 1
                    call current%paperList%addbigpaper()
                    print *, "la ventanilla", current%windowNumber, " recibio un papel grande"
                end if
                if(current%stepsClientneed == 0) then
                    current%isbusy = .false.
                    print *, "Cliente ", current%actualClient%uid, " sale de la ventana ", current%windowNumber
                    print *, "Cliente ", cliente%uid, " entra a la ventana ", current%windowNumber
                    call self%addClientInWindow(cliente)
                    call clientList%removeClient()
                    call printerList%addPaper(current%paperList)
                    !call waiting_List%addClient(clientWaiting)
                end if
            else
                if(associated(cliente)) then
                    print *, "Cliente ", cliente%uid, " entra a la ventana ", current%windowNumber
                    call self%addClientInWindow(cliente)
                    print *,"algo1"
                    call clientList%removeClient()
                    return !quitar esta linea si se quieren llenar todas las ventanas vacias
                end if
            end if
            current => current%next
        end do
    end subroutine checkWindows 

    !addClientInWin
    subroutine addClientInWindow(self,cliente)
        class(window_linked_list), intent(inout) :: self
        type(client), pointer, intent(in) :: cliente
        type(window), pointer :: current
        type(paperQueue), pointer :: paperList

        current => self%first
        do while(associated(current))
            if(.not. current%isbusy) then
                cliente%attendedWindow = current%windowNumber
                current%isbusy = .true.
                current%stepsClientneed = cliente%img_s+cliente%img_b+1
                current%img_b = cliente%img_b
                current%img_s = cliente%img_s
                current%actualClient = cliente
                call current%paperList%cleanPaperQueue()
                return
            end if
            current => current%next
        end do
        print *, "No hay ventanas disponibles"

    end subroutine addClientInWindow

    !printWindows
    subroutine printWindows(self)
        class(window_linked_list), intent(inout) :: self
        type(window), pointer :: current
        !print *, "Ventanas: "

        current => self%first
        print *, "Ventana "," state ", "id "," steps ", " steps left ", " nombreCliente "
        do while(associated(current))
            if (current%isbusy) then
                write (*,fmt="(1x,i4)",advance="no") current%windowNumber
                write (*,fmt="(1x,L8)",advance="no") current%isbusy
                write (*,fmt="(1x,i5)",advance="no") current%actualClient%uid
                write (*,fmt="(1x,i5)",advance="no") current%actualClient%steps
                write (*,fmt="(1x,i9)",advance="no") current%stepsClientneed
                write (*,fmt="(7x,a15)",advance="no") current%actualClient%name
                print *, " "
            else
                write (*,fmt="(1x,i4)",advance="no") current%windowNumber
                write (*,fmt="(1x,L8)",advance="no") current%isbusy
                print *, " "
            end if
            current => current%next
        end do
    end subroutine printWindows
end module
