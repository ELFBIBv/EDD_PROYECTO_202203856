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
        type(window), pointer :: head => null()
        contains
        procedure :: addWindow
        procedure :: checkWindows
        procedure :: addClientInWindow
        procedure :: printWindows
        procedure :: graphWindows
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
        current => self%head
        if(associated(current)) then
            do while(associated(current%next))
                current => current%next
            end do
            current%next => temp
        else
            self%head => temp
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
        type(client) :: c

        current => self%head
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
                    c = clientList%head
                    call waiting_List%addClient(c%uid, c%name,c%img_b,c%img_s,c%steps,c%attendedWindow)
                    call clientList%removeClient()
                    call printerList%addPaper(current%paperList)
                    return
                end if
            else
                print *, "Cliente ", cliente%uid, " entra a la ventana ", current%windowNumber
                call self%addClientInWindow(cliente)
                call clientList%removeClient()
                return !quitar esta linea si se quieren llenar todas las ventanas vacias
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

        current => self%head
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

        current => self%head
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
            call current%paperList%printQueue()
            current => current%next
        end do
    end subroutine printWindows
    
    subroutine graphWindows(this)
        class(window_linked_list), intent(in) :: this
        type(window), pointer :: current
        character(20) :: clientea
        type(window), pointer :: next
        integer :: unit,count =0
        next => this%head
        do while(associated(next))
            count = count + 1
            call next%paperList%graphPapers(count) !este está imprimiendo solo la cabeza
            next => next%next
        end do

        open(unit, file="images\windows.dot", status="replace")
        write(unit, *) 'digraph G {'
        
        if (.not. associated(this%head)) then
            write(unit, *) '"empty" [label="Empty windows", shape=box];'
        else
            current => this%head
            count = 0
            do while(associated(current))
                write(unit, *) " ",'"Node', count, '" [label=" Ventana', current%windowNumber,'"];'
                    if (current%isbusy) then
                        clientea = current%actualClient%name
                        write(unit, *) " ",'"Nodec', current%actualClient%uid, '" -> "Node', count, '";'
                        write(unit, *) " ",'"Nodec', current%actualClient%uid, '" [label=" cliente ',trim(clientea),'"];'
                    end if
                if (associated(current%next)) then
                    write(unit, *) " ",'"Node', count, '" -> "Node', count+1, '";'
                end if
                count = count + 1
                current => current%next
            end do
        end if
        write(unit, *) '}'

        close(unit)
        call execute_command_line('dot -Tpng images\windows.dot -o images\Window.png')
    end subroutine graphWindows

end module
