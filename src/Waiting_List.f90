module waitingListModule 
    use clientRegisterModule
    use clientQueueModule
    implicit none
    private
    
    type, public:: waitingList
        type(historyClients), pointer:: historyclients
        type(client), pointer :: head => null()
        contains
        procedure :: addClient
        procedure :: removeClient
        procedure :: checkTime
        procedure :: printWaitingList
    end type waitingList

    contains

    ! Add a client to the waiting list
    subroutine addClient(this, aclient)
        class(waitingList), intent(inout) :: this
        type(client),pointer, intent(in) :: aclient
        type(client), pointer :: currentClient

        integer :: uid
        character(len=50), allocatable :: name
        integer :: img_b
        integer :: img_s
        integer :: steps
        integer :: attendedWindow

        uid = aclient%uid
        name = aclient%name
        img_b = aclient%img_b
        img_s = aclient%img_s
        steps = aclient%steps
        attendedWindow = aclient%attendedWindow
        
        if ( .not. associated(this%head)) then
            currentClient => this%head
            do while (associated(currentClient%next, this%head))
                currentClient => currentClient%next
            end do
            currentClient%next => aclient
            aclient%prev => currentClient
            aclient%next => this%head
            this%head%prev => aclient
        else
            this%head => aclient
            aclient%next => this%head
            aclient%prev => this%head
        end if 

    end subroutine addClient

    ! Remove a client from the waiting list
    subroutine removeClient(this, uid)
        class(waitingList), intent(inout) :: this
        integer, intent(in) :: uid

        character(:),allocatable :: name
        integer :: img_b
        integer :: img_s
        integer :: steps
        integer :: attendedWindow

        type(client), pointer :: currentClient
        type(client), pointer :: nextClient
        type(client), pointer :: prevClient

        currentClient => this%head
        do while (associated(currentClient%next, this%head))
            if(currentClient%uid == uid) then
                name = currentClient%name
                img_b = currentClient%img_b
                img_s = currentClient%img_s
                steps = currentClient%steps
                attendedWindow = currentClient%attendedWindow

                call this%historyclients%addClient(name=name,attendedWindow=attendedWindow, NoImages=img_b + img_s, steps=steps)
                prevClient => currentClient%prev
                nextClient => currentClient%next
                prevClient%next => nextClient
                nextClient%prev => prevClient
                return
            end if
            currentClient => currentClient%next
        end do
    end subroutine removeClient

    ! Check out the time of the clients in the waiting list
    subroutine checkTime(this)
        class(waitingList), intent(inout) :: this

        type(client), pointer :: currentClient
        if (associated(this%head)) then
            currentClient => this%head
            do while (associated(currentClient%next, this%head))
                if (currentClient%steps == 0) then
                    call this%removeClient(currentClient%uid)
                else
                    currentClient%steps = currentClient%steps - 1
                end if
                currentClient => currentClient%next
            end do
        end if
    end subroutine checkTime

    ! Print the waiting list
    subroutine printWaitingList(this)
        class(waitingList), intent(in) :: this
        type(client), pointer :: currentClient
        if (associated(this%head)) then
            currentClient => this%head
            do while (associated(currentClient%next, this%head))
                write (*,fmt="(1x,a,i0)",advance="no") "id de cliente ", currentClient%uid
                write (*,fmt="(1x,a,a)",advance="no") " nombre ", currentClient%name
                write (*,fmt="(1x,a,i0)",advance="no") "img_b ", currentClient%img_b
                write (*,fmt="(1x,a,i0)",advance="no") "img_s ", currentClient%img_s
                write (*,fmt="(1x,a,i0)",advance="no") "No. pasos ", currentClient%steps
                write (*,fmt="(1x,a,i0)",advance="no") "Ventana atendida ", currentClient%attendedWindow
                currentClient => currentClient%next
            end do
        end if
    end subroutine printWaitingList

end module waitingListModule

