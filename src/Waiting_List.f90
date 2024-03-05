module waitingListModule 
    use clientRegisterModule
    implicit none
    private

    type, public:: clientWaiting
    integer, allocatable :: uid
    character(:), allocatable :: name
    integer, allocatable :: img_b
    integer, allocatable :: img_s
    integer, allocatable :: steps
    integer, allocatable :: attendedWindow
    integer, allocatable :: stepsClientneed
    type(clientWaiting), pointer :: next => null()
    end type clientWaiting
    
    type, public:: waitingList
        type(historyClients) :: historyclients
        type(clientWaiting), pointer :: head => null()
        contains
        procedure :: addClient
        procedure :: removeClient
        procedure :: addSteps
        procedure :: printWaitingList
        procedure :: graphWaitingList
    end type waitingList

    contains

    ! Add a client to the waiting list
    subroutine addClient(this, uid, name, img_b, img_s, steps, attendedWindow)
        class(waitingList), intent(inout) :: this
        type(clientWaiting),pointer :: aclient
        type(clientWaiting), pointer :: currentClient

        integer, intent(in) :: uid
        character(:), allocatable, intent(in) :: name
        integer, intent(in) :: img_b
        integer, intent(in) :: img_s
        integer, intent(in) :: steps
        integer, intent(in) :: attendedWindow

        allocate(aclient)
        aclient%uid = uid
        aclient%name = name
        aclient%img_b = img_b
        aclient%img_s = img_s
        aclient%steps = steps
        aclient%attendedWindow = attendedWindow
        aclient%stepsClientneed = img_b*2+img_s+1

        if (associated(this%head)) then
            if (associated(this%head,this%head%next)) then
                this%head%next => aclient
                aclient%next => this%head
            else
                currentClient => this%head
                do while ( .not. associated(currentClient%next, this%head))
                    currentClient => currentClient%next
                end do
                currentClient%next => aclient
                aclient%next => this%head
            end if
        else
            this%head => aclient
            aclient%next => this%head
        end if 

    end subroutine addClient

    ! Remove a client from the waiting list
    subroutine removeClient(this)
        class(waitingList), intent(inout) :: this

        character(:),allocatable :: name
        integer :: img_b
        integer :: img_s
        integer :: steps
        integer :: attendedWindow

        type(clientWaiting), pointer :: currentClient
        type(clientWaiting), pointer :: prevClient

        currentClient => this%head
        do while (.not. associated(currentClient%next, this%head))
            currentClient => currentClient%next
        end do
        prevClient => currentClient
        currentClient => this%head
        !
        name = currentClient%name
        img_b = currentClient%img_b
        img_s = currentClient%img_s
        steps = currentClient%steps
        attendedWindow = currentClient%attendedWindow
        call this%historyclients%addClient(name=name,attendedWindow=attendedWindow, NoImages=img_b + img_s, steps=steps)
        !
        this%head => this%head%next
        prevClient%next => this%head
        deallocate(currentClient)
    end subroutine removeClient

    ! Check out the time of the clients in the waiting list
    subroutine addSteps(this)
        class(waitingList), intent(inout) :: this
        type(clientWaiting), pointer :: cc
        character(20) :: name
        integer :: img_b
        integer :: img_s
        integer :: steps
        integer :: attendedWindow
        if (associated(this%head)) then
            cc => this%head
            cc%stepsClientneed = cc%stepsClientneed - 1
            if (cc%stepsClientneed == 0) then
                name = this%head%name
                img_b = this%head%img_b
                img_s = this%head%img_s
                steps = this%head%steps
                attendedWindow = this%head%attendedWindow
                call this%removeClient()
            end if
        end if
    end subroutine addSteps

    ! Print the waiting list
    subroutine printWaitingList(this)
        class(waitingList), intent(inout) :: this
        type(clientWaiting), pointer :: currentClient

        logical :: temp
        temp = .false.

        if (associated(this%head)) then
            currentClient => this%head
            print *, "id        name         big images          small images        steps      stepsneed    attended window"
            do while (.not. temp)
                write (*,fmt="(1x,a,i0)",advance="no") " ", currentClient%uid
                write (*,fmt="(1x,a,a20)",advance="no") " ", currentClient%name
                write (*,fmt="(1x,a,i3)",advance="no") " ", currentClient%img_b
                write (*,fmt="(1x,a,i16)",advance="no") " ", currentClient%img_s
                write (*,fmt="(1x,a,i20)",advance="no") " ", currentClient%steps
                write (*,fmt="(1x,a,i20)",advance="no") " ", currentClient%stepsClientneed
                write (*,fmt="(1x,a,i20)",advance="no") " ", currentClient%attendedWindow
                print *
                currentClient => currentClient%next
                if (associated(currentClient, this%head)) then
                    temp = .true.
                end if 
            end do 
            call this%historyClients%printHistoryClients()
        else
            print *, "No hay clientes en la lista de espera"
        end if
    end subroutine printWaitingList

    !graphWaitingList
    subroutine graphWaitingList(this)
        class(waitingList), intent(in) :: this
        type(clientWaiting), pointer :: current
    
        integer :: unit, count = 0
    
        open(unit, file="images\waitingList.dot", status="replace")
        write(unit, *) 'digraph G {'
        
        if (.not. associated(this%head)) then
            write(unit, *) '"empty" [label="Empty waitingList", shape=box];'
        else
            current => this%head
            count = 0
            do while( .not. associated(current%next,this%head))
                write(unit, *) " ",'"Node', count, '" [label="', trim(current%name),'"];'
                if (associated(current%next)) then
                    write(unit, *) " ",'"Node', count, '" -> "Node', count+1, '";'
                end if
                    count = count + 1
                current => current%next
            end do
            write(unit, *) " ",'"Node', count, '" -> "Node', 0, '";'
            write(unit, *) " ",'"Node', count, '" [label="', trim(current%name),'"];'
        end if
        write(unit, *) '}'
    
        close(unit)
        call execute_command_line('dot -Tpng images\waitingList.dot -o images\waitingList.png')
    end subroutine graphWaitingList

end module waitingListModule

