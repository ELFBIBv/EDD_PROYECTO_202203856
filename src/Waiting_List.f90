module waitingListModule 
    use clientRegisterModule
    use clientQueueModule
    implicit none
    private

    type, public:: waitingList
        type(historyClients), allocatable:: historyclients
        type(client), pointer :: head => null()
        contains
        procedure :: addClient
        !procedure :: removeClient
        !procedure :: checkOutTime
    end type waitingList

    contains

    ! Add a client to the waiting list
    subroutine addClient(this, uid, name, img_b, img_s)
        class(waitingList), intent(inout) :: this
        integer, intent(in) :: uid
        character(len=*), intent(in) :: name
        integer, intent(in) :: img_b
        integer, intent(in) :: img_s
        
        type(client), pointer :: cliente
        type(client), pointer :: currentClient

        allocate(cliente)

        cliente = client(uid, name, img_b, img_s)

        if(associated(this%head)) then
            currentClient = this%head
            do while (.not. associated(currentClient%next,this%head)) !el associated va a comparar si están asociados
                currentClient => currentClient%next
            end do
            currentClient%next => cliente
            cliente%prev => currentClient
            cliente%next => this%head
            this%head%prev => cliente
        else
            this%head%next => cliente
            this%head%prev => cliente
            cliente%next => this%head
            cliente%prev => this%head
        end if
    end subroutine addClient

end module waitingListModule

