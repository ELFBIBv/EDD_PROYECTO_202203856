module clientQueueModule !(terminado) solo faltaria que agregue clientes aleatoriamente

    implicit none
    private
    
    !client
    type, public :: client
        integer, allocatable :: uid
        character(:), allocatable :: name
        integer, allocatable :: img_b
        integer, allocatable :: img_s
        integer, allocatable :: steps
        character(:), allocatable :: state
        integer, allocatable :: attendedWindow

        type(client), pointer :: next => null()
        type(client), pointer :: prev => null()
    end type client

    !list
    type, public :: ClientQueue
        private
        type(client), pointer :: head => null()
        integer :: id = 1
    contains
        procedure :: append
        procedure :: print
        procedure :: removeClient
        procedure :: addSteps
    end type ClientQueue
    
    contains

    !append
    subroutine append(self, uid, name, img_b, img_s)
        class(ClientQueue), intent(inout) :: self
        integer, intent(in) :: uid
        character(len=*), intent(in) :: name
        integer, intent(in) :: img_b
        integer, intent(in) :: img_s
        
        type(client), pointer :: current
        type(client), pointer :: temp

        allocate(temp)
        
        !agrega 1 al contador
        if (uid>self%id) then
            self%id = uid
        end if
        
        temp = client(uid=self%id,name=name, img_b=img_b, img_s=img_s, steps=0)
        self%id = self%id + 1
        
        !agrega el cliente a la lista
        current => self%head
        if (associated(current)) then
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
            temp%prev => current
        else
            self%head => temp
        end if
    end subroutine append

    !print
    subroutine print(self)
        class(ClientQueue), intent(in) :: self
        integer :: count = 1
        type(client), pointer :: current
        current => self%head
        print *, "id        name         big images          small images        steps"
        do while (associated(current))
            print *,current%uid,current%name,current%img_b,current%img_s,current%steps
            current => current%next
            count = count + 1
        end do
    end subroutine print
    
    !removeClient
    subroutine removeClient(self,idclient)
        class(ClientQueue), intent(inout) :: self
        type(client), pointer :: Rclient
        integer, intent(in) :: idclient
        Rclient => self%head
        do while (associated(Rclient))
            if (Rclient%uid == idclient) then
                Rclient%next%prev => Rclient%prev
                Rclient%prev%next => Rclient%next
                deallocate(Rclient)
                print *, "Client found!, and removed!"
                return
            end if
            Rclient=>Rclient%next
        end do
        print *, "Client not found!"
    end subroutine removeClient
    
    !addSteps
    subroutine addSteps(self)
        class(ClientQueue), intent(inout) :: self
        type(client), pointer :: actualclient
        actualclient => self%head
        do while (associated(actualclient))
            actualclient%steps = actualclient%steps + 1
            actualclient => actualclient%next
        end do
        return
        print *, "Client not found!"
    end subroutine addSteps


end module clientQueueModule


