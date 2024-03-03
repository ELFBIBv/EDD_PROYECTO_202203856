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
        integer, allocatable :: attendedWindow

        type(client), pointer :: next => null()
        type(client), pointer :: prev => null()
    end type client

    !list
    type, public :: ClientQueue
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
    subroutine append(self, uid, name, img_b, img_s,attendedWindow)
        class(ClientQueue), intent(inout) :: self
        integer, intent(in) :: uid
        character(len=*), intent(in) :: name
        integer, intent(in) :: img_b
        integer, intent(in) :: img_s
        integer, intent(in) :: attendedWindow
        
        type(client), pointer :: current
        type(client), pointer :: temp

        allocate(temp)
        
        !agrega 1 al contador
        if (uid>self%id) then
            self%id = uid
        end if
        
        temp = client(uid=self%id,name=name, img_b=img_b, img_s=img_s, steps=0, attendedWindow=attendedWindow)
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
        type(client), pointer :: current
        current => self%head
        print *, "id        name         big images          small images        steps"
        do while (associated(current))
            write (*,fmt="(1x,a,i0)",advance="no") " ", current%uid
            write (*,fmt="(1x,a,a20)",advance="no") " ", current%name
            write (*,fmt="(1x,a,i3)",advance="no") " ", current%img_b
            write (*,fmt="(1x,a,i16)",advance="no") " ", current%img_s
            write (*,fmt="(1x,a,i20)",advance="no") " ", current%steps
            print *
            current => current%next
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
                if (Rclient%uid /= Rclient%next%uid) then
                    Rclient%next%prev => Rclient%prev
                    Rclient%prev%next => Rclient%next
                    deallocate(Rclient)
                    print *, "Client found!, and removed!"
                    return
                else 
                    print *, "unico cliente encontrado"
                    deallocate(Rclient)
                    print *, "unico cliente removido"
                    return
                end if
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


