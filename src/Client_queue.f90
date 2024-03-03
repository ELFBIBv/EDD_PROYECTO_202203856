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
        integer :: id = 0
    contains
        procedure :: append
        procedure :: print
        procedure :: removeClient
        procedure :: addSteps
        procedure :: addRandomClients
        procedure :: getRandomNum
    end type ClientQueue
    
    contains

    !append
    subroutine append(this, uid, name, img_b, img_s,attendedWindow)
        class(ClientQueue), intent(inout) :: this
        integer, intent(in) :: uid
        character(len=*), intent(in) :: name
        integer, intent(in) :: img_b
        integer, intent(in) :: img_s
        integer, intent(in) :: attendedWindow
        
        type(client), pointer :: current
        type(client), pointer :: temp

        allocate(temp)
        
        !agrega 1 al contador
        if (uid>this%id) then
            this%id = uid
        end if
        
        temp = client(uid=this%id,name=name, img_b=img_b, img_s=img_s, steps=0, attendedWindow=attendedWindow)
        this%id = this%id + 1
        
        !agrega el cliente a la lista
        current => this%head
        if (associated(current)) then
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
            temp%prev => current
        else
            this%head => temp
        end if
    end subroutine append

    !print
    subroutine print(this)
        class(ClientQueue), intent(in) :: this
        type(client), pointer :: current
        current => this%head
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
    subroutine removeClient(this,idclient)
        class(ClientQueue), intent(inout) :: this
        type(client), pointer :: Rclient
        integer, intent(in) :: idclient
        Rclient => this%head
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
    subroutine addSteps(this)
        class(ClientQueue), intent(inout) :: this
        type(client), pointer :: actualclient
        actualclient => this%head
        do while (associated(actualclient))
            actualclient%steps = actualclient%steps + 1
            actualclient => actualclient%next
        end do
        return
        print *, "Client not found!"
    end subroutine addSteps

    subroutine addRandomClients(this)
        class(ClientQueue), intent(inout) :: this
        CHARACTER(LEN=20), DIMENSION(10) :: names
        CHARACTER(LEN=20), DIMENSION(10) :: lastname
        integer :: iterations
        integer :: i
        integer :: numrandom1
        integer :: numrandom2
        integer :: img_b
        integer :: img_s
        character(len=20) :: name

        names = ["juan ", "luis ","jose ","lisa ","maria","pedro","lucas","laura","luisa","lucia"]
        lastname = ["perez     ", "gomez     ","rodriguez ","sanchez   ","garcia    ","lopez     ","martinez  ",& 
        "gonzalez  ","fernandez ","diaz      "]

        iterations = this%getRandomNum(3)
        do i=1,iterations
            numrandom1 = this%getRandomNum(10)
            numrandom2 = this%getRandomNum(10)
            name = trim(adjustl(names(numrandom1)))//" "//trim(adjustl(lastname(numrandom2)))
            img_b = this%getRandomNum(3)
            img_s = this%getRandomNum(3)
            call this%append(uid=this%id, name=name, img_b=img_b, img_s=img_s, attendedWindow=0)
            ! print *, "id        name         big images          small images        steps"
            ! write (*,fmt="(1x,a,i0)",advance="no") " ", this%id
            ! write (*,fmt="(1x,a,a20)",advance="no") " ", name
            ! write (*,fmt="(1x,a,i3)",advance="no") " ", img_b
            ! write (*,fmt="(1x,a,i16)",advance="no") " ", img_s
            ! write (*,fmt="(1x,a,i20)",advance="no") " ", 0
            ! print *
            ! print *, "Client added!"
        end do      
    end subroutine addRandomClients

    !getRandomNum
    function getRandomNum(this,max) result (randomInt)
        class(clientQueue), intent(inout) :: this
        integer, intent(in) :: max
        real :: random
        integer :: randomInt
        
        call random_seed()
        call random_number(random)
        randomInt = nint(random*(max-1))+1
    end function getRandomNum
end module clientQueueModule


