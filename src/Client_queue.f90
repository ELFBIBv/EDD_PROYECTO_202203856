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
    end type client

    !list
    type, public :: ClientQueue
        type(client), pointer :: head => null()
        type(client), pointer :: tail => null()
        integer :: id = 0
    contains
        procedure :: append
        procedure :: print
        procedure :: removeClient
        procedure :: addSteps
        procedure :: addRandomClients
        procedure :: getRandomNum
        procedure :: graphClients
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
            this%tail%next => temp
            this%tail => temp
        else
            this%head => temp
            this%tail => temp
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
    subroutine removeClient(this)
        class(ClientQueue), intent(inout) :: this
        class(client), pointer :: temp
        
        temp=>this%head
        if (associated(this%head)) then
            this%head => this%head%next
            deallocate(temp)
        else
            print *, "No clients to remove"
        end if
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
        
        names = ["Juan ", "Luis ","Jose ","Lisa ","Maria","Pedro","Lucas","Laura","Luisa","Lucia"]
        lastname = ["Perez     ", "Gomez     ","Rodriguez ","Sanchez   ","Garcia    ","Lopez     ","Martinez  ",& 
        "Gonzalez  ","Fernandez ","Diaz      "]
        
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
    
    !Graph_queue
    subroutine graphClients(this)
        class(ClientQueue), intent(in) :: this
        type(client), pointer :: current
    
        integer :: unit, count = 0
    
        open(unit, file="images\queue.dot", status="replace")
        write(unit, *) 'digraph G {'
        
        if (.not. associated(this%head)) then
            write(unit, *) '"empty" [label="Empty queue", shape=box];'
        else
            current => this%head
            count = 0
            do while(associated(current))
                write(unit, *) " ",'"Node', count, '" [label="', trim(current%name),'"];'
                if (associated(current%next)) then
                    write(unit, *) " ",'"Node', count, '" -> "Node', count+1, '";'
                end if
                    count = count + 1
                current => current%next
            end do
        end if
        write(unit, *) '}'
    
        close(unit)
        call execute_command_line('dot -Tpng images\queue.dot -o images\queue.png')
    end subroutine graphClients
    
end module clientQueueModule


