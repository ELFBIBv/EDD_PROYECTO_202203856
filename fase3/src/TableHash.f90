module TableHash
    implicit none
    type hashTable
        integer(kind=8) :: dpi 
        character(:), allocatable :: name, last_name, address, gender, phone
        integer :: trabajos_realizados
    end type hashTable

    type :: hash
        integer :: n ! Number of elements
        integer :: m ! Table size
        integer :: mini, maxi ! Min and max precentages (el mini que vamos a usar es de 50%)
        type(hashTable), dimension(:), allocatable :: hashT ! Hash table
        contains
        procedure :: init 
        procedure :: dispersion
        procedure :: collision
        procedure :: insert
        procedure :: rehashing 
        procedure :: search
        procedure :: showDataHash
        procedure :: showList
        procedure :: grafico
    end type hash

    contains
    subroutine init(this,  m, mini, maxi)
        class(hash), intent(inout) :: this
        integer, intent(in) :: m, mini, maxi
        this%m = m ! Table size
        this%mini = mini
        this%maxi = maxi
        this%n = 0
        if ( allocated(this%hashT) ) then
            deallocate(this%hashT)
        end if
        allocate(this%hashT(m))
        this%hashT = hashTable(-1, 'null', 'null', 'null', 'null', 'null', 0)
    end subroutine init

    function dispersion(this,  k) result(disp)
        class(hash), intent(inout) :: this
        integer(kind=8), intent(in) :: k
        integer(kind=8) :: disp
        disp = mod(k, this%m)
    end function dispersion

    function collision(this,  DPI, i) result(coll)
        class(hash), intent(inout) :: this
        integer(kind=8), intent(in) :: DPI
        integer, intent(in) :: i
        integer(kind=8) :: coll
        coll = mod((mod(DPI, 7) + 1)*i, this%m)
    end function collision

    subroutine insert(this, t)
        class(hash), intent(inout) :: this
        type(hashTable), intent(in) :: t
        integer :: i ! numero de colisiones
        integer(kind=8) :: DPI ! numero de DPI
        integer(kind=8) :: possition !Es la dispersion que se va a utilizar
        DPI = t%dpi
        i = 1
        possition = dispersion(this, DPI)
        ! Esto se hace porque las tablas en Fortran comienzan desde 1
        if ( possition == 0 ) then
            possition = 1
        end if
        !buscamos la posicion adecuada
        do while (this%hashT(possition)%dpi /= -1)
            possition = collision(this, DPI, i)
            i = i + 1
        end do    
        this%hashT(possition) = t
        this%n = this%n + 1

        !Dentro del metodo de rehashing siempre va a revisar 
        !si es necesario hacer rehashing o no por eso siempre se ejecuta
        
        call this%rehashing()            
    end subroutine insert

    subroutine rehashing(this)
        class(hash), intent(inout) :: this
        integer :: i, mprev
        type(hashTable), dimension(:), allocatable :: temp 
        if ( this%n * 100 / this%m >= this%maxi ) then
            allocate(temp(this%m))
            temp = this%hashT
            call this%showDataHash()
            mprev = this%m            
            this%m = this%m * 2
            print *, 'New size: ', this%m
            call this%init(this%m, this%mini, this%maxi)
            do i = 1, mprev
                if ( temp(i)%dpi /= -1 ) then
                    call this%insert(temp(i))
                end if
            end do
        else
            call this%showDataHash()
        end if               
    end subroutine rehashing

    subroutine search(this, DPI)
        class(hash), intent(inout) :: this
        integer(kind=8), intent(in) :: DPI
        integer :: i ! numero de colisiones
        integer(kind=8) :: possition !Es la dispersion que se va a utilizar
        possition = dispersion(this, DPI)
        i = 1
        ! Esto se hace porque las tablas en Fortran comienzan desde 1
        if ( possition == 0 ) then
            possition = 1
        end if
        !buscamos la posicion adecuada
        if (this%hashT(possition)%dpi == -1) then
            return
        end if
        do while (this%hashT(possition)%dpi /= DPI)
            possition = collision(this, DPI, i)
            i = i + 1
        end do
        print *, 'DPI: ', this%hashT(possition)%dpi
        print *, 'Name: ', this%hashT(possition)%name
        print *, 'Last Name: ', this%hashT(possition)%last_name
        print *, 'Address: ', this%hashT(possition)%address
        print *, "Trabajos realizados: ", this%hashT(possition)%trabajos_realizados
    end subroutine search

    subroutine showDataHash(this)
        class(hash), intent(inout) :: this
        integer :: i
        write(*, '(A)', advance='no') '['
        do i = 1, this%m
            write(*, '(A, A)', advance='no') trim(adjustl(this%hashT(i)%name)), " "
        end do      
        write(*, '(A, I0, A)') '] ', (this%n * 100 / this%m), '%'
    end subroutine showDataHash

    subroutine showList(this)
        class(hash), intent(inout) :: this
        integer :: i
        do i = 1, this%m
            if ( this%hashT(i)%dpi /= -1 ) then
                print *, 'DPI: ', this%hashT(i)%dpi
                print *, 'Name: ', this%hashT(i)%name
                print *, 'Last Name: ', this%hashT(i)%last_name
                print *, 'Address: ', this%hashT(i)%address
                print *, "Trabajos realizados: ", this%hashT(i)%trabajos_realizados
                print *, "---------------------------------"
            end if
        end do
    end subroutine showList

    subroutine grafico(self, filename)
        class(hash), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer :: i,contador
        integer :: unit
    
        unit = 10  
        open(unit, file="images/hash.dot", status='replace')
        print *, "GRAFICANDO HASH"
        write(unit, '(A)') 'digraph {'
        write(unit, '(A)') 'rankdir=BR'
        write(unit, '(A)') 'node [shape=record, style=filled, fillcolor="#66CCCC", fontname="Arial"];'
        write(unit, '(A)',advance="no") 'node1[label="',self%hashT(1)%name
        do i = 1, size(self%hashT) - 1
            if (self%hashT(i)%name /= 'null') then
                write(unit, '(A,I0,A,A)',advance="no") '|'//trim(self%hashT(i)%name)
            else 
                write(unit, '(A)',advance="no") '|'
            end if 
        end do
        write(unit, '(A)') '"];'
        
        write(unit, '(A)') '}'
    
        close(unit)
        call system('dot -Tpng images/hash.dot -o images/hash.png')
        ! windows
        ! call system("start images\merkle.png")
        !linux
        call system("eog images/hash.png") !si da error se debe de usar "unset GTK_PATH"
    end subroutine grafico
end module TableHash
