module tech_hash
    implicit none
    type tech
        integer(kind=8) :: dpi 
        character(:), allocatable :: name, last_name, address, gender, phone
    end type tech

    type :: hash
        integer :: n ! Number of elements
        integer :: m ! Table size
        integer :: mini, maxi ! Min and max precentages (el mini que vamos a usar es de 50%)
        type(tech), dimension(:), allocatable :: hashT ! Hash table
        contains
        procedure :: init 
        procedure :: dispersion
        procedure :: collision
        procedure :: insert
        procedure :: rehashing 
        procedure :: search
        procedure :: show
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
        this%hashT = tech(-1, 'null', 'null', 'null', 'null', 'null')
    end subroutine init

    function dispersion(this,  k) result(disp)
        class(hash), intent(inout) :: this
        integer(kind=8), intent(in) :: k
        integer :: disp
        disp = mod(k, this%m)
    end function dispersion

    function collision(this,  DPI, i) result(coll)
        class(hash), intent(inout) :: this
        integer(kind=8), intent(in) :: DPI
        integer, intent(in) :: i
        integer :: coll
        coll = mod((mod(DPI, 7) + 1)*i, this%m)
    end function collision

    subroutine insert(this, t)
        class(hash), intent(inout) :: this
        type(tech), intent(in) :: t
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

        !!Dentro del metodo de rehashing siempre va a revisar 
        !!si es necesario hacer rehashing o no por eso siempre se ejecuta
        
        call this%rehashing()            
    end subroutine insert

    subroutine rehashing(this)
        class(hash), intent(inout) :: this
        integer :: i, mprev
        type(tech), dimension(:), allocatable :: temp 
        if ( this%n * 100 / this%m >= this%maxi ) then
            allocate(temp(this%m))
            temp = this%hashT
            call this%show()
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
            call this%show()
        end if               
    end subroutine rehashing

    subroutine search(this, k)
        class(hash), intent(inout) :: this
        integer(kind=8), intent(in) :: k
        integer :: i, poss
        i = 0
        poss = dispersion(this, k)
        do while (this%hashT(poss)%dpi /= k)
            poss = collision(this, k, i)
            i = i + 1
        end do
        ! d = poss xd
        ! print *, this%hashT(d)%name, this%hashT(d)%last_name, this%hashT(d)%address, this%hashT(d)%gender, this%hashT(d)%phone            
        write(*, "(A)",advance="no") this%hashT(poss)%name//"  "//this%hashT(poss)%last_name//"  "//this%hashT(poss)%address
        write(*, "(A)") "  "//this%hashT(poss)%gender//"  "//this%hashT(poss)%phone
    end subroutine search
    
    subroutine show(this)
        class(hash), intent(inout) :: this
        integer :: i
        write(*, '(A)', advance='no') '['
        do i = 1, this%m
            write(*, '(A, A)', advance='no') this%hashT(i)%name, " "
        end do      
        write(*, '(A, I0, A)') '] ', (this%n * 100 / this%m), '%'
    end subroutine show
end module tech_hash