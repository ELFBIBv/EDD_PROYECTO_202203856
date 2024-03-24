module module_bst
	implicit none
	integer :: id = 1
	type :: node
		integer :: value
		integer :: uid 
		type(node), pointer :: left => null()
		type(node), pointer :: right => null()
	end type

	type :: bst
		type(node), pointer :: root => null()
		contains
		procedure :: add
		procedure :: add_rec
		procedure :: preorder
		procedure :: inorder
		procedure :: postorder
		procedure :: dotgen
		procedure :: dotgen_rec
	end type
    
    contains

    ! Add a value to the tree
    subroutine add(this, value)
        class(bst), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: tmp
        if(associated(this%root)) then
            call this%add_rec(value, this%root)
        else
            allocate(tmp)
            tmp%value = value
            tmp%uid = id
            id = id + 1
            this%root => tmp
        end if	
    end subroutine add

    !con esta funcion va agregar los valores (no usar esta funcion por aparte)
    subroutine add_rec(this, value, tmp)
        class(bst), intent(inout) :: this
        integer, intent(in) :: value
        class(node), intent(inout) :: tmp
        if (value < tmp%value) then
            if (associated(tmp%left)) then
                call this%add_rec(value, tmp%left)
            else
                allocate(tmp%left)
                tmp%left%value = value
                tmp%left%uid = id
                id = id + 1
            end if
        else
            if (associated(tmp%right)) then
                call this%add_rec(value, tmp%right)
            else
                allocate(tmp%right)
                tmp%right%value = value
                tmp%right%uid = id
                id = id + 1
            end if
        end if	
    end subroutine add_rec

    !solo imprime en preorden
    subroutine preorder(this, tmp)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        write (*, '(1I3)', advance='no') (tmp%value)
        call this%preorder(tmp%left)
        call this%preorder(tmp%right)
    end subroutine preorder

    !solo imprime en inorden
    subroutine inorder(this, tmp)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%inorder(tmp%left)
        write (*, '(1I3)', advance='no') (tmp%value)
        call this%inorder(tmp%right)
    end subroutine inorder

    !solo imprime en postorden
    subroutine postorder(this, tmp)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%postorder(tmp%left)
        call this%postorder(tmp%right)
        write (*, '(1I3)', advance='no') (tmp%value)
    end subroutine postorder

    !con esta funcion vamos a graficar
    subroutine dotgen(this, tmp, unit)
        class(bst), intent(in) :: this
            class(node), intent(in), pointer :: tmp
            integer, intent(in) :: unit
            !le indicamos que vamos a agregar un texto
            write(unit, '(A)') 'graph{'
            call this%dotgen_rec(tmp, unit)
        !le indicamos que vamos a agregar un texto
        write(unit, '(A)') '}'
    end subroutine dotgen

    !con esta funcion va a graficar los valores (no usar esta funcion por aparte)
    !basicamente va a ir graficando 
    subroutine dotgen_rec(this, tmp, unit)
        class(bst), intent(in) :: this
        class(node), intent(in), pointer :: tmp
            integer, intent(in) :: unit
        !si no tiene hijos se sale
        if (.not. associated(tmp)) then
            return
        end if
        write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' [label="', tmp%value, '"];'
        !si tiene hijo a la izquierda lo grafica
        if (associated(tmp%left)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%left%uid, ';'
        end if
        !si tiene hijo a la derecha lo grafica
        if (associated(tmp%right)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%right%uid, ';'
        end if
        
        call this%dotgen_rec(tmp%left, unit)
        call this%dotgen_rec(tmp%right, unit)
    end subroutine dotgen_rec
end module bstdef