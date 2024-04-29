module AvlSucursales
    use tech_hash
	implicit none
	integer :: id = 1
    type sucursal
    integer :: id
    integer :: impresorasMantenimiento
    character(len=50) :: departamento
    character(len=50) :: direccion
    character(len=50) :: password
    type (hash) :: hash !Tecnicos registrados en la sucursal
    end type sucursal
	type :: node
		type (sucursal) :: value
		integer :: uid
		integer :: height
		type(node), pointer :: left => null()
		type(node), pointer :: right => null()
	end type

	type :: avl
		type(node), pointer :: root => null()
		contains
		procedure :: add
		procedure :: add_rec
		procedure :: preorder
		procedure :: inorder
		procedure :: postorder
		procedure :: srl
		procedure :: srr
		procedure :: drl
		procedure :: drr
		procedure :: getheight
		procedure :: getmax
		procedure :: dotgen
		procedure :: dotgen_rec
		procedure :: searchSucursalID
		procedure :: searchSucursal_Rec
	end type

contains
subroutine add(this, value)
	class(avl), intent(inout) :: this
	type(sucursal), intent(in) :: value
	type(node), pointer :: tmp
	if(associated(this%root)) then
		call this%add_rec(value, this%root)
	else
		allocate(tmp)
		tmp%value = value
		tmp%uid = id
		tmp%height = 0
		id = id + 1
		this%root => tmp
	end if	
end subroutine add

subroutine add_rec(this, value, tmp)
	class(avl), intent(inout) :: this
	type(sucursal), intent(in) :: value
	type(node), pointer, intent(inout) :: tmp
	integer :: r, l, m
	
	if (.not. associated(tmp)) then
		allocate(tmp)
		tmp%value = value
		tmp%uid = id
		tmp%height = 0
		id = id + 1
		
	else if (value%id < tmp%value%id) then

		call this%add_rec(value, tmp%left)
		if ((this%getheight(tmp%left) - this%getheight(tmp%right))==2) then
			if (value%id < tmp%left%value%id) then
				tmp => this%srl(tmp)
			else
				tmp => this%drl(tmp)
			end if
		end if
	else
		call this%add_rec(value, tmp%right)
		if ((this%getheight(tmp%right) - this%getheight(tmp%left))==2) then
			if (value%id > tmp%right%value%id) then
				tmp => this%srr(tmp)
			else
				tmp => this%drr(tmp)
			end if
		end if
	end if
	r = this%getheight(tmp%right)
	l = this%getheight(tmp%left)
	m = this%getmax(r, l)
	tmp%height = m + 1		
end subroutine add_rec

integer function getheight (this, tmp)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: tmp
	if (.not. associated(tmp)) then
		getheight = -1
	else
		getheight = tmp%height 
	end if
end function getheight

function srl(this, t1) result(t2)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: t1
	type(node), pointer :: t2 
	t2 => t1%left
	t1%left => t2%right
	t2%right => t1
	t1%height = this%getmax(this%getheight(t1%left), this%getheight(t1%right))+1
	t2%height = this%getmax(this%getheight(t2%left), t1%height)+1
end function srl

function srr(this, t1) result(t2)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: t1
	type(node), pointer :: t2 
	t2 => t1%right
	t1%right => t2%left
	t2%left => t1
	t1%height = this%getmax(this%getheight(t1%left), this%getheight(t1%right))+1
	t2%height = this%getmax(this%getheight(t2%right), t1%height)+1
end function srr

function drl(this, tmp) result(res)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: tmp
	type(node), pointer :: res
	tmp%left => this%srr(tmp%left)
	res => this%srl(tmp)
end function drl

function drr(this, tmp) result(res)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: tmp
	type(node), pointer :: res
	tmp%right => this%srl(tmp%right)
	res => this%srr(tmp)
end function drr

integer function getmax(this, val1, val2)
	class(avl), intent(in) :: this
	integer, intent(in) :: val1, val2
	getmax = merge(val1, val2, val1 > val2)
end function getmax

subroutine preorder(this, tmp)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: tmp
	if( .not. associated(tmp)) then
		return
	end if
	write (*, '(1I3)', advance='no') (tmp%value%id)
	call this%preorder(tmp%left)
	call this%preorder(tmp%right)
end subroutine preorder

subroutine inorder(this, tmp)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: tmp
	if( .not. associated(tmp)) then
		return
	end if
	call this%inorder(tmp%left)
	write (*, '(1I3)', advance='no') (tmp%value%id)
	call this%inorder(tmp%right)
end subroutine inorder

subroutine postorder(this, tmp)
	class(avl), intent(in) :: this
	type(node), intent(in), pointer :: tmp
	if( .not. associated(tmp)) then
		return
	end if
	call this%postorder(tmp%left)
	call this%postorder(tmp%right)
	write (*, '(1I3)', advance='no') (tmp%value%id)
end subroutine postorder

subroutine dotgen(this, tmp, unit)
   	class(avl), intent(in) :: this
    	type(node), intent(in), pointer :: tmp
    	integer, intent(in) :: unit
	write(unit, '(A)') 'graph{'
	call this%dotgen_rec(tmp, unit)
	write(unit, '(A)') '}'
end subroutine dotgen

subroutine dotgen_rec(this, tmp, unit)
   	class(avl), intent(in) :: this
 	type(node), intent(in), pointer :: tmp
    	integer, intent(in) :: unit
	if (.not. associated(tmp)) then
        	return
    	end if
	
		write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' [label="', tmp%value%id , '"];'
	if (associated(tmp%left)) then
        	write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%left%uid, ';'
    	end if
	if (associated(tmp%right)) then
        	write (unit, '(A,I5,A,I5,A)') ' ', tmp%uid, ' -- ', tmp%right%uid, ';'
    	end if
	call this%dotgen_rec(tmp%left, unit)
    	call this%dotgen_rec(tmp%right, unit)
end subroutine dotgen_rec




    function searchSucursalID(this, id) result(res)
        class(avl), intent(in) :: this
        type(node), pointer :: res
        integer, intent(in) :: id
        res => null()
        res => this%searchSucursal_Rec(this%root, id)
    end function searchSucursalID

	recursive function searchSucursal_Rec(this,root, id) result(res)
	class(avl), intent(in) :: this
	type(node), pointer :: root
	! type(image), intent(in) :: temp
	integer, intent(in) :: id
	class(node), pointer :: res
	if (.not. associated(root)) then
		print *, "No se encontro la sucursal ", id
		res => null()
		return
	end if

	if (id < root%uid) then
		res => this%searchSucursal_Rec(root%left, id)
	else if (id > root%uid) then
		res => this%searchSucursal_Rec(root%right, id)
	else
		print *, "Se encontro la sucursal ", root%uid
		res => root
	end if
end function searchSucursal_Rec




end module AvlSucursales

