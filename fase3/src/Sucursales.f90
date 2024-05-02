module module_AvlSucursales
    use TableHash
    
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
        procedure :: showSucursal
        procedure :: showSucursales
        procedure :: addTableHash
        procedure :: showTableHash
        procedure :: showTableHashSucursales
        procedure :: searchTech
        procedure :: getNameSucursalId
        procedure :: graficarTablaHashIdSucursal
        procedure :: preorderhash
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
        print *, tmp%value%id, "->", tmp%value%departamento
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
        print *, "Se encontro la sucursal ", res%value%id
    end function searchSucursalID

    recursive function searchSucursal_Rec(this,root, id) result(res)
        class(avl), intent(in) :: this
        type(node), pointer :: root
        ! type(image), intent(in) :: temp
        integer, intent(in) :: id
        class(node), pointer :: res
        if (.not. associated(root)) then
            ! print *, "No se encontro la sucursal ", id
            res => null()
            return
        end if

        if (id < root%value%id) then
            res => this%searchSucursal_Rec(root%left, id)
        else if (id > root%value%id) then
            res => this%searchSucursal_Rec(root%right, id)
        else
            print *, "Se encontro la sucursal ", root%value%id
            res => root
        end if
    end function searchSucursal_Rec

    subroutine showSucursal(this, id)
        class(avl), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: tmp
        tmp => this%searchSucursalID(id)
        if (associated(tmp)) then
            write (*, '(A)') 'Sucursal: ', tmp%value%id
            write (*, '(A)') 'Departamento: ', tmp%value%departamento
            write (*, '(A)') 'Direccion: ', tmp%value%direccion
            write (*, '(A)') 'Impresoras en mantenimiento: ', tmp%value%impresorasMantenimiento
        else
            ! write (*, '(A)') 'No se encontro la sucursal ', id
        end if
    end subroutine showSucursal

    function getNameSucursalId(this, id) result(name)
        class(avl), intent(in) :: this
        integer, intent(in) :: id
        character(len=100) :: name
        type(node), pointer :: tmp
        tmp => this%searchSucursalID(id)
        if (associated(tmp)) then
            name = trim(tmp%value%departamento) // ', ' // trim(tmp%value%direccion)
        else
            name = ""
            ! print *, "No se encontro la sucursal ", id
        end if
    end function getNameSucursalId

    subroutine showSucursales(this)
        class(avl), intent(in) :: this
        call this%inorder(this%root)
    end subroutine showSucursales

    subroutine showTableHashSucursales(this)
        class(avl), intent(in) :: this
        type(node), pointer :: tmp
        tmp => this%root
        call this%preorderhash(tmp)
        ! do while (associated(tmp))
        !     print *, "Sucursal: ", tmp%value%id
        !     call tmp%value%hash%showList
        !     tmp => tmp%right
        ! end do
    end subroutine showTableHashSucursales

    subroutine preorderhash(this, tmp)
        class(avl), intent(in) :: this
        type(node), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        print *, "Sucursal: ", tmp%value%id
        call tmp%value%hash%showList
        print *, "----------------------"
        ! write (*, '(1I3)', advance='no') (tmp%value%id)
        call this%preorderhash(tmp%left)
        call this%preorderhash(tmp%right)
    end subroutine preorderhash

    subroutine graficarTablaHashIdSucursal(this, id)
        class(avl), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: tmp
        character(len=100) :: filename
        tmp => this%searchSucursalID(id)
        if (associated(tmp)) then
            filename = 'sucursal' // trim(adjustl(tmp%value%departamento)) // '.dot'
            call tmp%value%hash%showDataHash()
            call tmp%value%hash%grafico(filename)
        else
            ! write (*, '(A)') 'No se encontro la sucursal ', id
        end if
    end subroutine graficarTablaHashIdSucursal


    subroutine searchTech(this, id)
        class(avl), intent(in) :: this
        integer*8, intent(in) :: id
        type(node), pointer :: tmp
        
        tmp => this%root
        do while (associated(tmp))
            call tmp%value%hash%search(id)
            tmp => tmp%right
        end do
    end subroutine searchTech

    subroutine addTableHash(this, id, tableHash)
        class(avl), intent(inout) :: this
        type(hash), intent(in) :: tableHash
        integer, intent(in) :: id
        type(node), pointer :: tmp
        tmp => this%searchSucursalID(id)
        if (associated(tmp)) then
            tmp%value%hash= tableHash
        else
            ! print *, "No se encontro la sucursal ", id
        end if
    end subroutine addTableHash

    subroutine showTableHash(this, id)
        class(avl), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: tmp
        tmp => this%searchSucursalID(id)
        if (associated(tmp)) then
            call tmp%value%hash%showDataHash()
        else
            ! write (*, '(A)') 'No se encontro la sucursal ', id
        end if
    end subroutine showTableHash

end module

module module_jsonReaderSucursales
    use json_module
    use module_sha256
    use module_AvlSucursales
    use TableHash
    implicit none
    type jsonReaderSucursales
        character(len=50) :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        integer :: size
        logical :: found
    contains
        procedure :: InicialiceJson
        procedure :: getText
        procedure :: getInt
        procedure :: readJsonSucursales
    end type
    contains
    subroutine readJsonSucursales(this,hash_GlobalTable,avl1)
        class(jsonReaderSucursales), intent(inout) :: this
        type(hash), intent(inout):: hash_GlobalTable
        type(avl), intent(inout):: avl1
        type(sucursal) :: s1
        integer :: i,id       ! Se declaran variables enteras
        character(len=50), allocatable :: departamento, direccion, password  ! Se declaran variables de tipo caracter
        integer :: unit
        character(len=100) :: filename
        
        filename = 'images/sucursales.dot'
        open(unit, file=filename, status='replace')

        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        print *, "id | departamento | direccion | password"
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            id = this%getInt(poss=i,text="id") 
            departamento = this%getText(poss=i,text="departamento")
            direccion = this%getText(poss=i,text="direccion")
            password = this%getText(poss=i,text="password")
            password = sha256(password)
            s1 = sucursal(id,0, departamento, direccion, password, hash_GlobalTable)
            call avl1%add(s1)
            print *, id, " | ", departamento, " | ", direccion, " | ", password
        end do
        print *, 'Generating Dot file...'
        call avl1%dotgen(avl1%root, unit)
        close(unit)
        print *, 'Dot file generated:', trim(filename)

        call execute_command_line('dot -Tpng images/sucursales.dot -o images/sucursales.png')
        ! windows
        ! call system("start images\blockchain.png")
        !linux
        call system("eog images/sucursales.png") !si da error se debe de usar "unset GTK_PATH"
    end subroutine

    function getText(this,poss,text) result(valueret)
        class(jsonReaderSucursales), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        character(:), allocatable :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found

        found = .false.
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        if (found) then
            call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'nombre'
        end if
        
    end function getText

    function getInt(this,poss,text) result(valueret)
        class(jsonReaderSucursales), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        integer :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found
        
        found = .false.
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'nombre'
        if (.not.found) then
            ! print *, "No se encontró el valor"
            valueret = 000000000
        end if
    end function getInt

    subroutine InicialiceJson(this)
        class(jsonReaderSucursales), intent(inout) :: this
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=this%filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, this%found)
        if (this%found) then
            print *, "Se encontró el archivo"
        else
            ! print *, "No se encontró el archivo"
        end if
    end subroutine InicialiceJson

end module
