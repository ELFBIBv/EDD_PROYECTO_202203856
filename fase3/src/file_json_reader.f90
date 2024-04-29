module jsonReaderModule
    use json_module
    use tech_hash
    use AvlSucursales
    implicit none
    
    type, public :: jsonReader
        character(:), allocatable :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        type (hash) :: hash_table
        logical :: found
        integer :: size
        contains
        procedure :: readJsonTecnicos
        procedure :: readJsonSucursales
        procedure :: initHashTable
        procedure :: printHashTable
        
        ! procedure :: readJsonGrafos
        procedure :: getText
        procedure :: getInt
        procedure :: InicialiceJson
    end type jsonReader

    contains
    

    subroutine initHashTable(this)
        class(jsonReader), intent(inout) :: this
        call this%hash_table%init(7,30,70)
    end subroutine

    subroutine readJsonTecnicos(this)
        class(jsonReader), intent(inout) :: this
        integer :: i
        integer*8 :: dpiInt       ! Se declaran variables enteras
        character(13) :: dpi
        
       
        character(len=50), allocatable :: nombre, apellido, genero, direccion, telefono  ! Se declaran variables de tipo caracter
        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        call this%initHashTable()
        print *, "dpi | nombre | apellido | genero | direccion | telefono"
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            dpi = this%getText(poss=i,text="dpi") 
            nombre = this%getText(poss=i,text="nombre") 
            apellido = this%getText(poss=i,text="apellido")
            genero = this%getText(poss=i,text="genero")
            direccion = this%getText(poss=i,text="direccion")
            telefono = this%getText(poss=i,text="telefono")

            
            read(dpi,*) dpiInt
            !print*,"Nnumero transformado", dpiInt
           call this%hash_table%insert(tech(dpiInt, nombre, apellido, genero, direccion, telefono))
            !print *, dpi, " | ", nombre, " | ", apellido, " | ", genero, " | ", direccion, " | ", telefono
        end do
    end subroutine

    subroutine printHashTable(this)
        class(jsonReader), intent(inout) :: this
        print *, "Tabla de hash:"
        call this%hash_table%show()
    end subroutine


    subroutine readJsonSucursales(this)
        class(jsonReader), intent(inout) :: this
        type(sucursal) :: s1
        type(avl) :: avl1
        integer :: i,id       ! Se declaran variables enteras
        character(len=50), allocatable :: departamento, direccion, password  ! Se declaran variables de tipo caracter
        
        !Graph
        integer :: unit
    	character(len=100) :: filename
	    filename = 'output.dot'
    	open(unit, file=filename, status='replace')

        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        print *, "id | departamento | direccion | password"
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            id = this%getInt(poss=i,text="id") 
            departamento = this%getText(poss=i,text="departamento")
            direccion = this%getText(poss=i,text="direccion")
            password = this%getText(poss=i,text="password")
            s1 = sucursal(id,0, departamento, direccion, password)
            call avl1%add(s1)
            print *, id, " | ", departamento, " | ", direccion, " | ", password
        end do
        print *, 'Generating Dot file...'
        call avl1%dotgen(avl1%root, unit)
        close(unit)
        print *, 'Dot file generated:', trim(filename)
        call execute_command_line('dot -Tsvg output.dot > output.svg')
        call execute_command_line('eog output.svg')

    end subroutine

    ! subroutine readJsonGrafos(this)
    !     class(jsonReader), intent(inout) :: this
    !     integer :: i, id, s1, s2, distancia, imp_mantenimiento, num_elementos
    !     call this%InicialiceJson()  ! Se inicializa el módulo JSON
        
    !     num_elementos = this%size("grafo")
    !     print *, "s1 | s2 | distancia | imp_mantenimiento"
    !     do i = 1, num_elementos  ! Se inicia un bucle sobre el número de elementos en el JSON
    !         s1 = this%getInt("grafo", i, "s1")
    !         s2 = this%getInt("grafo", i, "s2")
    !         distancia = this%getInt("grafo", i, "distancia")
    !         imp_mantenimiento = this%getInt("grafo", i, "imp_mantenimiento")
    !         print *, s1, " | ", s2, " | ", distancia, " | ", imp_mantenimiento
    !     end do
    ! end subroutine
    

    
    function getText(this,poss,text) result(valueret)
        class(jsonReader), intent(inout) :: this
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
        class(jsonReader), intent(inout) :: this
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
            print *, "No se encontró el valor"
            valueret = 000000000
        end if
    end function getInt

    subroutine InicialiceJson(this)
        class(jsonReader), intent(inout) :: this
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=this%filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, this%found)
        if (this%found) then
            print *, "Se encontró el archivo"
        else
            print *, "No se encontró el archivo"
        end if
    end subroutine InicialiceJson
end module