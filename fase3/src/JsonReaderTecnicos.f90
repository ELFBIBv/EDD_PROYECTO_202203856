
module module_jsonReaderTecnicos
    use json_module
    ! use TableHash
    use module_AvlSucursales
    implicit none
    
    type jsonReaderTecnicos
            character(len=50) :: filename
            type(json_file) :: json
            type(json_value), pointer :: listPointer, personPointer, attributePointer
            type(json_core) :: jsonc
            integer :: size
            logical :: found
        contains
            procedure :: readJsonTecnicos
            procedure :: InicialiceJson
            procedure :: getText
            procedure :: getInt
    end type jsonReaderTecnicos

    contains

    subroutine readJsonTecnicos(this, idSucursal,hash_table,avl1)
        class(jsonReaderTecnicos), intent(inout) :: this
        !leer idSucursal
        integer, intent(in) :: idSucursal
        type(hash), intent(inout) :: hash_table
        type(avl), intent(inout) :: avl1
        integer :: i
        integer*8 :: dpiInt       ! Se declaran variables enteras
        character(13) :: dpi
        
        character(len=50), allocatable :: nombre, apellido, genero, direccion, telefono  ! Se declaran variables de tipo caracter
        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        call hash_table%init(7,30,70)
        print *, "IdSucursal: ", idSucursal
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
           call hash_table%insert(hashTable(dpiInt, nombre, apellido, genero, direccion, telefono,0))
            !print *, dpi, " | ", nombre, " | ", apellido, " | ", genero, " | ", direccion, " | ", telefono
        end do

        call avl1%addTableHash(idSucursal, hash_table)
        print *, "Tabla de hash desde el avl:"
        call avl1%showTableHash(idSucursal)

    end subroutine

    function getText(this,poss,text) result(valueret)
        class(jsonReaderTecnicos), intent(inout) :: this
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
        class(jsonReaderTecnicos), intent(inout) :: this
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
        class(jsonReaderTecnicos), intent(inout) :: this
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