module jsonReaderModule
    use json_module
    implicit none
    
    type, public :: jsonReader
        character(:), allocatable :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        logical :: found
        integer :: size
        contains
        procedure :: readJson
        procedure :: getText
        procedure :: getInt
        procedure :: InicialiceJson
    end type jsonReader

    contains
    
    subroutine readJson(this)
        class(jsonReader), intent(inout) :: this
        integer :: i,id,img_b,img_s        ! Se declaran variables enteras
        character(len=50), allocatable :: nombre
        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        print *, "id | nombre | img_g | img_p"
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            id = this%getInt(poss=i,text="id") 
            nombre = this%getText(poss=i,text="nombre") 
            img_b = this%getInt(poss=i,text="img_g") 
            img_s = this%getInt(poss=i,text="img_p")
            print *,id,nombre,img_b,img_s
        end do
    end subroutine
    
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
            valueret = 00000
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
end module jsonReaderModule