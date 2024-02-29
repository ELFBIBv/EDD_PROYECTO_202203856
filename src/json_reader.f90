module jsonReaderModule
    use json_module
    implicit none
    
    type, public :: jsonReader
        character(:), allocatable :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
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
        character(len=100) :: nombre
        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            id = this%getInt(poss=i,text="id") 
            nombre = this%getText(poss=i,text="nombre") 
            img_b = this%getInt(poss=i,text="img_g") 
            img_s = this%getInt(poss=i,text="img_p")
            print *,id,nombre,img_g,img_p
        end do
    
    end subroutine
    
    function getText(this,poss,text) result(value)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        character(:), allocatable :: value
        integer :: i, size        ! Se declaran variables enteras
        logical :: found
        
        
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        call this%jsonc%get(this%attributePointer, value)  ! Se obtiene el valor y se asigna a la variable 'nombre'
    end function getText

    function getInt(this,poss,text) result(value)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        integer :: value
        integer :: i, size        ! Se declaran variables enteras
        logical :: found
        
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        call this%jsonc%get(this%attributePointer, value)  ! Se obtiene el valor y se asigna a la variable 'nombre'
    end function getInt

    subroutine InicialiceJson(this)
        class(jsonReader), intent(inout) :: this
        logical :: found
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=this%filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, found)
    end subroutine InicialiceJson
end module jsonReaderModule