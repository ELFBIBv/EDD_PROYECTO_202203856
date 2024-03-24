module module_jsonReader
    use json_module
    use module_layers
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
        procedure :: getChild
        procedure :: InicialiceJson
    end type jsonReader

    contains
    
    subroutine readJson(this,matriz)
        class(jsonReader), intent(inout) :: this
        integer :: i,j,id,no_childs,fila, columna        ! Se declaran variables enteras
        character(len=50), allocatable :: color
        logical :: found
        type(json_value), pointer :: actualchild,actualsubchild,pixelatribute
        type(matrix), intent(inout) :: matriz
        call this%InicialiceJson()  ! Se inicializa el módulo JSON
        do i = 1, this%size         ! Se inicia un bucle sobre el número de elementos en el JSON
            call this%jsonc%get_child(this%listPointer, i,actualchild, found = found)
            if (found) then
                id = this%getInt(poss=i,text="id_capa",actualchild=actualchild)
                if (.not.found) then
                    print *, "No se obtuvo el id_capa, poss: ", i
                    id = 00000
                end if
                print *,"id_capa: ",id

                call this%jsonc%get_child(actualchild,"pixeles",actualsubchild,found=found)
                call this%jsonc%info(actualsubchild,n_children=no_childs)

                if (.not.found) then
                    print *, "No se obtuvo el pixeles, poss: ", i
                end if
                do j= 1, no_childs
                    call this%jsonc%get_child(actualsubchild,j,pixelatribute)
                    fila = this%getInt(poss=j,text="fila",actualchild=pixelatribute)
                    columna = this%getInt(poss=j,text="columna",actualchild=pixelatribute)
                    color = this%getText(poss=j,text="color",actualchild=pixelatribute)
                    call matriz%insert(i=fila,j=columna,color=color)
                end do
            end if
        end do
    end subroutine

    
    function getInt(this,poss,text,actualchild) result(valueret)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        type(json_value),intent(in),pointer :: actualchild
        character(len=*), intent(in) :: text
        integer :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found
        
        found = .false.
        call this%jsonc%get_child(actualchild, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'text' del hijo actual
        call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'valueret'
        if (.not.found) then
            print *, "No se obtuvo el numero, poss: ", poss
            valueret = 00000
        end if
    end function getInt

    function getChild(this,poss, actualchild) result(valueret)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        type(json_value), intent(in),pointer :: actualchild
        type(json_value), pointer :: valueret
        logical :: found
        call this%jsonc%get_child(actualchild, poss, valueret, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        if (.not.found) then
            print *, "hubo un error al obtener el hijo: ",poss
        end if
    end function getChild
    
    function getText(this,poss,text,actualchild) result(valueret)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        type(json_value), intent(in),pointer :: actualchild
        character(len=*), intent(in) :: text
        character(:), allocatable :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found

        found = .false.
        call this%jsonc%get_child(actualchild, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'text' del hijo actual
        if (.not.found) then
            print *, "No se obtuvo el texto, poss:", poss 
            valueret = "error"
        end if
        call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'valueret'
        
    end function getText
    
    subroutine InicialiceJson(this)
        class(jsonReader), intent(inout) :: this
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=this%filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, this%found)
        if (this%found) then
            print *, "Se encontro el archivo"
        else
            print *, "No se encontro el archivo"
        end if
    end subroutine InicialiceJson
end module module_jsonReader