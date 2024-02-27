module jsonReaderModule
    use json_module
    implicit none
    
    type, public :: jsonReader
        character(:), allocatable :: filename
        type(json_file) :: json
        contains
            procedure :: readJson
    end type jsonReader


    contains
    
    subroutine readJson(this,rute)
        class(jsonReader), intent(inout) :: this
        character(len=*), intent(in) :: rute
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        !el json_core funciona para acceder a las funciones basicas de la libreria
        type(json_core) :: jsonc

        character(:), dimension(4) :: requests
        integer, allocatable :: id
        character(:), allocatable :: nombre
        integer, allocatable :: img_g
        integer, allocatable :: img_p

        integer :: i, size
        logical :: found

        call json%initialize()
        call json%load(filename=rute)
        call json%info('',n_children=size) !obtenemos la cantidad de elementos en el json

        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do i = 1, size
            !call jsonc%get_child(listPointer, i, personPointer, found = found)
            !call jsonc%get_child(la madre (por asi decirlo), el numero de hijo, la variable donde se va a guardar, found = found)
            if (found) then
                print *, "ID:", json(i)%get("id")
                print *, "Nombre:", json(i)%get("nombre")
                print *, "Imagen en Google:", json(i)%get("img_g")
                print *, "Imagen en Pinterest:", json(i)%get("img_p")
            end if
        end do

        call json%destroy()
    
        
    end subroutine readJson

end module jsonReaderModule