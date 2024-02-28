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
        class(json_file), allocatable :: json
        character(len=*), intent(in) :: rute
        character(len=:), allocatable :: json_string
        integer :: status
        real(kind=json_rk) :: real_value1, real_value2, real_value3
        character(len=50) :: nombre
        integer :: edad
        character(len=20) :: telefono
        json = this%json

        call json%load('data2.json', status)
        if (status == 0) then

            CALL json%get('nombre', real_value1)
            CALL json%get('edad', real_value2)
            CALL json%get('contacto/telefono', real_value3)

            print *, "Nombre: ", real_value1
            print *, "Edad: ", real_value2
            print *, "Telefono: ", real_value3
        end if
    end subroutine
    

end module jsonReaderModule