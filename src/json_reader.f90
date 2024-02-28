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
        type(json_file) :: json
        character(len=:), allocatable :: json_string
        integer :: status
        character(len=50) :: nombre
        integer :: edad
        character(len=20) :: telefono

        call json%load(rute, status)

        if (status == 0) then
            json_string = json%to_string()

            call json%get("nombre", nombre)
            call 

    end subroutine readJson

end module jsonReaderModule