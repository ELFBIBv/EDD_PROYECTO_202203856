program sha256test
    use module_sha256
    implicit none
    integer :: i
    character(len=256) :: hash
    character(len=256) :: input
    input = "abc"

    hash= sha256(input)
    print *, hash   
end program sha256test