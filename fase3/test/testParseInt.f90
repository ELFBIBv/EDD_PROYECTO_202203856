program char_to_real
    implicit none
    character(len=13) :: string
    integer*8 :: realnum
    string = "1234567891012"
    read(string,*) realnum
    print*, realnum
end program char_to_real