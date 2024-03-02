program main
    use jsonReaderModule
    use menumodule
    implicit none
    ! type(jsonReader) :: reader
    ! reader%filename = "data.json"
    ! call reader%readJson()
    type(menumodule) :: menu
    call menu%menu()
end program main