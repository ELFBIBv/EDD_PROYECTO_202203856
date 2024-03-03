program main
    use jsonReaderModule
    use menumodule
    implicit none
    ! type(jsonReader) :: reader
    ! reader%filename = "data.json"
    ! call reader%readJson()
    type(menu) :: varmanu
    call varmanu%printMenu()
end program main