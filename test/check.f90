program main
    use jsonReaderModule
    implicit none
    type(jsonReader) :: reader
    call reader%readJson('data2.json')
end program main