program main
    use jsonReaderModule
    implicit none
    type(jsonReader) :: reader
    call reader%readJson('data.json')
end program main