program main
    use jsonReaderModule
    implicit none
    type(jsonReader) :: reader
    reader%filename = "data.json"
    call reader%readJson()
    
end program main