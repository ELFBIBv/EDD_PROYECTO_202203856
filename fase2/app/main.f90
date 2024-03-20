program main
  use module_jsonReader
  implicit none
  ! print *, "Hello World"
  type(jsonReader) :: reader
  reader%filename = "ImagenMario.json"
  ! call reader%InicialiceJson
  call reader%readJson
end program main
