program main
  use module_menu
  implicit none
  type (menu),allocatable :: programa
  allocate(programa)
  call programa%showMenu()
  
end program main