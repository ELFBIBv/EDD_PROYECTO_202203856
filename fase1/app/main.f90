program main
    use jsonReaderModule
    use menumodule
    implicit none
    type(menu) :: varmanu
    call varmanu%printMenu()
end program main

!en la lista de espera es donde voy a contar los pasos necesarios para que una persona se retire
!