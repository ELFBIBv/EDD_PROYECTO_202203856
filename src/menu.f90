module menumodule
    implicit none

    pun
    type, public :: menu
        private
        integer :: choice
    contains
        procedure :: process
        procedure :: printMenu
    end type menu

    contains

    !Imprimir menu

    subroutine printMenu(this)
        class(menu), intent(inout) :: this
        integer :: choice
        print *, "1. Parametros iniciales"
        print *, "2. Ejecutar paso"
        print *, "3. Estados en memoria de las estructuras"
        print *, "4. Reportes"
        print *, "5. Acerca de"
        print *, "6. Salir"
        read *, choice
        read
        return 
     end subroutine

end module menumodule