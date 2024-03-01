module menumodule
    use jsonReaderModule
    implicit none

    type, public :: menu
    contains
    procedure :: printMenu
    procedure :: parametrosIniciales
    procedure :: ejecutarPaso
    procedure :: estadosEnMemoria
    procedure :: reportes
    procedure :: acercaDe
    procedure :: salir
    end type menu

    contains

    !Imprimir menu

    subroutine printMenu(this)
        class(menu), intent(inout) :: this
        integer :: choice
        logical :: salir = .true.

        do while (salir)
            print *, "--------Menu--------"
            print *, "1. Parametros iniciales"
            print *, "2. Ejecutar paso"
            print *, "3. Estados en memoria de las estructuras"
            print *, "4. Reportes"
            print *, "5. Acerca de"
            print *, "6. Salir"
            read *, choice
            if (choice >= 1 .and. choice <= 6) then
                salir = .false.
            else
                print *, "Opcion no valida"
            end if
        end do

        select case (choice)
            case (1)
                call this%parametrosIniciales()
            case (2)
                call this%ejecutarPaso()
            case (3)
                call this%estadosEnMemoria()
            case (4)
                call this%reportes()
            case (5)
                call this%acercaDe()
            case (6)
                call this%salir()
            case default
                print *, "Opcion no valida"
        end select
        return

    end subroutine

    !Opciones del menú
    subroutine parametrosIniciales(this)
        type(menu), intent(inout) :: this
        character(len=1) :: opcion
        logical :: salir = .true.
        type(jsonReader) :: reader
        do while (.true.)
            print *, "--------Menu de cargas--------"
            print *, "a. Carga masiva de clientes"
            print *, "b. Carga de ventanillas"
            read *, opcion
            if (opcion == "a" .or. opcion == "b") then
                salir = .false.
                reader%filename = "data.json"
                call reader%InicialiceJson()
                call reader%readJson()
                do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                    id = this%getInt(poss=i,text="id") 
                    !nombre = this%getText(poss=i,text="nombre") 
                    img_b = this%getInt(poss=i,text="img_g") 
                    img_s = this%getInt(poss=i,text="img_p")
                    
                    print *,id,nombre,img_b,img_s
                end do
            else
                print *, "Opcion no valida"
            end if
        end do
    end subroutine

    subroutine ejecutarPaso(this)
        type(menu), intent(inout) :: this
        print *, "Ejecutar paso"
    end subroutine

    subroutine estadosEnMemoria(this)
        type(menu), intent(inout) :: this
        print *, "Estados en memoria de las estructuras"
    end subroutine

    subroutine reportes(this)
        type(menu), intent(inout) :: this
        print *, "Reportes"
    end subroutine

    subroutine acercaDe(this)
        type(menu), intent(inout) :: this
        print *, "--------Datos del estudiante:--------"
        print *, "Nombre: Alvaro Josue Morales Rodriguez"
        print *, "Carnet: 202203856"
        print *, "Curso: Estructura de datos"
        print *, "Seccion: "
        print *, "Año: 2020"
        print *, "-------------------------------------"
    end subroutine

    subroutine salir(this)
        type(menu), intent(inout) :: this
        print *, "Salir"
    end subroutine

end module menumodule