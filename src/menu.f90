module menumodule
    use jsonReaderModule
    use clientQueueModule
    use windowsModule
    use waitingListModule
    implicit none

    type, public :: menu
    private
    type(clientQueue) :: clientQueue
    type(window_linked_list) :: windowslist
    type(waitingList) :: waitingList
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
                salir = .false.
            case default
                print *, "Opcion no valida"
            end select
            return
        end do
    end subroutine

    !Opciones del menú
    subroutine parametrosIniciales(this)
        type(menu), intent(inout) :: this
        type(jsonReader) :: reader
        character(len=50) :: nombre
        character(len=1) :: opcion
        integer :: id, img_b, img_s, i, j
        logical :: salir = .true.

        do while (salir)
            print *, "--------Menu de cargas--------"
            print *, "a. Carga masiva de clientes"
            print *, "b. Carga de ventanillas"
            print *, "c. Regresar"
            read *, opcion
            if (opcion == "a") then
                print *, "Carga masiva de clientes"
                salir = .false.
                reader%filename = "data.json"
                call reader%InicialiceJson()
                call reader%readJson()
                do i = 1, reader%size
                    id = reader%getInt(poss=i,text="id") 
                    nombre = trim(reader%getText(poss=i,text="nombre")) 
                    img_b = reader%getInt(poss=i,text="img_g") 
                    img_s = reader%getInt(poss=i,text="img_p")
                    print *,id,nombre,img_b,img_s
                    call this%clientQueue%append(id,nombre,img_b,img_s)
                end do
                this%clientQueue%print()
            else if (opcion == "b") then
                salir = .false.
                print *, "Carga de ventanillas"
                print *, "Ingrese el numero de ventanillas"
                read *, i
                do j = 1, i
                    call this%windowslist%addWindow(j)
                end do
                this%windowslist%printWindows()
            else if (opcion == "c") then
                salir = .false.
            else
                print *, "Opcion no valida"
            end if
        end do
    end subroutine

    subroutine ejecutarPaso(this)
        type(menu), intent(inout) :: this
        print *, "Ejecutar paso"
        call this%clientQueue%addSteps()
        call this%windowslist%checkOutWindows()
        call this%waitingList%addWaitingList()
        !tengo que llamarlas bien
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