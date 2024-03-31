module module_menu
    use module_ordinal_user
    use module_btree
    use module_admin_user
    implicit none

    type, public :: menu
        type(admin_user) :: admin
    contains
        procedure :: showmenu
    end type menu

    contains

    !Imprimir menu

    subroutine showmenu(this)
        class(menu), intent(inout) :: this
        integer(kind=8) :: DPI, option
        character(len=50) :: password
        character(len=13) :: entrada
        class(ordinal_user),pointer :: user
        type(btree),pointer :: tree
        logical :: salir,salirprincipal = .true.
        logical :: found
        this%admin = admin_user(name="admin", DPI=1234567890123_8, password="EDD2024")
        do while (salirprincipal)
            print *, "-----------------Bienvenido-----------------"
            print *, "1. Iniciar sesion"
            print *, "2. Registro de usuario"
            print *, "3. Salir"
            read *, option
            select case(option)
                case(1)
                    salir = .true.
                    do while (salir)
                        print *, "-----------------inicio de sesion-----------------"
                        print *, "ingrese su usuario (DPI)"
                        read *, entrada
                        print *, "ingrese su contraseña"
                        read *, password
                        if (entrada==this%admin%name .and. password == this%admin%password) then
                            print *, "Bienvenido admin, ", this%admin%name
                            do while (salir)
                                print *, "1. arbolB de usuarios"
                                print *, "2. insertar usuario"
                                print *, "3. eliminar usuario"
                                print *, "4. modificar usuario"
                                print *, "5. carga masiva de usuarios"
                                print *, "6. salir"
                                read *, option
                                select case(option)
                                    case(1)
                                        call this%admin%arbolB_de_usuarios()
                                    case(2)
                                        call this%admin%insertar_usuario()
                                    case(3)
                                        call this%admin%eliminar_usuario()
                                    case(4)
                                        call this%admin%modificar_usuario()
                                    case(5)
                                        call this%admin%carga_masiva_usuarios()
                                    case(6)
                                        salir = .false.
                                    case default
                                        print *, "opcion no valida"
                                    end select
                                end do
                                salir = .true.
                        else
                            tree => this%admin%treeUsers%root
                            if (.not. associated(tree)) then
                                print *, "no hay usuarios registrados"
                                exit
                            end if
                            print *, "buscando usuario"
                            found = .false.
                            read(entrada, '(I13)') DPI
                            if ( .not. (DPI>999999999999_8 .and. DPI<=9999999999999_8)) then
                                print *, "DPI o contraseña incorrecta"
                                exit
                            end if
                            user => tree%getUser(DPI=DPI, password=password,myNode=tree, found=found)
                            if (found) then
                                print *, "Bienvenido , ", user%name
                                do while (salir)
                                    print *, "-----------------menu de usuario-----------------"
                                    print *, "1. visualizar reportes de las estructuras"
                                    print *, "2. navegacion y gestion de imagenes"
                                    print *, "3. opciones de carga masiva"
                                    print *, "4. salir"
                                    read *, option
                                    select case(option)
                                        case(1)
                                            call user%ver_reportes_estructuras()
                                        case(2)
                                        case(3)
                                            print *, "-----------------carga masiva-----------------"
                                            print *, "1. carga masiva de capas"
                                            print *, "2. carga masiva de imagenes"
                                            print *, "3. carga masiva de albumes"
                                            read *, option
                                            select case(option)
                                            case(1)
                                                call user%carga_masiva_capas()
                                            case(2)
                                                call user%carga_masiva_imagenes()
                                            case(3)
                                                call user%carga_masiva_albumes()
                                            case default
                                                print *, "opcion no valida"
                                            end select
                                        case(4)
                                            salir = .false.
                                        case default
                                            print *, "opcion no valida"
                                    end select
                                end do
                                salir = .true.
                            else
                                print *, "DPI incorrecto o contraseña incorrecta"
                                salir = .false.
                            end if
                        end if
                    end do
                case(2)
                    call this%admin%insertar_usuario()
                case(3)
                    salirprincipal = .false.
                case default
                    print *, "opcion no valida"
                end select
        end do
    end subroutine

end module module_menu