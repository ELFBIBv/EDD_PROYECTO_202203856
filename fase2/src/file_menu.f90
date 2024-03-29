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
        class(ordinal_user),pointer :: user
        type(btree),pointer :: tree
        logical :: salir = .true.
        logical :: found
        this%admin = admin_user(name="admin", DPI=1234567890123_8, password="admin")
        do while (salir)
            print *, "-----------------inicio de sesion-----------------"
            print *, "ingrese su DPI"
            read *, DPI
            print *, DPI
            print *, "ingrese su contraseña"
            read *, password
            if (DPI == this%admin%DPI .and. password == this%admin%password) then
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
                tree = this%admin%treeUsers%returnRoot()
                found = .false.
                user => tree%getUser(DPI=DPI, password=password,myNode=tree, found=found)
                print   *, "found: ", found
                if (found) then
                    print *, "Bienvenido , ", user%name
                    salir = .false.
                else
                    print *, "DPI incorrecto o contraseña incorrecta"
                end if
            end if
        end do
    end subroutine

end module module_menu