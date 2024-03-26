module module_admin_users
    use module_btree
    use module_ordinal_user
    implicit none

    
    type admin_user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    type(BTree) :: treeUsers
    contains
    procedure :: arbolB_de_usuarios
    procedure :: insertar_usuario
    procedure :: eliminar_usuario
    procedure :: modificar_usuario
    procedure :: cargaM_usuarios
    end type

    contains
    !crear un arbol B de usuarios
    subroutine arbolB_de_usuarios(this)
        class(admin_user), intent(inout) :: this
        if (associated(this%treeUsers%returnRoot())) then
            print *, "---------------------------------"
            print *, "haciendo arbol de usuarios"
            print *, "---------------------------------"
            call this%treeUsers%graph(myNode=this%treeUsers%returnRoot())
            print *, "---------------------------------"
            print *, "Arbol de usuarios hecho"
            print *, "---------------------------------"
        else
            print *, "---------------------------------"
            print *, "No se ha podido hacer el arbol de usuarios"
            print *, "---------------------------------"
        end if
    end subroutine

    !insertar un usuario en el arbol B de usuarios
    subroutine insertar_usuario(this, nombre, DPI, password)
        class(admin_user), intent(inout) :: this
        character(:),allocatable, intent(in) :: nombre
        integer(kind=8), intent(in) :: DPI  
        character(:),allocatable, intent(in) :: password
        call this%treeUsers%insert(ordinal_user(nombre,DPI, password))
        print *, "---------------------------------"
        print *, "Usuario insertado"
        print *, "---------------------------------"
    end subroutine

    !eliminar un usuario en el arbol B de usuarios
    subroutine eliminar_usuario(this, DPI)
        class(admin_user), intent(inout) :: this
        integer(kind=8), intent(in) :: DPI
        call this%treeUsers%remove(DPI)
        print *, "---------------------------------"
        print *, "Usuario eliminado"
        print *, "---------------------------------"
    end subroutine

    !modificar un usuario en el arbol B de usuarios
    subroutine modificar_usuario(this)
        class(admin_user), intent(inout) :: this
        integer(kind=8):: DPI,option
        character(:),allocatable :: nombre
        character(:),allocatable :: password
        type(ordinal_user), pointer :: user
        logical :: finaly, found
        finaly = .false.
        print *, "---------------------------------"    
        print *, "Ingrese el DPI del usuario a modificar"
        read *, DPI
        print *, "Ingrese la contraseña del usuario"
        read *, password
        user => this%treeUsers%getUser(DPI, myNode=this%treeUsers%returnRoot(), found=found, password=password)
        if (found) then
            !luego de saber que si existe lo vamos a eliminar e insertar de nuevo
            DPI=user%DPI
            nombre=user%name
            password=user%password
            !lo eliminamos
            call this%treeUsers%remove(DPI)
            do while (.not. finaly)
                print *, "---------------------------------"
                print *, "Que desea modificar?"
                print *, "1. Nombre"
                print *, "2. DPI"   
                print *, "3. Contraseña"
                print *, "4. Salir"
                read *, option
                select case(option)
                    case(1)
                        print *, "---------------------------------"
                        print *, "Ingrese el nuevo nombre"
                        read *, nombre
                    case(2)
                        print *, "---------------------------------"
                        print *, "Ingrese el nuevo DPI"
                        read *, DPI
                    case(3)
                        print *, "---------------------------------"
                        print *, "Ingrese la nueva contraseña"
                        read *, password
                    case(4)
                        finaly = .true.
                    case default
                        print *, "---------------------------------"
                        print *, "Opcion no valida"
                        print *, "---------------------------------"
                end select
            end do
            !insertamos el nuevo usuario
            call this%treeUsers%insert(ordinal_user(nombre,DPI, password))
            print *, "---------------------------------"
            print *, "Usuario modificado"
            print *, "---------------------------------"
        else
            print *, "---------------------------------"
            print *, "Usuario no encontrado o contraseña incorrecta"
            print *, "---------------------------------"
        end if
    end subroutine

    subroutine cargaM_usuarios(this)
        class(admin_user), intent(inout) :: this
    !Carga los usuarios del archivo de usuarios
    end subroutine
end module

module module_jsonReader_users
    use json_module
    implicit none
    
    type, public :: jsonReader
        character(:), allocatable :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        logical :: found
        integer :: size
        contains
        procedure :: readJson
        procedure :: InicialiceJson
        procedure :: getText
    end type jsonReader

    contains
    
    subroutine readJson(this, filename)
        class(jsonReader), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer :: i        ! Se declaran variables enteras
        character(len=50), allocatable :: dpi, nombre_cliente, password
        logical :: found
        type(json_value), pointer :: actualchild

        !con esto vamos a iniciar el json
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, this%found)
        if (this%found) then
            print *, "Se encontro el archivo"
            do i = 1, this%size         ! Se inicia un bucle sobre el número de elementos en el JSON
                dpi = this%getText(poss=i,text="dpi")
                nombre_cliente = this%getText(poss=i,text="nombre_cliente")
                password = this%getText(poss=i,text="password")
                print *, "DPI: ", dpi
                print *, "Nombre: ", nombre_cliente
                print *, "Password: ", password
            end do
        else
            print *, "No se encontro el archivo"
        end if
    end subroutine

    function getText(this,poss,text) result(valueret)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        character(:), allocatable :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found

        found = .false.
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        if (found) then
            call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'nombre'
        end if
        
    end function getText

    subroutine InicialiceJson(this)
        class(jsonReader), intent(inout) :: this
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=this%filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, this%found)
        if (this%found) then
            print *, "Se encontro el archivo"
        else
            print *, "No se encontro el archivo"
        end if
    end subroutine InicialiceJson
end module module_jsonReader_users