
module module_jsonReader_users
    use json_module
    use module_btree
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
        procedure :: getInt 
    end type jsonReader

    contains
    
    subroutine readJson(this, filename,treeUsers)
        class(jsonReader), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(BTree), intent(inout) :: treeUsers
        integer :: i        ! Se declaran variables enteras
        integer(kind=8) :: dpi
        character(len=50), allocatable :: nombre_cliente, password!,dpi
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
                dpi = this%getInt(poss=i,text="dpi")
                nombre_cliente = this%getText(poss=i,text="nombre_cliente")
                password = this%getText(poss=i,text="password")
                ! print *, "DPI: ", dpi
                ! print *, "Nombre: ", nombre_cliente
                ! print *, "Password: ", password
                call treeUsers%insert(ordinal_user(name=nombre_cliente, DPI=dpi, password=password))
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
            return
        else
            print *, "No se encontró el valor"
        end if
    end function getText

    function getInt(this,poss,text) result(valueret)
        class(jsonReader), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        character(:), allocatable :: valuerettemp
        integer(kind=8) :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found
        
        found = .false.
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual

        if (found) then
            call this%jsonc%get(this%attributePointer, valuerettemp)  ! Se obtiene el valor y se asigna a la variable 'nombre'
            read(valuerettemp, *) valueret
            return
        else
            print *, "No se encontró el valor"
            valueret = 00000
        end if
    end function getInt

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

module module_admin_user
    use module_btree
    use module_ordinal_user
    use module_jsonReader_users
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
    procedure :: carga_masiva_usuarios
    procedure :: reportes_de_admin_user
    end type

    contains
    !crear un arbol B de usuarios
    subroutine arbolB_de_usuarios(this)
        class(admin_user), intent(inout) :: this
        print *, "---------------------------------"
        print *, "haciendo arbol de usuarios"
        print *, "---------------------------------"
        call this%treeUsers%graphBTree(myNode=this%treeUsers%returnRoot())
        print *, "---------------------------------"
        print *, "Arbol de usuarios hecho"
        print *, "---------------------------------"
    end subroutine

    !insertar un usuario en el arbol B de usuarios
    subroutine insertar_usuario(this)
        class(admin_user), intent(inout) :: this
        type(ordinal_user) :: user
        character(50) :: nombre
        integer(kind=8) :: DPI  
        character(50) :: password
        print *, "---------------------------------"
        print *, "Ingrese el nombre del usuario"
        read *, nombre
        print *, "Ingrese el DPI del usuario (numeros)"
        read *, DPI
        print *, "Ingrese la contraseña del usuario"
        read *, password
        if (DPI<9999999999999_8 .and. DPI>999999999999_8) then
            user = ordinal_user(nombre, DPI, password)
            call this%treeUsers%insert(user)
            print *, "---------------------------------"
            print *, "Usuario insertado"
            print *, "---------------------------------"
        else
            print *, "---------------------------------"
            print *, "DPI no valido"
            print *, "---------------------------------"
        end if
    end subroutine

    !eliminar un usuario en el arbol B de usuarios
    subroutine eliminar_usuario(this)
        class(admin_user), intent(inout) :: this
        type(ordinal_user), pointer :: user
        integer(kind=8) :: DPI
        print *, "---------------------------------"
        print *, "Ingrese el DPI del usuario a eliminar (numeros)"
        read *, DPI
        user => this%treeUsers%getUserAdmin(DPI=DPI, myNode=this%treeUsers%root)
        if (associated(user)) then
            call this%treeUsers%remove(DPI)
            print *, "---------------------------------"
            print *, "Usuario eliminado"
            print *, "---------------------------------"
        else
            print *, "---------------------------------"
            print *, "Usuario no encontrado"
            print *, "---------------------------------"
        end if
    end subroutine

    !modificar un usuario en el arbol B de usuarios
    subroutine modificar_usuario(this)
        class(admin_user), intent(inout) :: this
        integer(kind=8):: DPI,option
        character(:),allocatable :: nombre
        character(50) :: password
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

    subroutine carga_masiva_usuarios(this)
        class(admin_user), intent(inout) :: this
        type(jsonReader) :: json
        character(:), allocatable :: filename
        print *, "---------------------------------"
        print *, "cargando usuarios"
        print *, "---------------------------------"

        call json%readJson("usuarios.json", this%treeUsers)
        
        print *, "---------------------------------"
        print *, "usuarios cargados"
        print *, "---------------------------------"
        
    end subroutine

    subroutine reportes_de_admin_user(this)
        class(admin_user), intent(inout) :: this
        type(ordinal_user), pointer :: user
        integer :: option,counter
        integer(kind=8) :: DPI
        logical :: finaly
        finaly = .false.
        do while (.not. finaly)
            print *, "---------------------------------"
            print *, "reportes de admin_user"
            print *, "---------------------------------"
            print *, "1. Informacion de usuario a elegir"
            print *, "2. Listar clientes por recorrido de nivel"
            print *, "3. salir"
            read *, option
            select case(option)
                case(1)
                    print *, "---------------------------------"
                    print *, "Ingrese el DPI del usuario a buscar"
                    read *, DPI
                    user => this%treeUsers%getUserAdmin(DPI=DPI, myNode=this%treeUsers%root)
                    if (associated(user)) then
                        print *, "---------------------------------"
                        print *, "Nombre: ", user%name
                        print *, "DPI: ", user%DPI
                        print *, "Password: ", user%password
                        print *, "---------------------------------"
                        counter = user%albums%cuantitiAlbums()
                        print *, "cantidad de albumes: ", counter
                        counter = user%albums%cuantitiImagesInAlbums()
                        print *, "cantidad de imagenes en albumes: ", counter
                        print *, "---------------------------------"
                        print *, "cantidad de imagenes en total: ", user%ImagesTree%num_images
                        print *, "cantidad de capas en total: ", user%LayersTree%num_layers
                    else
                        print *, "---------------------------------"
                        print *, "Usuario no encontrado"
                        print *, "---------------------------------"
                    end if
                case(2)
                    print *, "---------------------------------"
                    print *, "Listando clientes por recorrido de nivel"
                    print *, "---------------------------------"
                    call this%treeUsers%breadthFirstMatrix_adminReport()
                case(3)
                    finaly = .true.
                case default
                    print *, "---------------------------------"
                    print *, "Opcion no valida"
                    print *, "---------------------------------"
            end select
        end do
    end subroutine reportes_de_admin_user

end module