module module_admin_users
    use module_btree
    use module_ordinal_user
    implicit none

    type(BTree) :: arbolB_de_usuarios
    
    type admin_user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    ! contains
    ! procedure :: arbolB_de_usuarios
    ! procedure :: insertar_usuario
    ! procedure :: eliminar_usuario
    ! procedure :: modificar_usuario
    ! procedure :: cargaM_usuarios 
    end type
end module