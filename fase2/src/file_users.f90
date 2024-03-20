module module_users
    implicit none
    type admin_user
        character(:), allocatable :: name
        character(:), allocatable :: DPI
        character(:), allocatable :: password
        contains
            procedure :: ver_reportes_estructuras
            procedure :: navegacion_imgs
            procedure :: gestion_imgs
            
    end type

    type ordinal_user
        character(:), allocatable :: name
        character(:), allocatable :: DPI
        character(:), allocatable :: password
        contains
        procedure :: arbolB_de_usuarios
        procedure :: insertar_usuario
        procedure :: eliminar_usuario
        procedure :: modificar_usuario
        procedure :: cargaM_usuarios 
    end type



end module