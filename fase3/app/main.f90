program main
  use module_jsonReaderGrafos
  use module_jsonReaderTecnicos
  use module_jsonReaderSucursales
  use module_sha256
  implicit none
  
  type(avl) :: avl1
  type(merkle) :: tree 
  type(graph), pointer :: graph_read
  type(analyzer) :: analizador !a
  type(result_list), pointer :: list_Result
  type(hash) :: hash_table, hash_GlobalTable

  type(jsonReaderSucursales) :: readersucursales
  type(jsonReaderTecnicos) :: readertecnico
  type(jsonReaderGrafos) :: readergrafos

  integer :: option, idSucursalInicio, idSucursalFin
  integer*8 :: dpi

  
  character(len=100) :: usuarioDefault, usuario, filename
  character(len=100) :: passwordDefault, password
  usuarioDefault = "EDD1S2024"
  passwordDefault = "ProyectoFase3"

  passwordDefault = sha256(passwordDefault)

  print *, "Bienvenido"
  ! call reader%initGlobalHashTable()
  call hash_GlobalTable%init(7,30,70)
  do while(.true.)
    print *, "1. Iniciar Sesion"
    print *, "2. Salir"
    read (*, *) option
    select case(option)
      case(1)
        print *, "Ingrese su usuario"
        read (*, *) usuario
        print *, "Ingrese su contrasena"
        read (*, *) password
        password = sha256(password)
        if (usuario == usuarioDefault .and. password == passwordDefault) then
          print *, "Bienvenido ", usuario
          call menu()
        else
          print *, "Usuario o contrasena incorrecta"
        end if

      case(2)
        exit
      case default
        print *, "Opcion no valida, intente de nuevo"
    end select
  
end do

contains
  

    subroutine menu()
      do while(.true.)
        print *, "1. Carga masiva de archivos"
        print *, "2. Ver informacion tecnico"
        print *, "3. Generar recorrido mas largo"
        print *, "4. Generar reportes"
        print *, "5. Salir"
        read (*, *) option
    
        select case(option)
          case(1)
            print *, "1. Cargar Sucursales"
            print *, "2. Cargar Tecnicos"
            print *, "3. Cargar Rutas"
            read (*, *) option
            
            select case(option)
              case(1)

                print *, "Ingrese el nombre del archivo de sucursales"
                read (*, *) filename
          
                readersucursales%filename = filename
          
                call readersucursales%readJsonSucursales(hash_GlobalTable,avl1)
                print *, "Leyendo archivo sucursales"

              case(2)
                
                print *, "Seleccioa el id de la sucursal a la que pertenecen los tecnicos"
                call avl1%showSucursales()
                read (*, *) option
                print *, "Ingrese el nombre del archivo de tecnicos"
                read (*, *) filename
                
                readertecnico%filename = filename
                call readertecnico%readJsonTecnicos(option,hash_table,avl1)
              
              case(3)
                print *, "Ingrese el nombre del archivo de rutas"
                read (*, *) filename
                allocate(graph_read)
                call readergrafos%readJsonGrafos(filename,graph_read,tree,avl1)
              case default
                print *, "Opcion no valida, intente de nuevo"
            end select
    
          case(2)
            print *, "1. Ver todos los tecnicos"
            print *, "2. Buscar tecnico por DPI"
            read (*, *) option
            select case(option)
            case(1)
                  print *, "Listado de tecnicos"
                  call avl1%showTableHashSucursales()
            case(2)
                  print *, "Ingrese el dpi del tecnico"
                  read (*, *) dpi
                  print *, "-------------------------"
                  call avl1%searchTech(DPI)
                  print *, "-------------------------"
            case default
              print *, "Opcion no valida, intente de nuevo"
              end select
          
            
            
          case(3)
            print *, "--Generar recorrido mas largo--"
            print *, "Ingrese el id de la sucursal de inicio"
            read (*, *) idSucursalInicio
            print *, "Ingrese el id de la sucursal de fin"
            read (*, *) idSucursalFin
            call graph_read%graphic()
            call analizador%set_graph(graph_read)
            list_Result => analizador%get_longest_path(idSucursalInicio, idSucursalFin)
            call list_Result%print()
          case(4)
            do while(.true.)
              print *, "--Generar reportes--"
              print *, "1. Generar grafico de rutas"
              print *, "2. Arbol de Merckle"
              print *, "3. BlockChain"
              print *, "4. Tabla hash"
              print *, "5. Salir"
              read (*, *) option
              select case(option)
                case(1)
                  print *, "Generando grafico de rutas"
                  call graph_read%graphic()
                case(2)
                  print *, "Generando arbol de Merckle"
                  call tree%generate()
                  call tree%merkle_dot()
                case(3)
                  print *, "Generando BlockChain"
                case(4)
                  print *, "Generando tabla hash"
                  print *, "Seleccioa el id de la sucursal para mostrar tabla hash de tecnicos"
                  call avl1%showSucursales()                
                  read (*, *) option
                  call avl1%graficarTablaHashIdSucursal(option)
                case(5)
                  exit
                case default
                  print *, "Opcion no valida, intente de nuevo"
                end select
              end do
          case (5)
            exit
          case default
            print *, "Opcion no valida, intente de nuevo"
        end select
    
      end do
    end subroutine menu

end program main
