module linkedListImages
    implicit none
    type image
        integer :: id
        type(image), pointer :: next => null()
    end type

    type imageList
        type(image), pointer :: head => null()
        type(image), pointer :: tail => null()
    contains
        procedure :: addImage
        procedure :: deleteImage
        procedure :: existID
        procedure :: searchid
    end type

    contains
    subroutine addImage(this,id)
        class(imageList), intent(inout) :: this
        integer, intent(in) :: id
        type(image), pointer :: newImage,test
        test => this%searchid(id)
        if (.not. associated(test)) then
            allocate(newImage)
            newImage%id = id
            if (.not. associated(this%head)) then
                this%head => newImage
                this%tail => newImage
            else
                this%tail%next => newImage
                this%tail => newImage
            end if
        else
            print *, "Image already exists"
            test%id = id
        end if
    end subroutine addImage

    subroutine deleteImage(this,id)
        class(imageList), intent(inout) :: this
        integer, intent(in) :: id
        type(image),pointer :: current
        type(image), pointer :: previous
        current => this%head
        previous => null()
        do while (associated(current))
            if (current%id == id) then
                if (associated(previous)) then
                    previous%next => current%next
                else
                    this%head => current%next
                end if
                deallocate(current)
                exit
            end if
            previous => current
            current => current%next
        end do
    end subroutine deleteImage
    
    subroutine existID(this,id)
        class(imageList), intent(in) :: this
        integer, intent(in) :: id
        type(image),pointer :: current
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                print *, "Image found"
                exit
            end if
            current => current%next
        end do
        print *, "Image not found"
    end subroutine existID

    function searchid(this,id) result(res)
        class(imageList), intent(inout) :: this
        integer, intent(in) :: id
        type(image), pointer :: current 
        type(image), pointer :: res
        current => this%head
        res => null()
        do while (associated(current))
            if (current%id == id) then
                print *, "Image found"
                res => current
                return
            end if
            current => current%next
        end do
        print *, "Image not found"
    end function searchid

end module linkedListImages

module linkedListAlbum
    use linkedListImages
    implicit none
    type album
        character(:), allocatable :: name
        type(imageList) :: images
        type(album), pointer :: next => null()
        type(album), pointer :: prev => null()
    end type
    
    type albumList
        type(album), pointer :: head => null()
        type(album), pointer :: tail => null()
        contains
        procedure :: addAlbum
        procedure :: searchAlbum
        procedure :: graphAlbumes
        procedure :: addImageInAlbum
        procedure :: deleteImageInAlbum
        procedure :: deleteImageInAllAlbums
        procedure :: cuantitiImagesInAlbums
        procedure :: cuantitiAlbums
    end type

    contains

    !subrutinas o funciones para la lista de almbumes
    subroutine addAlbum(this,name)
        class(albumList), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(album), pointer :: newAlbum,test
        test => this%searchAlbum(name)
        if (.not. associated(test)) then
            allocate(newAlbum)
            newAlbum%name = name
            if (.not. associated(this%head)) then
                this%head => newAlbum
                this%tail => newAlbum
            else
                this%tail%next => newAlbum
                newAlbum%prev => this%tail
                this%tail => newAlbum
            end if
        end if
    end subroutine addAlbum 

    function searchAlbum(this,name) result(res)
        class(albumList), intent(in) :: this
        character(len=*), intent(in) :: name
        type(album), pointer :: current 
        type(album), pointer :: res
        current => this%head
        res => null()
        do while (associated(current))
            if (current%name == name) then
                print *, "Album found"
                res => current
                return
            end if
            current => current%next
        end do
        print *, "Album not found"
    end function searchAlbum

    subroutine graphAlbumes(this,filename)
        class(albumList), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(album), pointer :: current
        type(image), pointer :: currentImage
        character(:), allocatable :: path,rank
        character(:), allocatable :: conexion
        integer :: file
        path = "images\"//trim(adjustl(filename))//".dot"
        open(file, file=path, status="replace")
        write(file, *) 'digraph G {'
        write(file, *) 'node [shape=box];'
        current => this%head
        if (associated(current)) then
            rank = "{rank=same"
            do while (associated(current))
                write(file, *) '"Album', trim(adjustl(current%name)), '" [label="', trim(adjustl(current%name)),'"];'
                rank = trim(adjustl(rank))//';"Album'//trim(adjustl(current%name))//'"'
                currentImage => current%images%head
                if (associated(currentImage)) then
                    write(file, *) '"Image', currentImage%id, '" [label="', currentImage%id,'"];'
                    write(file, *) '"Album', trim(adjustl(current%name)), '" -> "Image', currentImage%id, '";'
                    ! rank = trim(adjustl(rank))//'"Album"'//trim(adjustl(current%name))//'"'
                    do while (associated(currentImage%next))
                        write(file, *) '"Image', currentImage%next%id, '" [label="', currentImage%next%id,'"];'
                        write(file, *) '"Image', currentImage%id, '" -> "Image', currentImage%next%id, '";'
                        currentImage => currentImage%next
                    end do
                else 
                    print *, "No images in album"
                end if
                if (associated(current%next)) then
                    write(file, *) '"Album', trim(adjustl(current%name)), '" [label="', trim(adjustl(current%name)),'"];'
                    conexion = '"Album'// trim(adjustl(current%name))// '" -> "Album'// trim(adjustl(current%next%name))
                    write(file, *) conexion// '"[color=blue];'
                    write(file, *) conexion// '"[color=red][dir = back];'
                end if
                current => current%next
            end do
            write(file, *) rank, "}"
        else
            write(file, *) '"empty" [label="Empty albumes", shape=box];'
        end if
        write(file, *) '}'
        close(file)
        call execute_command_line("dot -Tpng images\albumes.dot -o images\albumes.png")
        !windows
        call system("start images\"//trim(adjustl(filename))//".png")
        ! linux 
        ! call system("xdg-open images\"//trim(adjustl(filename))//".png")
    end subroutine graphAlbumes

    subroutine addImageInAlbum(this,nameAlbum,idImage)
        class(albumList), intent(inout) :: this
        character(len=*), intent(in) :: nameAlbum
        integer, intent(in) :: idImage
        type(album), pointer :: currentAlbum
        type(image), pointer :: newImage
        currentAlbum => this%searchAlbum(nameAlbum)
        if (associated(currentAlbum)) then
            call currentAlbum%images%addImage(idImage)
        else 
            print *, "Album not found"
        end if
    end subroutine addImageInAlbum

    subroutine deleteImageInAlbum(this,nameAlbum,idImage)
        class(albumList), intent(inout) :: this
        character(len=*), intent(in) :: nameAlbum
        integer, intent(in) :: idImage
        type(album), pointer :: currentAlbum
        currentAlbum => this%searchAlbum(nameAlbum)
        if (associated(currentAlbum)) then
            call currentAlbum%images%deleteImage(idImage)
        else 
            print *, "Album not found"
        end if
    end subroutine deleteImageInAlbum

    subroutine deleteImageInAllAlbums(this,idImage)
        class(albumList), intent(inout) :: this
        integer, intent(in) :: idImage
        type(album), pointer :: currentAlbum
        type(image), pointer :: currentImage
        currentAlbum => this%head
        do while (associated(currentAlbum))
            call currentAlbum%images%deleteImage(idImage)
            currentAlbum => currentAlbum%next
        end do
    end subroutine deleteImageInAllAlbums

    function cuantitiImagesInAlbums(this) result(counter)
        class(albumList), intent(in) :: this
        type(album), pointer :: currentAlbum
        type(image), pointer :: currentImage
        integer :: counter
        counter = 0
        currentAlbum => this%head
        do while (associated(currentAlbum))
            currentImage => currentAlbum%images%head
            do while (associated(currentImage))
                counter = counter + 1
                currentImage => currentImage%next
            end do
            currentAlbum => currentAlbum%next
        end do
    end function cuantitiImagesInAlbums

    function cuantitiAlbums(this) result(counter)
        class(albumList), intent(in) :: this
        type(album), pointer :: currentAlbum
        integer :: counter
        counter = 0
        currentAlbum => this%head
        do while (associated(currentAlbum))
            counter = counter + 1
            currentAlbum => currentAlbum%next
        end do
    end function cuantitiAlbums

end module linkedListAlbum

!este es para leer los albumes del archivo json
module module_jsonReader_albums
    use json_module
    use linkedListAlbum
    use linkedListImages
    implicit none
    
    type, public :: jsonReader_albumsJson
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        character(:), allocatable :: filename
        logical :: found
        integer :: size
        contains
        procedure :: readJson_albumsJson
        procedure :: getText_albums
        procedure :: Inicialice_albumsJson
    end type jsonReader_albumsJson

    contains
    
    subroutine readJson_albumsJson(this,filename,albums)
        class(jsonReader_albumsJson), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(albumList), intent(inout) :: albums
        type(album), pointer :: currentAlbum
        type(imageList), pointer :: images
        integer :: i,num,currentnum,tempid        ! Se declaran variables enteras
        character(:),allocatable :: nombre
        integer, dimension(:), allocatable :: array
        logical :: found
        this%filename = filename
        call this%Inicialice_albumsJson()  ! Se inicializa el módulo JSON
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            nombre = this%getText_albums(poss=i,text="nombre_album") 
            call this%jsonc%get_child(this%listPointer, i, this%personPointer, found = found)
            call this%jsonc%get_child(this%personPointer, "imgs", this%attributePointer, found = found)
            print *, "album: ", nombre
            call albums%addAlbum(name=nombre)
            if (found) then
                call this%jsonc%get(this%attributePointer, array)
                currentAlbum => albums%searchAlbum(nombre)
                print *, "album: ", nombre
                do num = 1, size(array)
                    print *, "imagen: ", array(num)
                    call currentAlbum%images%addImage(array(num))
                end do
            end if
        end do
    end subroutine

    function getText_albums(this,poss,text) result(valueret)
        class(jsonReader_albumsJson), intent(inout) :: this
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
        
    end function getText_albums

    subroutine Inicialice_albumsJson(this)
        class(jsonReader_albumsJson), intent(inout) :: this
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
    end subroutine Inicialice_albumsJson

end module module_jsonReader_albums

module module_ordinal_user
    use module_abbtree_layers
    use module_jsonReader_layers
    use module_avlTree_images
    use module_jsonReader_images
    use linkedListAlbum
    use module_jsonReader_albums
    use module_Matrix
    use module_queue
    implicit none

    type ordinal_user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    type(abbtree_layers) :: LayersTree
    type(avlTree_images) :: ImagesTree
    type(albumList) :: albums

    contains
    procedure :: ver_reportes_estructuras
    procedure :: navegacion_gestion_imagenes
    procedure :: carga_masiva_capas
    procedure :: carga_masiva_imagenes
    procedure :: carga_masiva_albumes
    procedure :: reportes_de_usuario
    procedure :: por_recorrido_limitado! no usar este
    procedure :: por_arbol_de_imagenes! no usar este
    procedure :: por_capa ! no usar este
    procedure :: ver_imagen_y_arbol_de_capas
    end type ordinal_user

    contains
    subroutine ver_reportes_estructuras(this)
        class(ordinal_user), intent(in) :: this
        type(pixel), pointer :: actPixel
        type(queue) :: cola
        logical :: found
        integer :: response,i,j,option
        found = .false.
        do while (.not. found)
            print *, "---------------------------------"
            print *, "Menu de reportes"
            print *, "---------------------------------"
            print *, "1. graficar arboles"
            print *, "2. graficar matriz de una capa"
            print *, "3. reportes del usuario"
            print *, "4. salir"
            read *, option
            select case(option)
                case(1)
                    print *, "---------------------------------"
                    print *, "Generando arboles"
                    print *, "---------------------------------"    
                    call this%ImagesTree%graphImages("images")   !4.3.1
                    call this%LayersTree%graphABBTree("layers")   !4.3.2
                    call this%albums%graphAlbumes("albumes") !4.3.3
                    print *, "---------------------------------"
                    print *, "arboles generados"
                    print *, "---------------------------------"
                case(2)
                    call this%por_capa(acumulativo=0)!4.1.3
                case(3)
                    call this%reportes_de_usuario()!4.1.4
                case(4)
                    found = .true.
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine ver_reportes_estructuras

    subroutine navegacion_gestion_imagenes(this)
        class(ordinal_user), intent(inout) :: this
        type(imageList), pointer :: currentImage
        type(album), pointer :: currentAlbum
        class(image), pointer :: img
        integer :: id
        integer :: option
        logical :: found
        found = .false.
        do while (.not. found)
            print *, "---------------------------------"
            print *, "Menu de navegacion de imagenes"
            print *, "---------------------------------"
            print *, "1. menu de graficacion de imagenes"
            print *, "2. eliminar imagen"
            print *, "3. salir"
            print *, "---------------------------------"
            read *, option
            select case(option)
                case(1)
                    print *, "---------------------------------"
                    print *, "menu de graficacion de imagenes"
                    print *, "---------------------------------"
                    print *, "1. por recorrido limitado"
                    print *, "2. por arbol de imagenes"
                    print *, "3. por capa"
                    print *, "4. ver imagen y arbol de capas"
                    print *, "5. salir"
                    read *, option
                    select case(option)
                        case(1)
                            call this%por_recorrido_limitado()!4.1.1
                        case(2)
                            call this%por_arbol_de_imagenes()!4.1.2
                        case(3)
                            call this%por_capa(acumulativo=1)!4.1.3
                        case(4)
                            call this%ver_imagen_y_arbol_de_capas()
                        case(5)

                        case default
                            print *, "Opcion no valida"
                    end select
                case(2)
                    print *, "---------------------------------"
                    print *, "Eliminar imagen"
                    print *, "---------------------------------"
                    print *, "ingrese el id de la imagen a eliminar"
                    read *, id
                    img => this%ImagesTree%searchImage(id)
                    if (associated(img)) then
                        call this%ImagesTree%deleteImg(id)
                        call this%albums%deleteImageInAllAlbums(id)
                        print *, "---------------------------------"
                        print *, "Imagen eliminada"
                        print *, "---------------------------------"
                    else
                        print *, "---------------------------------"
                        print *, "Imagen no encontrada"
                        print *, "---------------------------------"
                    end if
                case(3)
                    found = .true.
                case default
                    print *, "Opcion no valida"
            end select
        end do 
    end subroutine navegacion_gestion_imagenes

    subroutine carga_masiva_capas(this)
        class(ordinal_user), intent(inout) :: this
        type(jsonReader_layers) :: reader
        print *, "---------------------------------"
        print *, "Carga masiva de capas"
        print *, "---------------------------------"
        call reader%readJson_Layers(filename="capas.json",tree=this%LayersTree)
        print *, "---------------------------------"
        print *, "Carga masiva de capas finalizada"
        print *, "---------------------------------"
    end subroutine carga_masiva_capas

    subroutine carga_masiva_imagenes(this)
        class(ordinal_user), intent(inout) :: this
        type(jsonReader_images) :: reader
        print *, "---------------------------------"
        print *, "Carga masiva de imagenes"
        print *, "---------------------------------"
        call reader%readJson_Images(filename="imagenes.json",abbPrincipalTree=this%LayersTree,avlPrincipalTree=this%ImagesTree)
        print *, "---------------------------------"
        print *, "Carga masiva de imagenes finalizada"
        print *, "---------------------------------"
    end subroutine carga_masiva_imagenes

    subroutine carga_masiva_albumes(this)
        class(ordinal_user), intent(inout) :: this
        type(jsonReader_albumsJson) :: reader
        print *, "---------------------------------"
        print *, "Carga masiva de albumes"
        print *, "---------------------------------"
        call reader%readJson_albumsJson(filename="albumes.json",albums=this%albums)
        print *, "---------------------------------"
        print *, "Carga masiva de albumes finalizada"
        print *, "---------------------------------"
    end subroutine carga_masiva_albumes

    subroutine por_recorrido_limitado(this)
        class(ordinal_user), intent(in) :: this
        type(queue) :: cola
        type(abbtree_layers) :: tree
        type(pixelList) :: pixels
        type(matrix) :: actmatrix
        type(pixel), pointer :: actPixel
        class(image), pointer :: img
        type(layer), pointer :: layertemp
        character(:), allocatable :: recorrido
        character(15) :: casteo
        integer :: id,num,i
        logical :: salir
        salir = .false.
        actmatrix%root => null()
        print *, "---------------------------------"
        print *, "Menu de recorrido limitado"
        print *, "---------------------------------"
        print *, "ingrese el id de la imagen a graficar"
        call cola%cleanQueue()
        call this%ImagesTree%inorderAVL(this%ImagesTree%root,cola)
        call cola%printQueue()
        print *, "---------------------------------"
        read *, id
        img => this%ImagesTree%searchImage(id)
        if (associated(img)) then
            tree = img%abb
            print *, "ingrese el numero de capas a graficar"
            print *, "si el numero es mayor a la cantidad se graficara todo"
            read *, num
            do while (.not. salir)
                print *, "-------tipo de orden-----------"
                print *, "1. preorden"
                print *, "2. inorden"
                print *, "3. postorden"
                print *, "4. salir"
                read *, id
                select case(id)
                    case(1)
                        call cola%cleanQueue()
                        call tree%preorderABB(tree%root,cola)
                        recorrido = "preorden: "
                    case(2)
                        call cola%cleanQueue()
                        call tree%inorderABB(tree%root,cola)
                        recorrido = "inorden: "
                    case(3)
                        call cola%cleanQueue()
                        call tree%postorderABB(tree%root,cola)
                        recorrido = "postorden: "
                    case(4)
                        salir = .true.
                        return
                    case default
                        print *, "Opcion no valida"
                end select
                do i = 1, num
                    if (.not.cola%isEmpty()) then
                        id = cola%dequeue()
                        layertemp => tree%searchLayer(id)
                        actPixel => layertemp%pixels%head
                        write(casteo,'(I15)') id
                        if (i==num) then
                            recorrido = trim(adjustl(recorrido))//" " // trim(adjustl(casteo))
                        else
                            recorrido = trim(adjustl(recorrido))//" " // trim(adjustl(casteo))//","
                        end if
                        do while (associated(actPixel))
                            call actmatrix%insert(i=actPixel%row,j=actPixel%col,color=actPixel%color)
                            actPixel => actPixel%next
                        end do
                    else
                        exit
                    end if
                end do
                call actmatrix%graphTable("img_recorrido_limitado",trim(adjustl(recorrido)))
            end do
        end if 
    end subroutine por_recorrido_limitado

    subroutine por_arbol_de_imagenes(this)
        class(ordinal_user), intent(in) :: this
        type(matrix) :: actmatrix
        type(queue) :: cola
        class(image), pointer :: img
        integer :: response
        logical :: found
        found = .false.
        actmatrix%root => null()
        print *, "---------------------------------"
        print *, "que imagen desea graficar? (ingresar id de la imagen)"
        call cola%cleanQueue()
        call this%ImagesTree%inorderAVL(this%ImagesTree%root,cola)
        call cola%printQueue()
        print *, "---------------------------------"
        print *, "arriba estan las imagenes disponibles (ingrese '-1' para salir)"
        print *, "---------------------------------"
        read *, response
        if (response /= -1) then
            img => this%ImagesTree%searchImage(response)
            if (associated(img)) then
                call this%ImagesTree%breadthFirstMatrix(actualMatrix=actmatrix,idImage=response)
                call actmatrix%graphMatrix("matrix_image")
                call actmatrix%graphTable("image"," ")
                call actmatrix%cleanMatrix()
                print *, "---------------------------------"
                print *, "Imagen graficada"
                print *, "---------------------------------"
            else
                print *, "---------------------------------"
                print *, "Imagen no encontrada"
                print *, "---------------------------------"
            end if
        else
            return
        end if
    end subroutine por_arbol_de_imagenes

    subroutine por_capa(this,acumulativo)
        class(ordinal_user), intent(in) :: this
        integer, intent(in) :: acumulativo
        type(matrix) :: actmatrix
        type(queue) :: cola
        type(layer), pointer :: layertemp
        type(pixel), pointer :: actPixel
        integer :: response
        logical :: found
        integer :: i,j
        found = .false.
        actmatrix%root => null()
        do while (.not. found) !4.3.4
            if (associated(this%LayersTree%root)) then
                print *, "---------------------------------"
                print *, "que capa desea graficar? (ingresar id de la capa)"
                call cola%cleanQueue()
                call this%LayersTree%inorderABB(this%LayersTree%root,cola)
                call cola%printQueue()
                print *, "---------------------------------"
                print *, "arriba estan las capas disponibles (ingrese '-1' para salir)"
                print *, "---------------------------------"
                read *, response
                if (response /= -1) then
                    layertemp => this%LayersTree%searchLayer(response)
                    actPixel => layertemp%pixels%head
                    do while (associated(actPixel))
                        i = actPixel%row
                        j = actPixel%col
                        call actmatrix%insert(i=i,j=j,color=actPixel%color)
                        actPixel => actPixel%next
                    end do
                    call actmatrix%graphTable(filename="matrix_layer",text=" ")
                    if (acumulativo /= 1) then
                        call actmatrix%cleanMatrix()
                    end if
                else
                    return
                end if
            else
                print *, "No hay capas"
                exit
            end if
        end do
    end subroutine por_capa

    subroutine ver_imagen_y_arbol_de_capas(this)
        class(ordinal_user), intent(in) :: this
        type(matrix) :: actmatrix
        type(queue) :: cola
        class(image), pointer :: img
        integer :: response
        logical :: found
        found = .false.
        actmatrix%root => null()
        print *, "---------------------------------"
        print *, "que imagen desea graficar? (ingresar id de la imagen)"
        call cola%cleanQueue()
        call this%ImagesTree%inorderAVL(this%ImagesTree%root,cola)
        call cola%printQueue()
        print *, "---------------------------------"
        print *, "arriba estan las imagenes disponibles (ingrese '-1' para salir)"
        print *, "---------------------------------"
        read *, response
        if (response /= -1) then
            img => this%ImagesTree%searchImage(response)
            if (associated(img)) then
                call this%ImagesTree%graphImagesWithLayers(filename="arbolDeImagenesConArbolDeCapas",idImage=response)
                print *, "---------------------------------"
                print *, "Imagen graficada"
                print *, "---------------------------------"
            else
                print *, "---------------------------------"
                print *, "Imagen no encontrada"
                print *, "---------------------------------"
            end if
        else
            return
        end if
    end subroutine ver_imagen_y_arbol_de_capas

    subroutine reportes_de_usuario(this)
        class(ordinal_user), intent(in) :: this
        type(queue) :: cola
        integer :: num_images
        print *, "---------------------------------"
        print *, "Reportes de usuario"
        print *, "---------------------------------"
        print *, "usuario: ", this%name
        print *, "DPI: ", this%DPI
        print *, "password: ", this%password
        print *, "---------------------------------"    
        print *, "top 5 imagenes con mas numeros de capas"
        call this%ImagesTree%inorderAVL(this%ImagesTree%root,cola)
        num_images = cola%getQueueSize()
        call this%ImagesTree%top_five_images_with_more_layers(cola,num_images)
        call cola%cleanQueue()
        print *, "---------------------------------"
        print *, "todas las capas que son hojas"
        call this%LayersTree%leaf_layers(this%layersTree%root)
        print *, ""
        print *, "---------------------------------"
        print *, "profundidad de arbol de capas: ", this%LayersTree%profundidad
        print *, "--------------capas--------------"
        call this%LayersTree%preorderABB(this%LayersTree%root,cola)
        write(*,"(A)", advance='no') "preorden"
        call cola%printQueue()
        call cola%cleanQueue()
        call this%LayersTree%inorderABB(this%LayersTree%root,cola)
        write(*,"(A)", advance='no') "inorden"  
        call cola%printQueue()
        call cola%cleanQueue()
        call this%LayersTree%postorderABB(this%LayersTree%root,cola)
        write(*,"(A)", advance='no') "postorden"
        call cola%printQueue()
        print *, "---------------------------------"
    end subroutine reportes_de_usuario

end module
module module_btree
    use module_ordinal_user
    implicit none
    type queue_node
        type(BTree), pointer :: tree => null()
        type(queue_node), pointer :: next => null()
    end type queue_node

    type queue_BTree
        type(queue_node), pointer :: head => null()
        type(queue_node), pointer :: tail => null()
    contains
        procedure :: queueaddBTree
        procedure :: dequeueBTree
    end type

    type nodeptr
        type (BTree), pointer :: ptr => null()
    end type nodeptr
    
    type BTree
        !orden=MAXI+1
        integer :: MAXI = 4
        !el numero debe de ser el mismo que el maxi
        integer :: MINI = ceiling((dble(4)-1)/2)
        ! val(0:n), n=maxi+1
        type(ordinal_user) :: val(0:5)
        integer :: num = 0
        ! link(0:n), n=maxi+1
        type(nodeptr) :: link(0:5)
        type(BTree), pointer :: root => null()
        contains
        procedure :: insert
        procedure :: returnRoot
        procedure :: traversal
        procedure :: traversal2
        procedure :: remove
        procedure :: graphBTree
        procedure :: setValue
        procedure :: splitNode
        procedure :: createNode
        procedure :: deletenode
        procedure :: getUser
        procedure :: getUserAdmin
        procedure :: breadthFirstMatrix_adminReport
    end type BTree

    contains

    subroutine insert(this,val)
        class(BTree), intent(inout) :: this
        type(ordinal_user), intent(in) :: val
        type(ordinal_user) :: i
        type(BTree), pointer :: child
        allocate(child)
        if (this%setValue(val, i, this%root, child)) then
            this%root => this%createNode(i, child,this%root)
        end if
    end subroutine insert

    function returnRoot(this) result(myRoot)
        class(BTree) :: this
        type(BTree), pointer :: myRoot
        myRoot => this%root
    end function returnRoot

    recursive function setValue(this,val, pval, node, child) result(res)
        class(BTree), intent(inout) :: this
        type(ordinal_user), intent(in) :: val
        type(ordinal_user), intent(inout) :: pval
        type(BTree), pointer, intent(inout) :: node
        type(BTree), pointer, intent(inout) :: child
        type(BTree), pointer :: newnode        
        integer :: pos
        logical :: res
        allocate(newnode)
        if (.not. associated(node)) then            
                pval = val
                child => null()
                res = .true.
                return
        end if
        if (val%DPI < node%val(1)%DPI) then
            pos = 0
        else
            pos = node%num
            do while (val%DPI < node%val(pos)%DPI .and. pos > 1) 
            pos = pos - 1
            end do
            if (val%DPI == node%val(pos)%DPI) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
        end if
        if (this%setValue(val, pval, node%link(pos)%ptr, child)) then
            if (node%num < this%MAXI) then
                call insertNode(pval, pos, node, child)
            else
                call this%splitNode(pval, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
                return
            end if
        end if
        res = .false.
    end function setValue

    subroutine insertNode(val, pos, node, child)
        type(ordinal_user), intent(in) :: val
        integer, intent(in) :: pos
        type(BTree), pointer, intent(inout) :: node
        type(BTree), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while (j > pos)
                node%val(j + 1) = node%val(j)
                node%link(j + 1)%ptr => node%link(j)%ptr
                j = j - 1
        end do
        node%val(j + 1) = val
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(this,val, pval, pos, node, child, newnode)
        class(BTree), intent(in) :: this
        type(ordinal_user), intent(in) :: val
        integer,intent(in):: pos
        type(ordinal_user), intent(inout) :: pval
        type(BTree), pointer, intent(inout) :: node,  newnode
        type(BTree), pointer, intent(in) ::  child
        integer :: median, i, j
        if (pos > this%MINI) then
                median = this%MINI + 1
        else
                median = this%MINI
        end if
        if (.not. associated(newnode)) then
            allocate(newnode)
        do i = 0, this%MAXI
                    newnode%link(i)%ptr => null()
            enddo
        end if
        j = median + 1
        do while (j <= this%MAXI)
                newnode%val(j - median) = node%val(j)
                newnode%link(j - median)%ptr => node%link(j)%ptr
                j = j + 1
        end do
        node%num = median
        newnode%num = this%MAXI - median
        if (pos <= this%MINI) then
                call insertNode(val, pos, node, child)
        else
                call insertNode(val, pos - median, newnode, child)
        end if        
        pval = node%val(node%num)        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(this,val, child,root) result(newNode)
        class(BTree), intent(in) :: this
        type(ordinal_user), intent(in) :: val
        type(BTree), pointer, intent(in) :: child
        type(BTree), pointer, intent(in) :: root    
        type(BTree), pointer :: newNode
        integer :: i
        allocate(newNode)
        newNode%val(1) = val
        newNode%num = 1
        newNode%link(0)%ptr => root
        newNode%link(1)%ptr => child
        do i = 2, this%MAXI
            newNode%link(i)%ptr => null()
        end do
    end function createNode

    recursive subroutine traversal(this,myNode)
        class(BTree), intent(in) :: this
        type(BTree), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
                write (*, '(A)', advance='no') ' ['
                !en el i=0 no hay nada
                do i = 1, myNode%num
                    write (*,'(I17)', advance='no') myNode%val(i)%DPI
                end do
                do i = 0, myNode%num
                    ! write (*,'(I5)', advance='no') i
                    call this%traversal(myNode%link(i)%ptr)
                end do
                write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal

    recursive subroutine traversal2(this,myNode)
        class(BTree), intent(in) :: this
        type(BTree), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
            write (*, '(A)', advance='no') ' ['
            !en el i=0 no hay nada
            do i = 1, myNode%num
                write (*,'(I17)', advance='no') myNode%val(i)%DPI
                write (*,'(A)', advance='no') "("
                write (*,'(A)', advance='no') myNode%val(i)%password
                write (*,'(A)', advance='no') ")"
            end do
            do i = 0, myNode%num
                ! write (*,'(I5)', advance='no') i
                call this%traversal(myNode%link(i)%ptr)
            end do
            write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal2

    subroutine remove(this,DPI)
        class(BTree), intent(inout) :: this
        integer(kind=8), intent(in) :: DPI
        class(BTree), pointer :: root2
        allocate(root2)
        print *, ""
        call this%deletenode(DPI,this%root,root2)
        call root2%traversal(myNode=root2%returnRoot())
        print *, ""
        deallocate(this%root)
        this%root => root2%returnRoot()
    end subroutine remove
    
    recursive subroutine deletenode(this,DPI,root1,tree)
        class(BTree), intent(inout) :: this
        integer(kind=8), intent(in) :: DPI
        
        type(BTree), pointer ,intent(out) :: root1
        class(BTree), pointer, intent(out) :: tree
        integer :: i 
        if (associated(root1)) then
            do i = 1, root1%num
                if (DPI /= root1%val(i)%DPI) then
                    call tree%insert(root1%val(i))
                else
                end if 
            end do
            i = 0
            do i = 0, root1%num
                call this%deletenode(DPI,root1%link(i)%ptr,tree)
            end do
        end if
    end subroutine deletenode

    recursive subroutine graphBTree(this,myNode)
        class(BTree), intent(in) :: this
        type(BTree), pointer, intent(in) :: myNode
        type(BTree), pointer :: current
        character(:),allocatable :: path
        type(ordinal_user), pointer :: user
        integer :: i,file
        character(200) :: node1,casteo
        path= "images\treeUsers.dot"
        open(file, file=path, status="replace")
        write(file, *) 'digraph G {'
        
        if (.not. associated(myNode)) then
            write(file, *) '"empty" [label="Empty papers", shape=box];'
        else
            write (node1,'(I13)') myNode%val(1)%DPI
            do i = 1, myNode%num-1
                user => myNode%val(i+1)
                if (associated(user)) then
                    casteo=""
                    write (casteo,'(I13)') myNode%val(i+1)%DPI
                    print *, "casteo", trim(adjustl(casteo))
                    node1=trim(adjustl(node1))//","//trim(adjustl(casteo))
                end if
            end do
            write(file, *) " ",'"Node', trim(adjustl(node1)), '" [label="', trim(adjustl(node1)),'"];'
            
            do i = 0, myNode%num
                if (associated(myNode%link(i)%ptr)) then
                    call graphrec(myNode%link(i)%ptr,node1,file)
                else
                    write (*,*) "no hay mas nodos"
                end if 
            end do
        end if
        write(file, *) '}'
        close(file)
        call execute_command_line(trim("dot -Tpng images\treeUsers.dot -o images\treeUsers.png"))
        !windows
        call system("start images\treeUsers.png")
        ! linux 
        ! call system("xdg-open images\"//trim(adjustl(filename))//".png")
    end subroutine graphBTree

    recursive subroutine graphrec(myNode,node1,file)
        type(BTree), pointer, intent(in) :: myNode
        type(BTree), pointer :: current
        character(200), intent(inout) :: node1
        character(200) :: node2
        integer, intent(inout) :: file
        !si se va a cambiar el orden hay que cambiar el limite, se estima que para grado 5 son 72
        !pero por si las moscas mejor se le pone 200
        character(15) :: casteo
        integer :: i
        if (associated(myNode)) then
            ! do while(associated(myNode))
            ! print *, myNode%val(1)%DPI
            write (node2,'(I13)') myNode%val(1)%DPI
            print *, "entro"
            do i = 1, myNode%num-1
                !tengo que correjir esto
                casteo=""
                write (casteo,'(I13)') myNode%val(i+1)%DPI
                node2=trim(adjustl(node2))//","//trim(adjustl(casteo))
            end do
            print *, trim(adjustl(node2))
            print *, "tamanio de node", myNode%num
            write(file, *) " ",'"Node', trim(adjustl(node2)), '" [label="', trim(adjustl(node2)),'"];'
            
            write(file, *) " ",'"Node', trim(adjustl(node1)), '" -> "Node', trim(adjustl(node2)), '";'
            do i = 0, myNode%num
                call graphrec(myNode%link(i)%ptr,node2,file)
            end do
        end if
    end subroutine graphrec

    function getUser(this,DPI,password,myNode,found) result(user)
        class(BTree), intent(in) :: this
        integer(kind=8), intent(in) :: DPI
        character(50), intent(in) :: password
        type(ordinal_user),pointer :: user,actualuser
        type(BTree), pointer,intent(in) :: myNode
        type(BTree), pointer :: myNode2
        logical, intent(out) :: found
        integer :: i
        myNode2 => myNode
        found = .false.
        do while (.not. found)
            if (associated(myNode2)) then
                do i = 0, myNode2%num-1
                    actualuser => myNode2%val(i+1)
                    if (DPI<actualuser%DPI) then
                        if (associated(myNode2%link(i)%ptr)) then
                            myNode2 => myNode2%link(i)%ptr
                            exit
                        else
                            return
                        end if
                        !si es igual al primer nodo entonces va a devolver ese nodo
                    else if (actualuser%DPI==DPI) then
                        if (trim(adjustl(actualuser%password))==trim(adjustl(password))) then
                            user => myNode2%val(i+1)
                            found = .true.
                            return
                        else 
                            return
                        end if
                    end if
                end do
                if (DPI>myNode2%val(myNode2%num)%DPI) then
                    myNode2 => myNode2%link(myNode2%num)%ptr
                end if
            else
                return
            end if
        end do
    end function getUser

    function getUserAdmin(this,DPI,myNode) result(user)
        class(BTree), intent(in) :: this
        integer(kind=8), intent(in) :: DPI
        type(ordinal_user),pointer :: user,actualuser
        type(BTree), pointer,intent(in) :: myNode
        type(BTree), pointer :: myNode2
        logical :: found
        integer :: i
        myNode2 => myNode
        found = .false.
        user => null()
        do while (.not. found)
            if (associated(myNode2)) then
                do i = 0, myNode2%num-1
                    actualuser => myNode2%val(i+1)
                    if (DPI<actualuser%DPI) then
                        if (associated(myNode2%link(i)%ptr)) then
                            myNode2 => myNode2%link(i)%ptr
                            exit
                        else
                            print *, "No se encontro el usuario"
                            return
                        end if
                        !si es igual al primer nodo entonces va a devolver ese nodo
                    else if (actualuser%DPI==DPI) then
                        user => myNode2%val(i+1)
                        return
                    end if
                end do
                if (DPI>myNode2%val(myNode2%num)%DPI) then
                    myNode2 => myNode2%link(myNode2%num)%ptr
                end if
            else
                print *, "No se encontro el usuario"
                return
            end if
        end do
    end function getUserAdmin

    subroutine breadthFirstMatrix_adminReport(this)
        class(BTree), intent(in) :: this
        type(ordinal_user), pointer :: actualUser
        type(BTree), pointer :: myNode
        type(queue_BTree) :: queue
        integer :: posicion,posicion2,i
        integer(kind=8) :: DPI
        if (.not. associated(this%root)) then
            print *, "Arbol de usuarios esta vacio"
            return
        end if
        call queue%queueaddBTree(this%root)
        do while (associated(queue%head))
            myNode => queue%dequeueBTree()
            ! este es el visit()
            do i = 0, myNode%num-1
                actualUser => myNode%val(i+1)
                print *, "Usuario: ", actualUser%name
                print *, "DPI: ", actualUser%DPI
                print *, "cantidad imagenes: ", actualUser%ImagesTree%num_images
                print *, "---------------------------------"
            end do
            !
            do i = 0, myNode%num
                if (associated(myNode%link(i)%ptr)) then
                    call queue%queueaddBTree(myNode%link(i)%ptr)
                end if
            end do
        end do
    end subroutine breadthFirstMatrix_adminReport

    subroutine queueaddBTree(this,myNode)
        class(queue_BTree), intent(inout) :: this
        type(BTree), pointer, intent(in) :: myNode
        type(queue_node), pointer :: newNode
        allocate(newNode)
        newNode%tree => myNode
        if (.not. associated(this%head)) then
            this%head => newNode
            this%tail => newNode
        else
            this%tail%next => newNode
            this%tail => newNode
        end if
    end subroutine queueaddBTree

    function dequeueBTree(this) result(myNode)
        class(queue_BTree), intent(inout) :: this
        type(BTree), pointer :: myNode
        type(queue_node), pointer :: temp
        if (associated(this%head)) then
            myNode => this%head%tree
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
        else
            myNode => null()
        end if
    end function dequeueBTree
end module module_btree