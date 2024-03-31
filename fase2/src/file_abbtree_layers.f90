module module_layer
    type :: pixel
        integer :: row
        integer :: col
        character(len=7) :: color
        type(pixel), pointer :: next => null()
    end type pixel

    type :: pixelList
        type(pixel), pointer :: head => null()
        type(pixel), pointer :: tail => null()
        contains
            procedure :: insertPixel
            procedure :: getHead
    end type pixelList

    type :: layer
        integer :: id
        type(pixelList) :: pixels
        type(layer), pointer :: right => null()
        type(layer), pointer :: left => null()
    end type layer

    contains    
    !Subrutinas del tipo pixelList
    subroutine insertPixel(this, row, col, color)
        class(pixelList), intent(inout) :: this
        integer, intent(in) :: row, col
        character(len=7), intent(in) :: color
        type(pixel), pointer :: temp
        allocate(temp)
        temp = pixel(col=col, row=row, color=color)
        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => this%tail%next
        end if
    end subroutine insertPixel

    function getHead(this) result(head)
        class(pixelList), intent(in) :: this
        type(pixel), pointer :: head
        head => this%head
    end function getHead

end module module_layer

!este no lo he probado del todo
module module_abbtree_layers
    use module_layer
    use module_queue
    implicit none

    type :: abbtree_layers
            type(layer), pointer :: root => null()
            integer :: profundidad = 0
            integer :: num_layers = 0
        contains
            procedure :: insertLayer
            procedure :: deleteLayer
            procedure :: preorderABB
            procedure :: inorderABB
            procedure :: postorderABB
            procedure :: graphABBTree
            procedure :: searchLayer
            procedure :: searchLayer_Rec
            procedure :: insertRec
            procedure :: leaf_layers
    end type abbtree_layers

    contains

    !Subrutinas del tipo abbtree_layers
    subroutine insertLayer(this, id, pixels)
        class(abbtree_layers), intent(inout) :: this
        integer, intent(in) :: id
        type(pixelList), intent(in) :: pixels
        type(layer), pointer :: capa
        integer :: profundidad
        profundidad = 0
        allocate(capa)
        capa = layer(id=id, pixels=pixels)
        if (.not. associated(this%root)) then
            this%root => capa
        else
            call this%insertRec(this%root, capa,profundidad)
            if (profundidad > this%profundidad) then
                this%profundidad = profundidad
            end if
        end if
    end subroutine insertLayer

    !no usar por separado
    recursive subroutine insertRec(this,root, capa,profundidad)
        class(abbtree_layers), intent(inout) :: this
        type(layer), pointer, intent(inout) :: root
        class(layer),pointer, intent(in) :: capa
        integer, intent(inout) :: profundidad
        ! print *, "capa: ", capa%id, "capa2: ", root%id
        if (capa%id < root%id) then
            profundidad = profundidad + 1
            if (.not. associated(root%left)) then
                this%num_layers = this%num_layers + 1
                allocate(root%left)
                root%left => capa
            else
                call this%insertRec(root%left, capa,profundidad)
            end if
        else if (capa%id > root%id) then
            profundidad = profundidad + 1
            if (.not. associated(root%right)) then
                this%num_layers = this%num_layers + 1
                allocate(root%right)
                root%right => capa
            else
                call this%insertRec(root%right, capa,profundidad)
            end if
        else if (capa%id == root%id) then
            !revisar esto
            root => capa
        end if
    end subroutine insertRec

    subroutine deleteLayer(this, capa)
        class(abbtree_layers), intent(inout) :: this
        integer, intent(in) :: capa
    
        this%root => deleteRec(this%root, capa)
    end subroutine deleteLayer

    !no usar por separado
    recursive function deleteRec(root, key) result(res)
        type(layer), pointer :: root
        integer, intent(in) :: key
        type(layer), pointer :: res
        type(layer), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (key < root%id) then
            root%left => deleteRec(root%left, key)
        else if (key > root%id) then
            root%right => deleteRec(root%right, key)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinorsLayers(root%left, temp)
                root%id = temp%id
                root%left => deleteRec(root%left, temp%id)
            end if
        end if

        res => root
    end function deleteRec

    subroutine preorderABB(this, tmp,cola)
        class(abbtree_layers), intent(in) :: this
        type(layer), intent(in), pointer :: tmp
        type(Queue), intent(inout) :: cola
        if(associated(tmp)) then
            call cola%enqueue(tmp%id)
            call this%preorderABB(tmp%left,cola)
            call this%preorderABB(tmp%right,cola)
        else 
            return
        end if
    end subroutine preorderABB

    subroutine inorderABB(this, tmp,cola)
        class(abbtree_layers), intent(in) :: this
        type(layer), intent(in), pointer :: tmp
        type(Queue), intent(inout) :: cola
        if(associated(tmp)) then
            call this%inorderABB(tmp%left,cola)
            call cola%enqueue(tmp%id)
            call this%inorderABB(tmp%right,cola)
        else
            return
        end if
    end subroutine inorderABB

    subroutine postorderABB(this, tmp,cola)
        class(abbtree_layers), intent(in) :: this
        type(layer), intent(in), pointer :: tmp
        type(Queue), intent(inout) :: cola
        if(associated(tmp)) then
            call this%postorderABB(tmp%left,cola)
            call this%postorderABB(tmp%right,cola)
            call cola%enqueue(tmp%id)
        else
            return
        end if
    end subroutine postorderABB

    subroutine graphABBTree(this, filename)
        class(abbtree_layers), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(:),allocatable :: path
        integer :: file
        path = "images/"//trim(adjustl(filename))//".dot"
        open(file, file=path, status="replace")
        write(file, '(A)') 'digraph{'
        if (associated(this%root)) then
            call graphABBTree_rec(this%root, file)
        else 
            write(file, '(A)') '"empty" [label="Empty layers", shape=box];'
        end if
        write(file, '(A)') '}'
        close(file)
        call execute_command_line("dot -Tpng images/"//trim(adjustl(filename))//".dot -o images/"//trim(adjustl(filename))//".png")
        !windows
        call system("start images\"//trim(adjustl(filename))//".png")
        ! linux 
        ! call system("xdg-open images\"//trim(adjustl(filename))//".png")
    end subroutine graphABBTree

    recursive subroutine graphABBTree_rec( tmp, unit)
        class(layer), intent(in), pointer :: tmp
        integer, intent(in) :: unit
        if (.not. associated(tmp)) then
            return
        end if
        write (unit, '(A,I5,A,I5,A)') ' ', tmp%id, ' [label="', tmp%id, '"];'
        if (associated(tmp%left)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%id, ' -> ', tmp%left%id, ';'
        end if
        if (associated(tmp%right)) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%id, ' -> ', tmp%right%id, ';'
        end if
        call graphABBTree_rec(tmp%left, unit)
        call graphABBTree_rec(tmp%right, unit)
    end subroutine graphABBTree_rec
    
    recursive subroutine getMajorOfMinorsLayers(root, major)
        type(layer), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinorsLayers(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinorsLayers

    function searchLayer(this, id) result(res)
        class(abbtree_layers), intent(in) :: this
        type(layer), pointer :: res
        integer, intent(in) :: id
        type(layer) :: temp
        temp = layer(id=id)
        res => this%searchLayer_Rec(this%root, temp)
    end function searchLayer

    !no usar por separado   
    recursive function searchLayer_Rec(this,root, temp) result(res)
        class(abbtree_layers), intent(in) :: this
        type(layer), pointer :: root
        type(layer), intent(in) :: temp
        class(layer), pointer :: res
        if (.not. associated(root)) then
            print *, "No se encontro la capa ", temp%id
            return
        end if

        if (temp%id < root%id) then
            res => this%searchLayer_Rec(root%left, temp)
        else if (temp%id > root%id) then
            res => this%searchLayer_Rec(root%right, temp)
        else
            res => root
        end if
    end function searchLayer_Rec

    subroutine leaf_layers(this, tmp)
        class(abbtree_layers), intent(in) :: this
        type(layer), intent(in), pointer :: tmp
        if(associated(tmp)) then
            call this%leaf_layers(tmp%left)
            if (.not. associated(tmp%left) .and. .not. associated(tmp%right)) then
                write(*,"(I4)", advance='no') tmp%id
            end if 
            call this%leaf_layers(tmp%right)
        else
            return
        end if
    end subroutine leaf_layers


end module module_abbtree_layers

!este es para leer las capas del archivo json
module module_jsonReader_layers
    use json_module
    use module_layer
    use module_abbtree_layers
    implicit none
    
    type :: jsonReader_layers
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        logical :: found
        integer :: size
        contains
        procedure :: readJson_Layers
        procedure :: getText_abbjson
        procedure :: getInt_abbjson
    end type jsonReader_layers

    contains
    
    subroutine readJson_Layers(this,filename,tree)
        class(jsonReader_layers), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(abbtree_layers), intent(inout) :: tree
        type(pixelList) :: list
        integer :: i,j,id,no_childs,fila, columna        ! Se declaran variables enteras
        character(len=50) :: color
        type(layer), pointer :: capa
        logical :: found
        type(json_value), pointer :: actualchild,actualsubchild,pixelatribute
        
        call this%json%initialize()    ! Se inicializa el módulo JSON
        call this%json%load(filename=filename)  ! Se carga el archivo JSON llamado 'data.json'
        call this%json%info('',n_children=this%size)
        call this%json%get_core(this%jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call this%json%get('', this%listPointer, this%found)

        allocate(capa)
        if (this%found) then
            print *, "Se encontro el archivo"
        else
            print *, "No se encontro el archivo"
        end if
        print *, "size: ", this%size
        do i = 1, this%size         ! Se inicia un bucle sobre el número de elementos en el JSON
            call this%jsonc%get_child(this%listPointer, i,actualchild, found = found)
            if (found) then
                id = this%getInt_abbjson(poss=i,text="id_capa",actualchild=actualchild)
                if (.not.found) then
                    print *, "No se obtuvo el id_capa, poss: ", i
                    return
                end if
                call this%jsonc%get_child(actualchild,"pixeles",actualsubchild,found=found)
                call this%jsonc%info(actualsubchild,n_children=no_childs)
                if (.not.found) then
                    print *, "No se obtuvo el pixeles, poss: ", i
                    return
                end if
                do j= 1, no_childs
                    call this%jsonc%get_child(actualsubchild,j,pixelatribute)
                    fila = this%getInt_abbjson(poss=j,text="fila",actualchild=pixelatribute)
                    columna = this%getInt_abbjson(poss=j,text="columna",actualchild=pixelatribute)
                    color = this%getText_abbjson(poss=j,text="color",actualchild=pixelatribute)
                    call list%insertPixel(row=fila,col=columna,color=color)
                end do
                call tree%insertLayer(id=id,pixels=list)
                list = pixelList()
            end if
        end do
    end subroutine

    
    function getInt_abbjson(this,poss,text,actualchild) result(valueret)
        class(jsonReader_layers), intent(inout) :: this
        integer, intent(in) :: poss
        type(json_value),intent(in),pointer :: actualchild
        character(len=*), intent(in) :: text
        integer :: valueret
        logical :: found
        
        found = .false.
        call this%jsonc%get_child(actualchild, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'text' del hijo actual
        call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'valueret'
        if (.not.found) then
            print *, "No se obtuvo el numero, poss: ", poss
            valueret = 00000
        end if
    end function getInt_abbjson
    
    function getText_abbjson(this,poss,text,actualchild) result(valueret)
        class(jsonReader_layers), intent(inout) :: this
        integer, intent(in) :: poss
        type(json_value), intent(in),pointer :: actualchild
        character(len=*), intent(in) :: text
        character(:), allocatable :: valueret
        logical :: found

        found = .false.
        call this%jsonc%get_child(actualchild, text, this%attributePointer, found = found)
        if (.not.found) then
            print *, "No se obtuvo el texto, poss:", poss 
            valueret = "error"
        end if
        call this%jsonc%get(this%attributePointer, valueret)
        
    end function getText_abbjson
    
end module module_jsonReader_layers