
module module_avlTree_images
    use module_abbtree_layers
    implicit none
    type :: image
        integer :: id
        integer :: height
        type(abbtree_layers) :: abb
        type(image), pointer :: left => null()
        type(image), pointer :: right => null()
    end type

    type :: avlTree_images
        type(image), pointer :: root => null()
        contains
        procedure :: insertImage
        procedure :: insertImage_rec !no usar por separado
        procedure :: preorderAVL
        procedure :: inorderAVL
        procedure :: postorderAVL
        procedure :: srl !no usar por separado
        procedure :: srr !no usar por separado
        procedure :: drl !no usar por separado
        procedure :: drr !no usar por separado
        procedure :: getheight !no usar por separado
        procedure :: getmax !no usar por separado
        procedure :: dotgen
        procedure :: dotgen_rec !no usar por separado
        procedure :: deleteImg
        procedure :: deleteImg_rec !no usar por separado
    end type

    contains
    subroutine insertImage(this, id,abb)
        class(avlTree_images), intent(inout) :: this
        integer, intent(in) :: id
        type(abbtree_layers), intent(in) :: abb
        type(image), pointer :: tmp
        if(associated(this%root)) then
            call this%insertImage_rec(tmp=this%root,id=id,abb=abb)
        else
            allocate(tmp)
            tmp%height = 0
            tmp%abb = abb
            tmp%id = id
            this%root => tmp
        end if
    end subroutine insertImage

    subroutine insertImage_rec(this,tmp, id,abb)
        class(avlTree_images), intent(inout) :: this
        integer, intent(in) :: id
        type(image), pointer, intent(inout) :: tmp
        class(abbtree_layers), intent(in) :: abb
        integer :: r, l, m
        if (.not. associated(tmp)) then
            allocate(tmp)
            tmp%height = 0
            tmp%abb = abb
            tmp%id = id
        else if (id == tmp%id) then
            tmp%abb = abb
        else if (id < tmp%id) then
            call this%insertImage_rec(tmp=tmp%left,id=id,abb=abb)
            if ((this%getheight(tmp%left) - this%getheight(tmp%right))==2) then
                if (id < tmp%left%id) then
                    tmp => this%srl(tmp)
                else
                    tmp => this%drl(tmp)
                end if
            end if
        else if (id > tmp%id) then
            call this%insertImage_rec(tmp=tmp%right,id=id,abb=abb)    
            if ((this%getheight(tmp%right) - this%getheight(tmp%left))==2) then
                if (id > tmp%right%id) then
                    tmp => this%srr(tmp)
                else
                    tmp => this%drr(tmp)
                end if
            end if
        else 
            
        end if
        r = this%getheight(tmp%right)
        l = this%getheight(tmp%left)
        m = this%getmax(r, l)
        tmp%height = m + 1
    end subroutine insertImage_rec

    integer function getheight (this, tmp)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        if (.not. associated(tmp)) then
            getheight = -1
        else
            getheight = tmp%height 
        end if
    end function getheight

    ! rotacion simple a la izquierda
    function srl(this, t1) result(t2)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: t1
        type(image), pointer :: t2 
        t2 => t1%left
        t1%left => t2%right
        t2%right => t1
        t1%height = this%getmax(this%getheight(t1%left), this%getheight(t1%right))+1
        t2%height = this%getmax(this%getheight(t2%left), t1%height)+1
    end function srl

    ! rotacion simple a la derecha
    function srr(this, t1) result(t2)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: t1
        type(image), pointer :: t2 
        t2 => t1%right
        t1%right => t2%left
        t2%left => t1
        t1%height = this%getmax(this%getheight(t1%left), this%getheight(t1%right))+1
        t2%height = this%getmax(this%getheight(t2%right), t1%height)+1
    end function srr

    ! doble rotacion a la izquierda
    function drl(this, tmp) result(res)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        type(image), pointer :: res
        tmp%left => this%srr(tmp%left)
        res => this%srl(tmp)
    end function drl

    !doble rotacion a la derecha
    function drr(this, tmp) result(res)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        type(image), pointer :: res
        tmp%right => this%srl(tmp%right)
        res => this%srr(tmp)
    end function drr

    integer function getmax(this, val1, val2)
        class(avlTree_images), intent(in) :: this
        integer, intent(in) :: val1, val2
        getmax = merge(val1, val2, val1 > val2)
    end function getmax

    subroutine preorderAVL(this, tmp)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        write (*, '(1I3)', advance='no') (tmp%id)
        call this%preorderAVL(tmp%left)
        call this%preorderAVL(tmp%right)
    end subroutine preorderAVL

    subroutine inorderAVL(this, tmp)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%inorderAVL(tmp%left)
        write (*, '(1I3)', advance='no') (tmp%id)
        call this%inorderAVL(tmp%right)
    end subroutine inorderAVL

    subroutine postorderAVL(this, tmp)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%postorderAVL(tmp%left)
        call this%postorderAVL(tmp%right)
        write (*, '(1I3)', advance='no') (tmp%id)
    end subroutine postorderAVL

    subroutine dotgen(this, filename)
        class(avlTree_images), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(:),allocatable :: path
        integer :: file
        path = "images/"//trim(adjustl(filename))//".dot"
        open(file, file=path, status="replace")
        write(file, '(A)') 'digraph{'
        call this%dotgen_rec(this%root, file)
        write(file, '(A)') '}'
        close(file)
        call execute_command_line("dot -Tsvg images/"//trim(adjustl(filename))//".dot > images/"//trim(adjustl(filename))//".svg")
    end subroutine dotgen

    subroutine dotgen_rec(this, tmp, unit)
        class(avlTree_images), intent(in) :: this
        type(image), intent(in), pointer :: tmp
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
        call this%dotgen_rec(tmp%left, unit)
        call this%dotgen_rec(tmp%right, unit)
    end subroutine dotgen_rec

    subroutine deleteImg(this, id)
        class(avlTree_images), intent(inout) :: this
        integer, intent(in) :: id

        this%root => this%deleteImg_rec(this%root, id)
    end subroutine deleteImg

    recursive function deleteImg_rec(this, root, id) result(res)
        class(avlTree_images), intent(inout) :: this
        type(image), pointer :: root
        integer, intent(in) :: id
        type(image), pointer :: res
        type(image), pointer :: temp

        if(.not. associated(root)) then
            res => root
            return
        end if 

        if(id < root%id) then
            root%left => this%deleteImg_rec(root%left, id)

        else if(id > root%id) then
            root%right => this%deleteImg_rec(root%right, id)
        
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
            else
                call getMajorOfMinorsImages(root%left, temp)
                root%id = temp%id
                root%left => this%deleteImg_rec(root%left, temp%id)
            end if
        end if

        res => root
        if(.not. associated(root)) return

        root%height = this%getmax(root%left%height, root%right%height) + 1

        if(getBalance(root) > 1) then
            if(getBalance(root%right) < 0) then
                ! root%right => rightRotation(root%right)
                ! root => leftRotation(root)
                root=>this%drr(root)

            else
                ! root => leftRotation(root)
                root => this%srr(root)
            end if
        end if

        if(getBalance(root) < -1) then
            if(getBalance(root%left) > 0) then
                ! root%left => leftRotation(root%left)
                ! root => rightRotation(root)
                root=>this%drl(root)

            else
                ! root => rightRotation(root)
                root => this%srl(root)
            end if
        end if
        res => root
    end function deleteImg_rec

    function getBalance(root) result(res)
        type(image), intent(in) :: root

        integer :: res
        res = root%right%height - root%left%height
    end function getBalance

    recursive subroutine getMajorOfMinorsImages(root, major)
        type(image), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinorsImages(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinorsImages

end module module_avlTree_images

!este es para leer las capas del archivo json (tengo que adaptarlo para que lea las imagenes)
module module_jsonReader_images
    use json_module
    use module_abbtree_layers
    use module_avlTree_images
    implicit none
    
    type, public :: jsonReader_images
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        character(:), allocatable :: filename
        logical :: found
        integer :: size
        contains
        procedure :: readJson_images
        procedure :: getInt_avlJson
        procedure :: Inicialice_avlJson
    end type jsonReader_images

    contains
    
    subroutine readJson_images(this,filename,abbPrincipalTree,avlPrincipalTree)
        class(jsonReader_images), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(abbtree_layers), intent(in) :: abbPrincipalTree
        type(avlTree_images), intent(inout) :: avlPrincipalTree
        type(abbtree_layers) :: abbTemp 
        type(image), pointer :: img_temp
        type(layer), pointer :: layer_temp
        type(layer), pointer :: layer_temp2
        integer :: i,id,num,currentnum        ! Se declaran variables enteras
        integer, dimension(:), allocatable :: array
        character(len=50) :: nombre
        logical :: found
        this%filename = filename
        call this%Inicialice_avlJson()  ! Se inicializa el módulo JSON
        do i = 1, this%size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            id = this%getInt_avlJson(poss=i,text="id") 
            call this%jsonc%get_child(this%listPointer, i, this%personPointer, found = found)
            call this%jsonc%get_child(this%personPointer, "capas", this%attributePointer, found = found)
            print *, "Imagen: ", id
            if (found) then
                call this%jsonc%get(this%attributePointer, array)
                do num = 1, size(array)
                    print *, ""
                    print *, "Capa: ", array(num)
                    currentnum = array(num)
                    layer_temp => abbPrincipalTree%searchLayer(currentnum)
                    if (.not. associated(layer_temp)) then
                        print *, "No se encontró la capa, error grave en la lectura del archivo json"
                        return
                    else
                        ! layer_temp2=layer(id=layer_temp%id,pixels=layer_temp%pixels)
                        ! do while (associated(layer_temp))
                        !     print *, "Capaaaaa: ", layer_temp%id
                        !     layer_temp => layer_temp%right
                        ! end do
                        call abbTemp%insertLayer(id=layer_temp%id,pixels=layer_temp%pixels)
                    end if
                    ! print *, layer_temp%id
                    ! write(*,"(I5)",advance="no") layer_temp%id
                    ! write(*,"(I5)",advance="no") array(num)
                end do
                write(nombre, '(I5)') id
                call abbTemp%graphABBTree("vuelta_"//trim(adjustl(nombre)))
                ! nombre =""
                ! print *, ""
                ! call avlPrincipalTree%insertImage(id=id,abb=abbTemp)
                ! abbTemp = abbtree_layers()
            end if
        end do
    end subroutine

    function getInt_avlJson(this,poss,text) result(valueret)
        class(jsonReader_images), intent(inout) :: this
        integer, intent(in) :: poss
        character(len=*), intent(in) :: text
        integer :: valueret
        integer :: i, size        ! Se declaran variables enteras
        logical :: found
        
        found = .false.
        call this%jsonc%get_child(this%listPointer, poss, this%personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
        call this%jsonc%get_child(this%personPointer, text, this%attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
        call this%jsonc%get(this%attributePointer, valueret)  ! Se obtiene el valor y se asigna a la variable 'nombre'
        if (.not.found) then
            print *, "No se encontró el valor"
            valueret = 00000
        end if
    end function getInt_avlJson

    subroutine Inicialice_avlJson(this)
        class(jsonReader_images), intent(inout) :: this
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
    end subroutine Inicialice_avlJson
end module module_jsonReader_images