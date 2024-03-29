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
    procedure :: searchImage
    end type
    contains
    subroutine addImage(this,id)
        class(imageList), intent(inout) :: this
        integer, intent(in) :: id
        type(image), pointer :: newImage
        allocate(newImage)
        newImage%id = id
        if (.not. associated(this%head)) then
            this%head => newImage
            this%tail => newImage
        else
            this%tail%next => newImage
            this%tail => newImage
        end if
    end subroutine addImage

    subroutine deleteImage(this,id)
        class(imageList), intent(inout) :: this
        integer, intent(in) :: id
        type(image),pointer :: current
        type(image), pointer :: previous
        current => this%head
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
    
    subroutine searchImage(this,id)
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
    end subroutine searchImage
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
    end type

    contains

    !subrutinas o funciones para la lista de almbumes
    subroutine addAlbum(this,name)
        class(albumList), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(album), pointer :: newAlbum
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
        character(:), allocatable :: path
        integer :: file
        path = "images\"//trim(adjustl(filename))//".dot"
        open(file, file=path, status="replace")
        write(file, *) 'digraph G {'
        do while (associated(current))
            current => this%head
            write(file, *) '"Album', trim(adjustl(current%name)), '" [label="', trim(adjustl(current%name)),'"];'
            if (associated(current%images%head)) then
                currentImage => current%images%head
                write(file, *) '"Image', currentImage%id, '" [label="', currentImage%id,'"];'
                write(file, *) '"Album', trim(adjustl(current%name)), '" -> "Image', currentImage%id, '";'
                do while (associated(currentImage%next))
                    write(file, *) '"Image', currentImage%next%id, '" [label="', currentImage%next%id,'"];'
                    write(file, *) '"Image', currentImage%id, '" -> "Image', currentImage%next%id, '";'
                    currentImage => currentImage%next
                end do
            end if
            if (associated(current%next)) then
                write(file, *) '"Album', trim(adjustl(current%name)), '" [label="', trim(adjustl(current%name)),'"];'
                write(file, *) '"Album', trim(adjustl(current%name)), '" -> "Album', trim(adjustl(current%next%name)), '";'
            end if
            current => current%next
        end do
        write(file, *) '}'
        close(file)
        call execute_command_line(trim("dot -Tpng images\albumes.dot -o images\albumes.png"))
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

end module linkedListAlbum

module module_ordinal_user
    use module_abbtree_layers
    use module_avlTree_images
    implicit none

    type ordinal_user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    type(abbtree_layers),allocatable :: PrincipalLayersTree
    type(avlTree_images),allocatable :: PrincipalImagesTree

    contains
    ! procedure :: ver_reportes_estructuras
    ! procedure :: navegacion_imgs_gestion_imgs
    ! procedure :: carga_masiva_capas
    ! procedure :: carga_masiva_imagenes
    ! procedure :: carga_masiva_albumes
    end type

    contains

end module

module module_btree
    use module_ordinal_user
    implicit none

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
                ! write(file, *) "//chekpoint3"
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
                print *, "DPI", myNode2%val(1)%DPI, "num", myNode2%num, "password", trim(adjustl(myNode2%val(1)%password))
                do i = 0, myNode2%num-1
                    actualuser => myNode2%val(i+1)
                    !entra entre los 2 nodos que el cree conveniente es otra forma de escribir
                        ! if (myNode%val(i)%DPI < DPI < myNode%val(i+1)%DPI) then
                        ! else if (DPI<myNode%val(i+1)%DPI) then
                        !     myNode => myNode%link(i)%ptr
                        !     exit
                        !si es mayor que el ultimo valor entra al ultimo link
                    print *, "vuelta", i, "DPI", actualuser%DPI
                    if (DPI<actualuser%DPI) then
                        if (associated(myNode2%link(i)%ptr)) then
                            myNode2 => myNode2%link(i)%ptr
                            exit
                        else
                            print *, "No se encontro el usuario"
                            return
                        end if
                        !si es igual al primer nodo entonces va a devolver ese nodo
                    else if (actualuser%DPI==DPI .and. trim(adjustl(actualuser%password))==trim(adjustl(password))) then
                        user => myNode2%val(i+1)
                        found = .true.
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
    end function getUser

end module module_btree