module imagesModule 
    implicit none
    private

    !node images
    type, public:: image
        character(:), allocatable :: type
        type(image), pointer :: next => null()
    end type image

    type, public :: stackImages
        type(image), pointer :: head
        contains
        procedure :: pushBigImg
        procedure :: pushSmallImg
        procedure :: remove
        procedure :: clean
    end type stackImages

    contains
    !pushBigImg
    subroutine pushBigImg(this)
        class(stackImages), intent(inout) :: this
        type(image), pointer :: newImage
        allocate(newImage)
        newImage => this%head
        if (associated(this%head)) then
            do while (associated(newImage%next))
                newImage => newImage%next
            end do
            newImage%next = image(type='big')
        else
            this%head => newImage
        end if
    end subroutine pushBigImg

    !pushSmallImg
    subroutine pushSmallImg(this)
        class(stackImages), intent(inout) :: this
        type(image), pointer :: newImage
        allocate(newImage)
        newImage => this%head
        if (associated(this%head)) then
            do while (associated(newImage%next))
                newImage => newImage%next
            end do
            newImage%next = image(type='small')
        else
            this%head => newImage
        end if
    end subroutine pushSmallImg

    !remove
    subroutine remove(this)
        class(stackImages), intent(inout) :: this
        type(image), pointer :: currentImage
        currentImage => this%head
        do while (associated(currentImage%next))
            currentImage => currentImage%next
        end do
        deallocate(currentImage)
    end subroutine remove

    !clean
    subroutine clean(this)
        class(stackImages), intent(inout) :: this
        type(image), pointer :: currentImage
        type(image), pointer :: nextImage
        currentImage => this%head
        do while (associated(currentImage%next))
            nextImage => currentImage%next
            deallocate(currentImage)
            currentImage => nextImage
        end do
        deallocate(currentImage)
    end subroutine clean

    !Graph_queue
    subroutine graph(this)
        class(stackImages), intent(in) :: this
        type(image), pointer :: current

        integer :: unit, count = 0

        open(unit, file="stackimages.dot", status="replace")
        write(unit, *) 'digraph G {'
        
        if (.not. associated(this%head)) then
            write(unit, *) '"empty" [label="Empty iamges", shape=box];'
        else
            current => this%head
            count = 0
            do while(associated(current))
                write(unit, *) count, '   "Node', count, '" [label="', current%type,'"];'
                if(associated(current%next)) then
                    write(unit, *) '   "Node', count, '" -> "Node', count+1, '";'
                end if
                count = count + 1
                current => current%next

            end do
        end if
        write(unit, *) '}'

        close(unit)
        call execute_command_line('dot -Tpng stackimages.dot -o stackimages.png')
    end subroutine graph

end module imagesModule