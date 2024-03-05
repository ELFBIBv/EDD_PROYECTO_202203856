module PaperQueueModule 
    type, public :: paper
        character(len=10) :: type
        integer :: steps
        type(paper), pointer :: next => null()
    end type paper

    type, public :: PaperQueue
        type(paper), pointer :: head => null()
        type(paper), pointer :: tail => null()
    contains
        procedure :: addbigpaper
        procedure :: addsmallPaper
        procedure :: removepaper
        procedure :: printQueue
        procedure :: cleanPaperQueue
        procedure :: graphPapers
    end type PaperQueue

    contains
    !addbigpaper
    subroutine addbigpaper(self)
        class(PaperQueue), intent(inout) :: self
        type(paper), pointer :: newpaper
        type(paper), pointer :: current
        current => self%tail
        allocate(newpaper)
        newpaper%type = "big"
        newpaper%steps = 2

        !agregamos el papel al final
        if(associated(current)) then
            current%next => newpaper
            self%tail => newpaper
        else
            self%head => newpaper
            self%tail => newpaper
        end if
    end subroutine addbigpaper

    !addsmallpaper
    subroutine addsmallpaper(self)
        class(PaperQueue), intent(inout) :: self
        type(paper), pointer :: newpaper
        type(paper), pointer :: current
        current => self%tail
        allocate(newpaper)
        newpaper%type = "small"
        newpaper%steps = 1
        !agregamos el papel al final
        if(associated(current)) then
            current%next => newpaper
            self%tail => newpaper
        else
            self%head => newpaper
            self%tail => newpaper
        end if
    end subroutine addsmallpaper

    !removepaper
    subroutine removepaper(self)
        class(PaperQueue), intent(inout) :: self
        type(paper), pointer :: current
        current => self%head
        if(associated(current)) then
            self%head => current%next
            deallocate(current)
        end if
    end subroutine removepaper

    !printQueue
    subroutine printQueue(self)
        class(PaperQueue), intent(in) :: self
        type(paper), pointer :: current
        current => self%head
        print *, "-----------------"
        print *, "Paper Queue:"
        print *, "-----------------"
        do while(associated(current))
            write (*,fmt="(1x,a5)",advance="no") " ", "|", "->",current%type
            current => current%next
        end do
        print *, " "
    end subroutine printQueue

    !cleanPaperQueue
    subroutine cleanPaperQueue(self)
        class(PaperQueue), intent(inout) :: self
        type(paper), pointer :: current
        type(paper), pointer :: next
        current => self%head
        do while(associated(current))
            next => current%next
            deallocate(current)
            current => next
        end do
        self%head => null()
        self%tail => null()
    end subroutine cleanPaperQueue

    subroutine graphPapers(this,counter)
        class(PaperQueue), intent(in) :: this
        integer, intent(in) :: counter
        type(paper), pointer :: current
        character(len=100) :: path

        integer :: unit, count = 0
        write(path, "(a, i0, a)") "images\paperQueue", counter, ".dot"
        open(unit, file=path, status="replace")
        write(unit, *) 'digraph G {'
        
        if (.not. associated(this%head)) then
            write(unit, *) '"empty" [label="Empty iamges", shape=box];'
        else
            current => this%head
            count = 0
            do while(associated(current))
                write(unit, *) " ",'"Node', count, '" [label="', trim(current%type),'"];'
                if (associated(current%next)) then
                    write(unit, *) " ",'"Node', count, '" -> "Node', count+1, '";'
                end if
                    count = count + 1
                current => current%next
            end do
        end if
        write(unit, *) '}'
        close(unit)
        path=""
        write(path, "(a, i0, a, i0, a)") "dot -Tpng images\paperQueue",counter,".dot -o images\paperQueue", counter, ".png"
        call execute_command_line(trim(path))
    end subroutine graphPapers
end module PaperQueueModule