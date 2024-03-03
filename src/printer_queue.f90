module PaperQueueModule
    type, public :: paper
        character(len=10) :: type
        integer :: steps
        type(paper), pointer :: next => null()
    end type paper

    type, public :: printerQueue
        type(paper), pointer :: head => null()
        type(paper), pointer :: tail => null()
    contains
        procedure :: addbigpaper
        procedure :: addsmallPaper
        procedure :: removepaper
        procedure :: printQueue
    end type printerQueue

    contains
    !addbigpaper
    subroutine addbigpaper(self)
        class(printerQueue), intent(inout) :: self
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
        class(printerQueue), intent(inout) :: self
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
        class(printerQueue), intent(inout) :: self
        type(paper), pointer :: current
        current => self%head
        if(associated(current)) then
            self%head => current%next
            deallocate(current)
        end if
    end subroutine removepaper

    !printQueue
    subroutine printQueue(self)
        class(printerQueue), intent(in) :: self
        type(paper), pointer :: current
        current => self%head
        do while(associated(current))
            write (*,fmt="(1x,a,i0)",advance="no") "|",current%type,"|", "->"
            current => current%next
        end do
    end subroutine printQueue
end module PaperQueueModule