module printerModule
    implicit none

    type, public :: paper
        integer :: steps
        type(paper), pointer :: next => null()
        end type paper
        
        type, public :: printer
        type(paper), pointer :: papel => null()
        type(printer), pointer :: next => null()
        contains
        procedure :: stepPrint
        procedure :: finishPaper
        end type printer
        
        type, public :: listPrinter
        type(printer), pointer :: bprinter
        type(printer), pointer :: sprinter
        contains
        procedure :: addbigPaper
        procedure :: addsmallPaper
        end type listPrinter
    
        contains
        
        !addPaper in the big paper printer
        subroutine addbigPaper(this)
            class(listPrinter), intent(inout) :: this
            type(paper), pointer :: newPaper
            allocate(newPaper)
            newPaper%steps = 2
            if (associated(this%bprinter%papel)) then
                this%bprinter%papel%next => newPaper
            else
                this%bprinter%papel => newPaper
            end if
        end subroutine addbigPaper
    
        !addPaper in the small paper printer
        subroutine addsmallPaper(this)
            class(listPrinter), intent(inout) :: this
            type(paper), pointer :: newPaper
            allocate(newPaper)
            newPaper%steps = 1
            if (associated(this%sprinter%papel)) then
                this%sprinter%papel%next => newPaper
            else
                this%sprinter%papel => newPaper
            end if
        end subroutine addsmallPaper

    !stepPrint
    subroutine stepPrint(this)
        class(printer), intent(inout) :: this
        this%papel%steps = this%papel%steps - 1
    end subroutine stepPrint

    !finishPaper
    subroutine finishPaper(this)
        class(printer), intent(inout) :: this
        type(paper), pointer :: Paper
        
        if (this%papel%steps == 0) then
            Paper => this%papel
            this%papel => this%papel%next
            deallocate(Paper)
        end if
    end subroutine finishPaper

end module printerModule