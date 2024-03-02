module paperPrinterModule

    type, public :: paper
        integer :: steps
        type(paper), pointer :: next => null()
        contains
        procedure :: addbigPaper
        procedure :: addsmallPaper
    end type paper

        !stepPrint
    subroutine stepPrint(this)
        class(printer), intent(inout) :: this
        this%papel%steps = this%papel%steps - 1
    end subroutine stepPrint


end module paperPrinterModule