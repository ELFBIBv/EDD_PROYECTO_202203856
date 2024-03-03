module printerModule
    use PaperQueueModule
    implicit none

    type, public :: printer
        type(paper), pointer :: papeltail => null()
        type(paper), pointer :: paperhead => null()
        integer :: steps = 0
        contains
        !esta funcion solament será para comprobar si la hoja ya fue impresa
        procedure :: checkTime
    end type printer
    
    type, public :: listPrinter
        type(printer), pointer :: bprinter
        type(printer), pointer :: sprinter
        contains
        procedure :: addPaper
        procedure :: addSteps
    end type listPrinter

        contains

    !addPaper in all printers
    subroutine addPaper(this,printer_Queue)
        class(listPrinter), intent(inout) :: this
        class(printerQueue), intent(inout) :: printer_Queue
        type(paper), pointer :: actualPaper
        type(printer), pointer :: actualprinter

        actualPaper => printer_Queue%head
        allocate(actualPaper)

        do while (associated(actualPaper))
            !buscamos el tipo de impresora
            if (actualPaper%steps == 1) then
                actualprinter => this%sprinter
            else if (actualPaper%steps == 2) then
                actualprinter => this%bprinter
            end if
            !si la impresora tiene papel
            if (associated(actualprinter%paperhead)) then
                actualprinter%papeltail%next => actualPaper
                actualprinter%papeltail => actualPaper
            !si la impresora no tiene papel
            else
                actualprinter%paperhead => actualPaper
                actualprinter%papeltail => actualPaper
            end if
        end do
    end subroutine addPaper

    !stepPrint
    subroutine addSteps(this)
        class(listPrinter), intent(inout) :: this
        call this%sprinter%checkTime()
        call this%bprinter%checkTime()
    end subroutine addSteps

    !checkTime (no usar esta funcion de forma individual)
    subroutine checkTime(this)
        class(printer), intent(inout) :: this
        type(paper), pointer :: actualPaper
        this%steps = this%steps + 1
        if (associated(this%paperhead) .and. this%steps==this%paperhead%steps) then
            actualPaper => this%paperhead
            this%paperhead => this%paperhead%next
            deallocate(actualPaper)
            if (this%steps == 1) then
                print *, "La impresora pequeña ha impreso una hoja"
            else if (this%steps == 2) then
                print *, "La impresora grande ha impreso una hoja"
            else 
                print *, "error en la parte de impresoras"
            end if
            this%steps = 0
        end if
    end subroutine checkTime

end module printerModule