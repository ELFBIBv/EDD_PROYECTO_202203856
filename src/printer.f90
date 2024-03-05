module printerModule 
    use PaperQueueModule
    implicit none

    type, public :: printer
        type(paper),pointer :: papeltail => null()
        type(paper),pointer :: paperhead => null()
        integer :: steps = 0
        contains
        !esta funcion solament será para comprobar si la hoja ya fue impresa
        procedure :: checkTime
    end type printer
    
    type, public :: listPrinter
        type(printer), pointer :: bprinter
        type(printer), pointer :: sprinter
        contains
        procedure :: init
        procedure :: addPaper
        procedure :: addSteps
        procedure :: printList
    end type listPrinter

    contains

    !init
    subroutine init(this)
        class(listPrinter), intent(inout) :: this
        allocate(this%sprinter)
        allocate(this%bprinter)
    end subroutine init

    !addPaper in all printers
    subroutine addPaper(this,PaperList)
        class(listPrinter), intent(inout) :: this
        class(PaperQueue), intent(inout) :: PaperList
        type(paper), pointer :: actualPaper
        type(printer) , pointer:: actualprinter
        integer :: i

        actualPaper => PaperList%head
        do while (associated(actualPaper))
            print *, "actualPaper", actualPaper%steps
            !buscamos el tipo de impresora
            if (actualPaper%steps == 1) then
                actualprinter => this%sprinter
            else if (actualPaper%steps == 2) then
                actualprinter => this%bprinter
            end if
            !si la impresora tiene papel
            if (associated(actualprinter%paperhead)) then
                print *, "algo5"
                actualprinter%papeltail%next => actualPaper
                print *, "algo6"
                actualprinter%papeltail => actualPaper
                !si la impresora no tiene papel
            else
                print *, "algo7"
                actualprinter%paperhead => actualPaper
                print *, "algo8"
                actualprinter%papeltail => actualPaper
            end if
            read *, i
            actualPaper => actualPaper%next
        end do
    end subroutine addPaper

    !stepPrint
    subroutine addSteps(this)
        class(listPrinter), intent(inout) :: this
        call this%sprinter%checkTime()
        call this%bprinter%checkTime()
    end subroutine addSteps

    !printList
    subroutine printList(this)
        class(listPrinter), intent(inout) :: this
        type(paper), pointer :: actualPaper
        actualPaper => this%sprinter%paperhead
        print *, "----------------------"
        print *, "Impresora pequeña"
        do while (associated(actualPaper))
            write (*,fmt="(1x,a5)",advance="no") " ", "|small image|"
            actualPaper => actualPaper%next
        end do
        print *, "----------------------"
        print *, "Impresora grande"
        actualPaper => this%bprinter%paperhead
        do while (associated(actualPaper))
            write (*,fmt="(1x,a5)",advance="no") " ", "|big image|"
            actualPaper => actualPaper%next
        end do
    end subroutine printList
    
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