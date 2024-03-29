module module_Matrix
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        logical :: value
    end type node_val

    type :: node
        private
        integer :: i, j
        character(7) :: color
        type(node), pointer :: up => null()
        type(node), pointer :: down => null()
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: printColumnHeaders
        procedure :: getValue
        ! procedure :: printRowHeaders
    end type
    contains

    !con esto vamos a ingresar un valor en la matriz
    !haciendo uso de las coordenadas i (fila), j (columna)
    subroutine insert(self, i, j, color) 
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(7), intent(in) :: color

        type(node), pointer :: new
        type(node), pointer :: row
        type(node), pointer :: column

        allocate(new)
        new = node(i=i, j=j, color=color)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1, color="#FFFFFF")
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%width) self%width = j
        if(i > self%height) self%height = i

        if(.not. self%nodeExists(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertRowHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInRow(new, column)
        end if
    end subroutine insert

    !vamos a obtener la columna principal
    function searchColumn(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%j == j) return
            actual => actual%right
        end do
    end function searchColumn

    !vamos a obtener la fila principal
    function searchRow(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%down
        end do
    end function searchRow
    
    !vamos a verificar si el nodo existe
    !si existe, vamos a actualizar su valor
    function nodeExists(self, new) result(exists)
        class(matrix), intent(inout) :: self  
        type(node), pointer :: new
        
        logical :: exists
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        rowHeader => self%root
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%i == new%i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == new%j) then
                        column%color = new%color
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExists

    !vamos a insertar una fila principal
    function insertRowHeader(self, i) result(newRowHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i

        type(node), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node(i=i, j=-1,color="#FFFFFF")
        call self%insertInRow(newRowHeader, self%root)
    end function insertRowHeader
    
    !vamos a insertar un valor en una fila especifica
    subroutine insertInRow(self, new, rowHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: rowHeader

        type(node), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%i < actual%down%i .and. new%i > actual%i) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInRow

    !vamos a insertar una columna principal
    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: j

        type(node), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node(i=-1, j=j,color="#FFFFFF")
        call self%insertInColumn(newColumnHeader, self%root)
    end function insertColumnHeader

    !vamos a insertar un valor en una columna especifica
    subroutine insertInColumn(self, new, columnHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: columnHeader
        
        type(node), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%j < actual%right%j .and. new%j > actual%j) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    !vamos a imprimir la matriz
    subroutine print(self)
        class(matrix), intent(inout) :: self  
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%down

        call self%printColumnHeaders()

        do i = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(A3)', advance='no') " "
                else
                    write(*, fmt='(L3)', advance='no') val%value
                end if
            end do
        end do
    end subroutine print

    !vamos a imprimir las cabeceras de las columnas
    subroutine printColumnHeaders(self)
        class(matrix), intent(in) :: self
        integer :: j

        do j=-1, self%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    !vamos a obtener el valor de la matriz
    function getValue(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node_val) :: val
        rowHeader => self%root

        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        val%value = .true.
                        val%exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue
end module module_Matrix
