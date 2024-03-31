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
        procedure :: graphMatrix
        procedure :: graphTable
        procedure :: cleanMatrix
        ! procedure :: printRowHeaders
    end type
    contains

    !con esto vamos a ingresar un valor en la matriz
    !haciendo uso de las coordenadas i (fila), j (columna)
    subroutine insert(this, i, j, color) 
        class(matrix), intent(inout) :: this  
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(7), intent(in) :: color

        type(node), pointer :: new
        type(node), pointer :: row
        type(node), pointer :: column
        allocate(new)
        new = node(i=i, j=j, color=color)
        if(.not. associated(this%root)) then
            allocate(this%root)
            print *, "Creando nodo raiz"
            this%root = node(i=-1, j=-1, color="#FFFFFF")
        end if
        row => this%searchRow(i)
        column => this%searchColumn(j)
        if(j > this%width) this%width = j
        if(i > this%height) this%height = i
        if(.not. this%nodeExists(new)) then
            if(.not. associated(column)) then
                column => this%insertColumnHeader(j)
            end if
            if(.not. associated(row)) then
                row => this%insertRowHeader(i)
            end if
            call this%insertInColumn(new, row)
            call this%insertInRow(new, column)
        end if
    end subroutine insert

    !vamos a obtener la columna principal
    function searchColumn(this, j) result(actual)
        class(matrix), intent(in) :: this
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => this%root
        do while(associated(actual))
            if(actual%j == j) then 
                return
            end if 
            actual => actual%right
        end do
        actual => null()
    end function searchColumn

    !vamos a obtener la fila principal
    function searchRow(this, i) result(actual)
        class(matrix), intent(inout) :: this
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => this%root
        do while(associated(actual))
            if(actual%i == i) then
                return
            end if 
            actual => actual%down
        end do
        actual => null()
    end function searchRow
    
    !vamos a verificar si el nodo existe
    !si existe, vamos a actualizar su valor
    function nodeExists(this, new) result(exists)
        class(matrix), intent(inout) :: this  
        type(node), pointer :: new
        
        logical :: exists
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        rowHeader => this%root
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
    function insertRowHeader(this, i) result(newRowHeader)
        class(matrix), intent(inout) :: this  
        integer, intent(in) :: i

        type(node), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node(i=i, j=-1,color="#FFFFFF")
        call this%insertInRow(newRowHeader, this%root)
    end function insertRowHeader
    
    !vamos a insertar un valor en una fila especifica
    subroutine insertInRow(this, new, rowHeader)
        class(matrix), intent(inout) :: this
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
    function insertColumnHeader(this, j) result(newColumnHeader)
        class(matrix), intent(inout) :: this  
        integer, intent(in) :: j

        type(node), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node(i=-1, j=j,color="#FFFFFF")
        call this%insertInColumn(newColumnHeader, this%root)
    end function insertColumnHeader

    !vamos a insertar un valor en una columna especifica
    subroutine insertInColumn(this, new, columnHeader)
        class(matrix), intent(inout) :: this
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
    subroutine print(this)
        class(matrix), intent(inout) :: this  
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node), pointer :: val
        aux => this%root%down

        call this%printColumnHeaders()

        do i = 0, this%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, this%width
                val => this%getValue(i,j)
                if(associated(val)) then
                    write(*, fmt='(L3)', advance='no') 1
                else
                    write(*, fmt='(A3)', advance='no') " "
                end if
            end do
        end do
    end subroutine print

    !vamos a imprimir las cabeceras de las columnas
    subroutine printColumnHeaders(this)
        class(matrix), intent(in) :: this
        integer :: j

        do j=-1, this%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    !vamos a obtener el valor de la matriz
    function getValue(this, i, j) result(val)
        class(matrix), intent(in) :: this
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node), pointer :: val
        rowHeader => this%root
        val => null()
        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        print *, "Nodo encontrado"
                        val => column
                        return
                    end if
                    column => column%right
                end do
                print *, "No existe el nodo, (getValue Matrix)"
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue

    !vamos a graficar la matriz
    subroutine graphMatrix(this,filename)
        class(matrix), intent(in) :: this
        
        integer :: unit,i
        character(len=10) :: str_i,str_j,str_i_aux,str_j_aux
        character(len=150) :: node_dec
        character(len=20) :: nombre
        character(len=*),intent(in) :: filename

        character(:), allocatable :: rank
        character(:), allocatable :: conexion
        character(:), allocatable :: conexionRev
        type(node), pointer :: fila_aux
        type(node), pointer :: columna_aux
        fila_aux => this%root

        open(unit, file="images/"//trim(adjustl(filename))//".dot",status="replace")

        write(unit, *) "digraph Matrix {"
        write(unit, *) 'node[shape = "box"]'

        do while (associated(fila_aux))
            rank = "{rank=same"
            columna_aux => fila_aux
            do while(associated(columna_aux)) 
                write(str_i, '(I10)') columna_aux%i + 1
                write(str_j, '(I10)') columna_aux%j + 1
                nombre = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'

                if (columna_aux%i == -1 .and. columna_aux%j == -1) then
                    node_dec = trim(adjustl(nombre))//'[label = "root", group="'//trim(adjustl(str_i))//'"]'

                else if(columna_aux%i == -1) then
                    write(str_j_aux, '(I10)') columna_aux%j
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(str_j_aux))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                    
                else if(columna_aux%j == -1) then
                    write(str_i_aux, '(I10)') columna_aux%i
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(str_i_aux))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                        
                else
                    node_dec = trim(adjustl(nombre))//'[label = "'//" "
                    node_dec = trim(adjustl(node_dec))//'", style = filled, fillcolor= "'//&
                    & trim(adjustl(columna_aux%color))//'" group="'// &
                    & trim(adjustl(str_i))//'"]'

                end if
                write(unit, *) node_dec

                if(associated(columna_aux%right)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%right%i + 1
                    write(str_j_aux, '(I10)') columna_aux%right%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(unit, *) conexion
                    write(unit, *) conexionRev
                end if

                if(associated(columna_aux%down)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%down%i + 1
                    write(str_j_aux, '(I10)') columna_aux%down%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(unit, *) conexion
                    write(unit, *) conexionRev
                end if

                rank = rank//';"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                columna_aux => columna_aux%right
            end do
            rank = rank//'}'
            write(unit, *) rank

            fila_aux => fila_aux%down
        end do
        write(unit, *) "}"
        close(unit)
        !el -Gnslimit=2 indica el numero de iteraciones que hará graphviz para ajustar los nodos
        call execute_command_line("dot  -Gnslimit=2 -Tpng images/"//trim(adjustl(filename))//&
        &".dot -o images/"//trim(adjustl(filename))//".png", exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
                !windows
                call system("start images\"//trim(adjustl(filename))//".png")
                ! linux 
                ! call system("xdg-open images\"//trim(adjustl(filename))//".png")
        end if
    end subroutine graphMatrix


    ! vamos a graficar la imagen con tablas
    subroutine graphTable(this, filename,text)
        class(matrix), intent(in) :: this
        character(len=*), intent(in) :: filename,text
        character(:),allocatable :: path
        integer :: i, j,unit
        type(node), pointer :: val
        path = "images/"//trim(adjustl(filename))//".dot"
        ! Open the DOT file for writing
        open(unit, file=path, status='replace')
        print *, "checpoint 2"
        ! Write the DOT code to the file
        write(unit, '(A)') "digraph{"
        write(unit, '(A)') "  node [ shape=plaintext fontname=Helvetica ]"
        write(unit, '(A)') ""
        print *, "checpoint 3"
        write(unit, '(A)') '  a [ label="' // trim(adjustl(text)) // '"]'
        write(unit, '(A)') "  b [ label = <"
        write(unit, '(A)') "    <table border=""0"" cellborder=""0"" cellspacing=""0"" bgcolor=""white"">"
        print *, "checpoint 4"
        do i = 0, this%height
            print *, "checpoint 5"
            write(unit, '(A)') "      <tr>"
            do j = 0, this%width
                print *, "checpoint 6"
                val => this%getValue(i, j)
                print *, "checpoint 7"
                if (.not. associated(val)) then
                    write(unit, '(A)') "        <td></td>"
                else
                    write(unit, '(A)') "        <td bgcolor=""" // trim(val%color) // """></td>"
                end if
            end do
            print *, "checpoint 8"
            write(unit, '(A)') "      </tr>"
        end do
        print *, "checpoint 9"
        write(unit, '(A)') "    </table>"
        write(unit, '(A)') "  > ]"
        write(unit, '(A)') ""
        write(unit, '(A)') "}"
        close(unit)
        call system("dot -Gnslimit=2 -Tpng -o "//"images/"// trim(filename)//".png"&
        & // " " // "images/"//trim(adjustl(filename))//".dot")
        call system("start " // "images/"// trim(filename) // ".png")
        !windows
        ! call system("start images\"//trim(adjustl(filename))//".png")
        ! linux 
        ! call system("xdg-open images\"//trim(adjustl(filename))//".png")
    end subroutine graphTable

    !vamos a limpiar la matriz
    subroutine cleanMatrix(this)
        class(matrix), intent(inout) :: this
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node), pointer :: aux
        type(node), pointer :: aux2
        rowHeader => this%root
        column => rowHeader%right
        do while(associated(column))
            print *,"limpiando columna: ", column%i,"-", column%j
            aux => column
            column => column%right
            deallocate(aux)
        end do
        rowHeader => rowHeader%down
        do while(associated(rowHeader))
            column => rowHeader%right
            do while(associated(column))
                print *,"limpiando columna: ", column%i,"-", column%j
                aux => column
                column => column%right
                deallocate(aux)
            end do
            aux2 => rowHeader
            rowHeader => rowHeader%down
            print *,"limpiando fila: ", aux2%i,"-", aux2%j
            deallocate(aux2)
        end do
        deallocate(this%root)
        this%width = 0
        this%height = 0
        this%root => null()
    end subroutine cleanMatrix
end module module_Matrix
