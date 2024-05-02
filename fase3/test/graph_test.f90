module moduleRoutes2
    implicit none

    type edge
        integer :: id
        integer :: weight
        integer :: parent_id
        integer :: printers
        type(edge), pointer :: next => null()
        type(edge), pointer :: prev => null()
    end type edge

    type edge_list
        type(edge), pointer :: head => null()
        type(edge), pointer :: tail => null()
        contains
        procedure :: add_sorted
        procedure :: pop
        procedure :: is_empty
        procedure :: merge
        procedure :: add_weight
        procedure :: get_size
    end type edge_list

    type result
        integer :: id
        integer :: weight
        type(result), pointer :: next => null()
    end type result

    type result_list
        integer :: total_weight
        integer :: total_printers
        type(result), pointer :: head => null()
        type(result), pointer :: tail => null()
        contains
        procedure :: add_result
        procedure :: print
    end type result_list

    type node
        integer :: id 
        type(edge_list) :: neighbors
        type(node), pointer :: next => null()
    end type node

    type graph
        integer :: n_nodes
        type(node), pointer :: head => null()
        contains
        procedure :: insert_data
        procedure :: add_node
        procedure :: add_edge
        procedure :: get_node
        procedure :: show
        procedure :: graphic
        !procedure :: graphic_rec
    end type graph

    type analyzer
        type(graph), pointer :: graph_data => null()
        contains
        procedure :: set_graph
        procedure :: get_shortest_path
        procedure :: get_longest_path
    end type analyzer

    contains
    ! Edge list methods
    subroutine add_sorted(this, id, weight, parent_id, sort_by_id)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id
        logical, intent(in) :: sort_by_id
        type(edge), pointer :: new_edge
        type(edge), pointer :: current
        type(edge), pointer :: previous
        allocate(new_edge)
        new_edge%id = id
        new_edge%weight = weight
        new_edge%parent_id = parent_id

        if (.not. associated(this%head)) then
            this%head => new_edge
            this%tail => new_edge
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight > weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_edge%next => this%head
            this%head%prev => new_edge
            this%head => new_edge
        else if (.not. associated(current)) then
            this%tail%next => new_edge
            new_edge%prev => this%tail
            this%tail => new_edge
        else
            previous%next => new_edge
            new_edge%prev => previous
            new_edge%next => current
            current%prev => new_edge
        end if
    end subroutine add_sorted

    function pop(this) result(edge_res)
        class(edge_list), intent(inout) :: this
        type(edge), pointer :: edge_res
        if (.not. associated(this%head)) then
            edge_res => null()
            return
        end if
        edge_res => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => null()
        else
            this%tail => null()
        end if
    end function pop

    function is_empty(this) result(res)
        class(edge_list), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function is_empty

    subroutine merge(this,  to_merge)
        class(edge_list), intent(inout) :: this
        class(edge_list), intent(in) :: to_merge
        type(edge), pointer :: current

        current => to_merge%head
        do while (associated(current))
            call this%add_sorted(current%id, current%weight, current%parent_id, .FALSE.)
            current => current%next
        end do
        
    end subroutine merge

    subroutine add_weight(this, weight)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: weight
        type(edge), pointer :: current
        current => this%head
        do while (associated(current))
            current%weight = current%weight + weight
            current => current%next
        end do        
    end subroutine add_weight

    function get_size(this) result(size)
        class(edge_list), intent(in) :: this
        integer :: size
        type(edge), pointer :: current
        current => this%head
        size = 0
        do while (associated(current))
            size = size + 1
            current => current%next
        end do
    end function get_size

    ! Result list methods
    subroutine add_result(this,  id, weight)
        class(result_list), intent(inout) :: this
        integer, intent(in) :: id, weight
        type(result), pointer :: new_result
        allocate(new_result)
        new_result%id = id
        new_result%weight = weight
        if (.not. associated(this%head)) then
            this%head => new_result
            this%tail => new_result
            return
        end if
        this%tail%next => new_result
        this%tail => new_result  
        this%total_weight = this%tail%weight  
    end subroutine add_result

    subroutine print(this)
        class(result_list), intent(in) :: this
        type(result), pointer :: current
        current => this%head
        do while (associated(current))
            write(*,'(A, I0, A, I0)') 'Node: ', current%id, ", Acumulated Weight: ", current%weight
            current => current%next
        end do
    end subroutine print
    ! Graph methods
    subroutine insert_data(this, id, neighbor_id, weight)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight    
        type(node), pointer :: current

        current => this%get_node(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            print *, "id: ",neighbor_id, "parent id: ", this%head%id
            call this%add_edge(neighbor_id, weight, this%head)
        else
            call this%add_edge(neighbor_id, weight, current)
        end if
    end subroutine insert_data

    subroutine add_node(this,  id)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: new_node
        allocate(new_node)
        new_node%id = id
        
        if (.not. associated(this%head)) then
            this%head => new_node
            return
        end if

        new_node%next => this%head
        this%head => new_node
    end subroutine add_node

    subroutine add_edge(this, id, weight, parent)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight
        type(node),intent(in), pointer :: parent
        type(node), pointer :: edge_node 
        ! edge_node => this%get_node(id)
        print *, "id: ",id, "parentid: ", parent%id
        ! if ( .NOT. associated(edge_node) ) then
        !     call this%add_node(id)
        ! end if
        call parent%neighbors%add_sorted(id, weight, parent%id, .TRUE.)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_edge

    function get_node(this, id) result(retval)
        class(graph), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: retval
        type(node), pointer :: current
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                retval => current
                return
            end if
            current => current%next
        end do
        retval => null()
    end function get_node

    subroutine show(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(edge), pointer :: current_edge
        current => this%head
        do while (associated(current))
            write(*,'(A, I0)') 'Node: ', current%id
            current_edge => current%neighbors%head
            do while (associated(current_edge))
                write(*,'(A, I0, A, I0, A)', advance='no') 'Edge: ', current_edge%id, ", " ,current_edge%weight, " "
                current_edge => current_edge%next
            end do
            write(*,*) ''
            current => current%next
        end do
    end subroutine show
    ! Analyzer methods  
    subroutine set_graph(this,  graph_p)
        class(analyzer), intent(inout) :: this
        type(graph), pointer, intent(in) :: graph_p
        this%graph_data => graph_p
    end subroutine set_graph

    function get_shortest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: weight,id
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        print *, 'Getting shortest path from ', id_origin, ' to ', id_destination
        
        allocate(retval)
        retval%total_weight = 0
        allocate(queue)
        current_node => this%graph_data%get_node(id_origin)
        
        if ( associated(current_node) ) then
            call queue%merge(current_node%neighbors) !se agregan todas las aristas del nodo origen
            call retval%add_result(id_origin, 0)
        end if
        
        do while ( .NOT. queue%is_empty() )
            current_edge => queue%pop()
            weight = current_edge%weight
            id = current_edge%id
            current_node => this%graph_data%get_node(id)
            if ( .NOT. associated(current_node) ) then
                print *, 'Node not found'
                exit
            end if
            if (current_node%id == id_destination) then
                print *, 'Found destination'
                call retval%add_result(current_node%id, weight)
                exit
            end if
            call current_node%neighbors%add_weight(weight)
            call queue%merge(current_node%neighbors)
            call retval%add_result(current_node%id, weight)
            current_node => current_node%next
        end do
        print *, "La ruta mas optima es:"
        call retval%print()
    end function get_shortest_path

    function get_longest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue
        type(node), pointer :: current_node
        type(edge), pointer :: current_arista
        integer, allocatable :: max_weights(:)
        logical, allocatable :: visited(:)
    
        allocate(retval)
        retval%total_weight = 0
        retval%total_printers = 0
        allocate(queue)
        allocate(max_weights(this%graph_data%n_nodes))
        allocate(visited(this%graph_data%n_nodes))
        max_weights = -HUGE(0)  ! Inicializar todos los pesos como negativos
        visited = .false.
    
        current_node => this%graph_data%get_node(id_origin)
        if ( associated(current_node) ) then
            call queue%merge(current_node%neighbors)
            call retval%add_result(id_origin, 0)
            max_weights(current_node%id) = 0
        end if
    
        do while ( .NOT. queue%is_empty())
            current_arista => queue%pop()
            sub_total = current_arista%weight + max_weights(current_arista%parent_id)
            sub_total_printers = current_arista%printers
            if (sub_total > max_weights(current_arista%id)) then
                max_weights(current_arista%id) = sub_total
                current_node => this%graph_data%get_node(current_arista%id)
                if ( .NOT. associated(current_node) ) then
                    print *, 'No se encontró el nodo'
                    exit
                end if
                if (current_node%id == id_destination) then
                    print *, 'Se encontró la sucursal destino'
                    call retval%add_result(current_node%id, sub_total)
                    exit
                end if
                if (.not. visited(current_node%id)) then
                    call current_node%neighbors%add_weight(sub_total)
                    !call current_node%neighbors%add_weight(sub_total - max_weights(current_node%id), sub_total_printers)
                    call queue%merge(current_node%neighbors)
                    call retval%add_result(current_node%id, sub_total)
                    visited(current_node%id) = .true.
                end if
            end if
        end do
    end function get_longest_path
!
    function successors(node) result(suc)
        integer, intent(in), dimension(2) :: node
            integer, dimension(:, :), allocatable :: suc
            if (node(1) == 1) then
                suc = reshape([2, 3, node(2)+5, node(2)+3],[2,2])
            elseif (node(1) == 2) then
                suc = reshape([4, node(2)+3], [1, 2])
            elseif (node(1) == 3) then
                suc = reshape([4, 5, node(2)+2, node(2)+2], [2, 2])
            elseif (node(1) == 4) then
                suc = reshape([6, node(2)+4], [1, 2])
            elseif (node(1) == 5) then
                suc = reshape([6, node(2)+9], [1, 2])
           else
                allocate(suc(0,0))
            endif
    end function successors
    
    
    
    
    function concatenate_matrices(mat1, mat2) result(concatenated)
            integer, dimension(:, :), intent(in) :: mat1, mat2
            integer, dimension(:,:), allocatable :: concatenated
    
            integer :: rows1, cols1, rows2, cols2
            integer :: i, j
    
            rows1 = size(mat1, 1)
            cols1 = size(mat1, 2)
            rows2 = size(mat2, 1)
            cols2 = size(mat2, 2)
    
            allocate(concatenated(rows1 + rows2, cols1))
    
            do i = 1, rows1
                do j = 1, cols1
                        concatenated(i, j) = mat1(i, j)
                end do
            end do
    
            do i = 1, rows2
                do j = 1, cols2
                        concatenated(rows1 + i, j) = mat2(i, j)
                end do
            end do
    end function concatenate_matrices
    
    subroutine sort(matrix)
            integer, dimension(:, :), intent(inout) :: matrix
            integer :: i, j, temp1, temp2
    
            do i = 1, size(matrix, 1) - 1
                do j = 1, size(matrix, 1) - i
                        if (matrix(j, 2) < matrix(j+1, 2)) then
                            temp1 = matrix(j, 1)
                            matrix(j, 1) = matrix(j+1, 1)
                            matrix(j+1, 1) = temp1
    
                            temp2 = matrix(j, 2)
                            matrix(j, 2) = matrix(j+1, 2)
                            matrix(j+1, 2) = temp2
                        end if
                end do
            end do
    end subroutine sort
!

    subroutine graphic(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(edge),pointer :: current_edge
        integer :: unit, id,weight
        open(unit, file='images/route.dot',status='replace')

        write(unit, '(A)') 'digraph {'
        current => this%head
        do while (associated(current))
            id = current%id
            current_edge=> current%neighbors%head
            do while (associated(current_edge))
                weight =current_edge%weight
                write(unit,"(A,I5,A,I5,A,I5,A,I5,A)") " ",id,"->",current_edge%id,"[weight=",weight,"][label=",weight,"]"
                current_edge=>current_edge%next
            end do 
            current => current%next
        end do
        write(unit,'(A)') "}"
        close(unit)
        call execute_command_line('dot -Tpng images/route.dot -o images/route.png')
        call system("eog images/route.png") !si da error se debe de usar "unset GTK_PATH"
        

    end subroutine graphic

end module moduleRoutes2

program graph_test
    use moduleRoutes2
    implicit none
    type(graph), pointer :: g
    type(analyzer) :: a
    type(result_list), pointer :: r

    allocate(g)
    print *, "Creating graph..."
    call g%insert_data(21, 2, 5)
    call g%insert_data(1, 2, 10)
    call g%insert_data(5, 10, 21)
    
    print *, "Graph created."
    call g%graphic()
    !call g%show()
    call a%set_graph(g)
    r => a%get_longest_path(2, 5)
    
    call r%print()
end program graph_test