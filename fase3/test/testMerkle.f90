! SHA256
! Author: Mikael Leetmaa 2014 under zlib License
! Modifications: Luis Espino 2024

module sha256_module
    use iso_c_binding
      implicit none      
contains

function sha256(str)
    character(len=64) :: sha256
          character(len=*), intent(in) :: str
          sha256 = sha256b(str, 1)
end function sha256

function dirty_sha256(str)
          character(len=64) :: dirty_sha256
          character(len=*), intent(in) :: str
          dirty_sha256 = sha256b(str, 0)
end function dirty_sha256

function sha256b(str, swap)
          character(len=64) :: sha256b
          character(len=*), intent(in) :: str
          integer, intent(in) :: swap
          integer(kind=c_int64_t) :: length
          integer(kind=c_int32_t) :: temp1, temp2, i
          integer :: break, pos0
          integer(kind=c_int32_t) :: h0_ref(8), k0_ref(64)
          integer(kind=c_int32_t) :: h0(8), k0(64), a0(8), w0(64)
          data (h0_ref(i),i=1,8)/&
           & z'6a09e667', z'bb67ae85', z'3c6ef372', z'a54ff53a', z'510e527f', z'9b05688c', z'1f83d9ab', z'5be0cd19'/
          data (k0_ref(i), i=1,64)/&
           & z'428a2f98', z'71374491', z'b5c0fbcf', z'e9b5dba5', z'3956c25b', z'59f111f1', z'923f82a4', z'ab1c5ed5',&
           & z'd807aa98', z'12835b01', z'243185be', z'550c7dc3', z'72be5d74', z'80deb1fe', z'9bdc06a7', z'c19bf174',&
           & z'e49b69c1', z'efbe4786', z'0fc19dc6', z'240ca1cc', z'2de92c6f', z'4a7484aa', z'5cb0a9dc', z'76f988da',&
           & z'983e5152', z'a831c66d', z'b00327c8', z'bf597fc7', z'c6e00bf3', z'd5a79147', z'06ca6351', z'14292967',&
           & z'27b70a85', z'2e1b2138', z'4d2c6dfc', z'53380d13', z'650a7354', z'766a0abb', z'81c2c92e', z'92722c85',&
           & z'a2bfe8a1', z'a81a664b', z'c24b8b70', z'c76c51a3', z'd192e819', z'd6990624', z'f40e3585', z'106aa070',&
           & z'19a4c116', z'1e376c08', z'2748774c', z'34b0bcb5', z'391c0cb3', z'4ed8aa4a', z'5b9cca4f', z'682e6ff3',&
           & z'748f82ee', z'78a5636f', z'84c87814', z'8cc70208', z'90befffa', z'a4506ceb', z'bef9a3f7', z'c67178f2'/
          h0 = h0_ref
          k0 = k0_ref
          break = 0
          pos0 = 1
          length = len(trim(str))
          do while (break .ne. 1)
             call consume_chunk(str, length, w0(1:16), pos0, break, swap)
        do i=17,64
                    w0(i) = ms1(w0(i-2)) + w0(i-16) + ms0(w0(i-15)) + w0(i-7)
             end do
             a0 = h0
             do i=1,64
                    temp1 = a0(8) + cs1(a0(5)) + ch(a0(5),a0(6),a0(7)) + k0(i) + w0(i)
                    temp2 = cs0(a0(1)) + maj(a0(1),a0(2),a0(3))
                    a0(8) = a0(7)
                    a0(7) = a0(6)
                    a0(6) = a0(5)
                    a0(5) = a0(4) + temp1
                    a0(4) = a0(3)
                    a0(3) = a0(2)
                    a0(2) = a0(1)
                    a0(1) = temp1 + temp2
        end do
            h0 = h0 + a0
          end do
          write(sha256b,'(8z8.8)') h0(1), h0(2), h0(3), h0(4), h0(5), h0(6), h0(7), h0(8)
end function sha256b

function swap32(inp)
          integer(kind=c_int32_t) :: swap32
          integer(kind=c_int32_t), intent(in)  :: inp
          call mvbits(inp, 24, 8, swap32,  0)
          call mvbits(inp, 16, 8, swap32,  8)
          call mvbits(inp,  8, 8, swap32, 16)
          call mvbits(inp,  0, 8, swap32, 24)
end function swap32

function swap64(inp)
          integer(kind=c_int64_t) :: swap64
          integer(kind=c_int64_t), intent(in)  :: inp
          call mvbits(inp, 56, 8, swap64,  0)
          call mvbits(inp, 48, 8, swap64,  8)
          call mvbits(inp, 40, 8, swap64, 16)
          call mvbits(inp, 32, 8, swap64, 24)
          call mvbits(inp, 24, 8, swap64, 32)
          call mvbits(inp, 16, 8, swap64, 40)
          call mvbits(inp,  8, 8, swap64, 48)
          call mvbits(inp,  0, 8, swap64, 56)
end function swap64

function swap64a(inp)
          integer(kind=c_int64_t) :: swap64a
          integer(kind=c_int64_t), intent(in)  :: inp
          call mvbits(inp,  0, 8, swap64a, 32)
          call mvbits(inp,  8, 8, swap64a, 40)
          call mvbits(inp, 16, 8, swap64a, 48)
          call mvbits(inp, 24, 8, swap64a, 56)
          call mvbits(inp, 32, 8, swap64a,  0)
          call mvbits(inp, 40, 8, swap64a,  8)
          call mvbits(inp, 48, 8, swap64a, 16)
          call mvbits(inp, 56, 8, swap64a, 24)
end function swap64a

function ch(a, b, c)
          integer(kind=c_int32_t) :: ch
          integer(kind=c_int32_t), intent(in) :: a, b, c
          ch = ieor(iand(a, b), (iand(not(a), c)))
end function ch

function maj(a, b, c)
      integer(kind=c_int32_t) :: maj
      integer(kind=c_int32_t), intent(in) :: a, b, c
      maj = ieor(iand(a, b), ieor(iand(a, c), iand(b, c)))
end function maj

function cs0(a)
          integer(kind=c_int32_t) :: cs0
          integer(kind=c_int32_t), intent(in) :: a
          cs0 = ieor(ishftc(a, -2), ieor(ishftc(a, -13), ishftc(a, -22)))
end function cs0

function cs1(a)
          integer(kind=c_int32_t) :: cs1
          integer(kind=c_int32_t), intent(in) :: a
          cs1 = ieor(ishftc(a, -6), ieor(ishftc(a, -11), ishftc(a, -25)))
end function cs1

function ms0(a)
          integer(kind=c_int32_t) :: ms0
          integer(kind=c_int32_t), intent(in) :: a
          ms0 = ieor(ishftc(a, -7), ieor(ishftc(a, -18), ishft(a, -3)))
end function ms0

function ms1(a)
          integer(kind=c_int32_t) :: ms1
          integer(kind=c_int32_t), intent(in) :: a
          ms1 = ieor(ishftc(a, -17), ieor(ishftc(a, -19), ishft(a, -10)))
end function ms1

subroutine consume_chunk(str, length, inp, pos0, break, swap)
          character(len=*), intent(in) :: str
          integer(kind=c_int64_t), intent(in) :: length
          integer(kind=c_int32_t), intent(inout) :: inp(*)
          integer, intent(inout) :: pos0, break
          integer, intent(in) :: swap
          character(len=4) :: last_word
          integer(kind=c_int64_t) :: rest
          integer(kind=c_int32_t) :: to_pad, leftover, space_left, zero
          integer(kind=c_int8_t)  :: ipad0, ipad1, i
          data zero  / b'00000000000000000000000000000000'/
          data ipad0 / b'00000000' /
          data ipad1 / b'10000000' /
          rest = length - pos0 + 1
          if (rest .ge. 64) then
             inp(1:16) = transfer(str(pos0:pos0+64-1), inp(1:16))
             if (swap .eq. 1) then
                    do i=1,16
                           inp(i) = swap32(inp(i))
                    end do
             end if
             pos0 = pos0 + 64
          else
             space_left = 16
             leftover   = rest/4
             if (leftover .gt. 0) then
                    inp(1:leftover) = transfer(str(pos0:pos0+leftover*4-1), inp(1:16))
                    if (swap .eq. 1) then
                           do i=1,leftover
                              inp(i) = swap32(inp(i))
                           end do
                    end if
                    pos0 = pos0 + leftover*4
                    rest = length - pos0 + 1
                    space_left = space_left - leftover
             end if

             if (space_left .gt. 0) then
                    if (break .ne. 2) then
                           if (rest .gt. 0) then
                              last_word(1:rest) = str(pos0:pos0+rest-1)
                              pos0 = pos0 + rest
                           end if
                           last_word(rest+1:rest+1) = transfer(ipad1, last_word(1:1))
                           to_pad = 4 - rest - 1
                do i=1,to_pad
                              last_word(rest+1+i:rest+1+i) = transfer(ipad0, last_word(1:1))
                           end do
                           inp(17-space_left) = transfer(last_word(1:4), inp(1))
                           if (swap .eq. 1) then
                              inp(17-space_left) = swap32(inp(17-space_left))
                           end if
                           space_left = space_left - 1
                           break = 2
                    end if
                    if (space_left .eq. 1) then
                           inp(16) = zero
                           space_left = 0
                    end if
                    rest = 0
             end if
             if ((rest .eq. 0) .and. (space_left .ge. 2)) then
                    do while (space_left .gt. 2)
                           inp(17-space_left) = zero
                           space_left = space_left - 1
                    end do
                    inp(15:16) = transfer(swap64a(length*8), inp(15:16))
                    break = 1
             end if
      end if
end subroutine consume_chunk

end module sha256_module
module merkleTree
    use sha256_module
    implicit none
    integer :: uid = 1

    type data
        integer :: uid
        character(:), allocatable :: id_origin, address_origin                
        character(:), allocatable :: id_destination, address_destination
        character(:), allocatable :: cost_between, hash_value
        type(data), pointer :: next => null()
    end type data

    type hash_node
        integer :: uid
        character(:), allocatable :: hash        
        type(hash_node), pointer :: left => null()
        type(hash_node), pointer :: right => null()
        type(data), pointer :: dataref => null()
    end type hash_node

    type merkle 
        type(hash_node), pointer :: top_hash => null()
        type(data), pointer :: data_head => null()
        type(data), pointer :: data_tail => null()
        integer :: pos = 0
        contains 
        procedure :: add_data
        procedure :: get_data
        procedure :: data_length
        procedure :: create_tree
        procedure :: gen_hash
        procedure :: generate
        procedure :: merkle_dot 
        procedure :: merkle_dot_rec
    end type merkle

    contains
    subroutine add_data(this, id_origin, address_origin, id_destination, address_destination, cost_between)
        class(merkle), intent(inout) :: this
        integer, intent(in) :: id_origin
        character(*), intent(in) :: address_origin
        integer, intent(in) :: id_destination
        character(*), intent(in) :: address_destination
        integer, intent(in) :: cost_between !es el costo total entre las 2 sucursales

        character(len=256) :: id_o_str !id de la sucursal origen
        character(len=256) :: id_d_str !id de la sucursal destino
        character(len=256) :: cost_str !costo entre las 2 sucursales
        character(:), allocatable :: hash_value !valor del hash
        type(data), pointer :: new_data 

        !los casteamos para pasar los enteros en texto
        write(id_o_str, '(I10)') id_origin
        write(id_d_str, '(I10)') id_destination
        write(cost_str, '(I10)') cost_between

        !obtenemos el valor del hash sumando:
        !id origen, direccion origen, id destino, direccion destino y costo entre las 2 sucursales
        hash_value = sha256(trim(id_o_str) // address_origin // trim(id_d_str) // address_destination // trim(cost_str))

        allocate(new_data)
        allocate(new_data%id_origin, source=id_o_str)
        allocate(new_data%address_origin, source=address_origin)
        allocate(new_data%id_destination, source=id_d_str)
        allocate(new_data%address_destination, source=address_destination)
        allocate(new_data%cost_between, source=cost_str)
        allocate(new_data%hash_value, source=hash_value)
        new_data%uid = uid
        uid = uid + 1

        if ( associated(this%data_head) ) then
            this%data_tail%next => new_data
            this%data_tail => new_data
        else 
            this%data_head => new_data
            this%data_tail => new_data
        end if

    end subroutine add_data

    function get_data(this, pos) result(data_node)
        class(merkle), intent(inout) :: this
        integer, intent(inout) :: pos
        type(data), pointer :: data_node
        data_node => this%data_head       
        do while (associated(data_node))
            if ( pos == 0 ) then
                return
            end if
            pos = pos - 1
            data_node => data_node%next
        end do
    end function get_data

    function data_length(this) result(res)
        class(merkle), intent(inout) :: this
        type(data), pointer :: tmp 
        integer :: res 
        res = 0
        tmp => this%data_head
        do while (associated(tmp))
            res = res + 1
            tmp => tmp%next
        end do        
    end function data_length

    subroutine create_tree(this, node, expo)
        class(merkle), intent(inout) :: this 
        type(hash_node), pointer, intent(inout) :: node
        integer, intent(in) :: expo
        node%uid = uid
        uid = uid + 1

        if ( expo > 0 ) then
            allocate(node%left)
            allocate(node%right)
            call this%create_tree(node%left, expo - 1)
            call this%create_tree(node%right, expo - 1)
        end if    
        
    end subroutine create_tree

    subroutine gen_hash(this,  node, pow)
        class(merkle), intent(inout) :: this
        type(hash_node), pointer, intent(inout) :: node        
        character(:), allocatable :: hash
        integer, intent(in) :: pow
        integer :: tmp 
        
        if ( associated(node) ) then
            call this%gen_hash(node%left, pow)
            call this%gen_hash(node%right, pow)
            if ( .NOT. associated(node%left) .AND. .NOT. associated(node%right) ) then
                tmp = pow - this%pos
                node%dataref => this%get_data(tmp)
                this%pos = this%pos - 1
                hash = node%dataref%hash_value
                node%hash = hash
            else 
                hash = sha256(node%left%hash // node%right%hash)
                node%hash = hash
            end if
        end if
        
    end subroutine gen_hash

    subroutine generate(this)
        class(merkle), intent(inout) :: this
        integer :: expo, i, pow         
        expo = 1

        do while (2 ** expo < this%data_length() )
            expo = expo + 1
        end do

        pow = 2 ** expo
        this%pos = pow 
        i = this%data_length()

        do while (i < pow)
            call this%add_data(-1, "null", -1, "null", -1)
            i = i + 1
        end do
        allocate(this%top_hash)
        call this%create_tree(this%top_hash, expo)
        call this%gen_hash(this%top_hash, pow)
    end subroutine generate
    
    subroutine merkle_dot(this)
        class(merkle), intent(inout) :: this        
        open(69, file='images/merkle.dot', status='replace')
        write(69, '(A)') 'digraph Merkle_tree {'
        write(69, '(A)') 'node [shape=record, fontname=Arial, fontsize=12];'
        call this%merkle_dot_rec(this%top_hash, 69)
        write(69, '(A)') '}'
        close(69)
        ! call execute_command_line('dot -Tsvg outputs/merkle.dot -o outputs/merkle.svg')
        call execute_command_line('dot -Tpng images/merkle.dot -o images/merkle.png')
        
        call system("eog images/merkle.png") !si da error se debe de usar "unset GTK_PATH"
    end subroutine merkle_dot

    subroutine merkle_dot_rec(this,  tmp, unit)
        class(merkle), intent(inout) :: this
        class(hash_node), pointer, intent(in) :: tmp
        integer, intent(in) :: unit        
        if ( .NOT. associated(tmp) ) then
            return
        end if
        write(unit, '(I0, A, A, A)') tmp%uid, ' [label="', tmp%hash, '"];'
        if ( associated(tmp%left) ) then
            write(unit, '(I0, A, I0, A)') tmp%uid, ' -> ', tmp%left%uid, ';'            
        end if
        if ( associated(tmp%right) ) then
            write(unit, '(I0, A, I0, A)') tmp%uid, ' -> ', tmp%right%uid, ';'
        end if
        call this%merkle_dot_rec(tmp%left, unit)
        call this%merkle_dot_rec(tmp%right, unit)
        if ( associated(tmp%dataref) ) then            
            write(unit, '(I0, A)') tmp%dataref%uid, ' [label=<<TABLE><TR>'
            write(unit, '(A, A, A)') '<TD>id_origin: ', trim(tmp%dataref%id_origin), '</TD>'
            write(unit, '(A, A, A)') '<TD>address_origin: ', trim(tmp%dataref%address_origin), '</TD></TR>'
            write(unit, '(A, A, A)') '<TR><TD>id_destination: ', trim(tmp%dataref%id_destination), '</TD>'
            write(unit, '(A, A, A)') '<TD>address_destination: ', trim(tmp%dataref%address_destination), '</TD></TR>'
            write(unit, '(A, A, A)') '<TR><TD>cost_between: ', trim(tmp%dataref%cost_between), '</TD></TR>'
            write(unit, '(A)') '</TABLE>>];'
            write(unit, '(I0, A, I0, A)') tmp%uid, ' -> ', tmp%dataref%uid, ';'
        end if
    end subroutine merkle_dot_rec
end module merkleTree

program merkle_test2
    use merkleTree
    implicit none
    type(merkle) :: tree 

    call tree%add_data(1, "Chimaltenango", 3, "Guatemala", 500)
    call tree%add_data(2, "Quetzaltenango", 3, "Guatemala", 200)
    call tree%add_data(3, "Huehuetenango", 1, "Chimaltenango", 100)
    call tree%add_data(4, "San Marcos", 2, "Quetzaltenango", 50)
    call tree%add_data(5, "Solola", 2, "Quetzaltenango", 150)

    call tree%generate()

    call tree%merkle_dot()

end program merkle_test2