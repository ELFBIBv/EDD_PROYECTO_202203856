module module_blockchain
    implicit none
    
    type block
        integer :: index
        character(len=25) :: timestamp
        character(len=50) :: data
        integer :: nonce
        character(len=32) :: previous_hash
        character(len=256) :: rootmerklel
        character(len=32) :: hash
        type(block), pointer :: next => null()
    end type block

    type blockchain
        type(block), pointer :: head
        type(block), pointer :: tail
        integer :: index = 0
        contains
        procedure :: add_block
        procedure :: print_blockchain
        procedure :: graph_blockchain
    end type blockchain

    contains

    subroutine add_block(this, timestamp, data, nonce, rootmerklel, hash)
        class(blockchain), intent(inout) :: this
        character(len=25), intent(in) :: timestamp
        character(len=50), intent(in) :: data
        integer, intent(in) :: nonce
        character(len=256), intent(in) :: rootmerklel
        character(len=32), intent(in) :: hash
        character(len=32):: previous_hash

        type(block), target :: new_block
        type(block), pointer :: current_block

        new_block%index = this%index
        this%index = this%index + 1

        new_block%timestamp = timestamp
        new_block%data = data
        new_block%nonce = nonce
        new_block%previous_hash = "0000"
        new_block%rootmerklel = rootmerklel
        new_block%hash = hash

        if(associated(this%head)) then
            current_block => this%head
            previous_hash = current_block%hash
            do while(associated(current_block%next))
                previous_hash = current_block%hash
                current_block => current_block%next
            end do
            new_block%previous_hash = previous_hash
            current_block%next => new_block
            this%tail => new_block
        else
            this%head => new_block
            this%tail => new_block
        end if
    end subroutine add_block

    subroutine print_blockchain(this)
        class(blockchain), intent(in) :: this
        type(block), pointer :: current_block

        current_block => this%head
        do while(associated(current_block))
            print *, 'Index: ', current_block%index
            print *, 'Timestamp: ', current_block%timestamp
            print *, 'Data: ', current_block%data
            print *, 'Nonce: ', current_block%nonce
            print *, 'Previous Hash: ', current_block%previous_hash
            print *, 'Root Merkle: ', current_block%rootmerklel
            print *, 'Hash: ', current_block%hash
            print *, "-----------------------------"
            current_block => current_block%next
        end do
    end subroutine print_blockchain

    subroutine graph_blockchain(this)
        class(blockchain), intent(in) :: this
        type(block), pointer :: current_block
        integer :: file,previous_index
        open(newunit=file, file='blockchain.dot', status='replace')
        write(file, '(a)') 'digraph blockchain {'
        write(file, '(a)') 'rankdir=LR;'
        write(file, '(a)') 'node [fontname = "Helvetica,Arial,sans-serif" shape = record]'
        current_block => this%head
        write(file, '(A,I5,A)',advance="no") 'node',current_block%index,'[label="'
        write(file, '(A,I5)',advance="no") '<f0>index:',current_block%index
        write(file, '(A,I5)',advance="no") '|<f1>timestamp:',current_block%timestamp
        write(file, '(A,I5)',advance="no") '|<f2>data:',current_block%data
        write(file, '(A,I5)',advance="no") '|<f3>nonce:',current_block%nonce
        write(file, '(A)',advance="no") '|<f4>previous_hash'//current_block%previous_hash
        write(file, '(A,I5)',advance="no") '|<f5>rootmerklel:',current_block%rootmerklel
        write(file, '(A,I5)',advance="no") '|<f6>hash:',current_block%hash
        write(file, '(A)') '"];'
        
        do while(associated(current_block%next))
            previous_index = current_block%index
            current_block => current_block%next
            write(file, '(A,I5,A)',advance="no") 'node',current_block%index,'[label="'
            write(file, '(A,I5)',advance="no") '<f0>index:',current_block%index
            write(file, '(A,I5)',advance="no") '|<f1>timestamp:',current_block%timestamp
            write(file, '(A,I5)',advance="no") '|<f2>data:',current_block%data
            write(file, '(A,I5)',advance="no") '|<f3>nonce:',current_block%nonce
            write(file, '(A)',advance="no") '|<f4>previous_hash'//current_block%previous_hash
            write(file, '(A,I5)',advance="no") '|<f5>rootmerklel:',current_block%rootmerklel
            write(file, '(A,I5)',advance="no") '|<f6>hash:',current_block%hash
            write(file, '(A)') '"];'
            write(file, '(A,I5,A,I5,A)',advance="no") 'node',previous_index,':f6-> node',current_block%index,':f4;'
        end do
        write(file, '(a)') '}'
        close(file)

        call execute_command_line('dot -Tpng images/blockchain.dot -o images/blockchain.png')
        ! windows
        ! call system("start images\blockchain.png")
        !linux
        call system("eog images/blockchain.png") !si da error se debe de usar "unset GTK_PATH"
    end subroutine graph_blockchain
            


end module module_blockchain