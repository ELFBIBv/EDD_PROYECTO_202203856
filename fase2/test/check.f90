!probar la matriz
! program main
!   use module_jsonReader
!   use module_layers
!   use module_bst
!   implicit none
!   ! print *, "Hello World"
!   type(jsonReader) :: reader
!   type(matrix) :: matriz
!   reader%filename = "ImagenMario.json"
!   call reader%readJson(matriz)
!   call matriz%print()
! end program main

!probar que los usuarios los agregue bien
program main
  use module_btree
  ! use module_users
  implicit none
  type(BTree) :: bst
  type(ordinal_user),pointer :: user
  character(len=50) :: password
  integer(kind=8) :: i
  logical :: found
  do i = 1,17
    call bst%insert(ordinal_user(name="Juan", DPI=i,password="1234"))
  end do
  call bst%remove(20_8)
  call bst%remove(10_8)
  call bst%remove(5_8)
  call bst%remove(3_8)
  call bst%graph(myNode=bst%returnRoot())
  call bst%traversal(myNode=bst%returnRoot())
  print *, ""
  password = "1234"
  do i = 1,17
    print *, "Buscando usuario con DPI: ", i
    found = .false.
    user => bst%getUser(DPI=i,password=password,myNode=bst%returnRoot(),found=found)
    print *, "salio"
    if (.not. found) then
      print *, "Usuario encontrado: ", user%name
    else
      print *, "Usuario no encontrado"
    end if
  end do
end program main

! program main
!   use module_btree
!   ! use module_users
!   implicit none
!   type(BTree) :: bst
!   type(ordinal_user),pointer :: user
!   character(len=50) :: password
!   integer(kind=8) :: i
!   do i = 1,17
!     call bst%insert(ordinal_user(name="Juan", DPI=i,password="1234"))
!   end do
!   call bst%remove(20_8)
!   ! call bst%insert(ordinal_user(name="pedro", DPI=18_8,password="1234"))
!   call bst%graph(myNode=bst%returnRoot())!este me está quitando un nodo que es el principal
!   call bst%traversal(myNode=bst%returnRoot())
!   print *, ""
!   password = "1234"
!   ! user => bst%getUser(DPI=1_8,password=password,myNode=bst%returnRoot())
!   ! print *, "Usuario encontrado: ", user%DPI
!   do i = 1,17
!     user => bst%getUser(DPI=i,password=password,myNode=bst%returnRoot())
!     print *, "Usuario encontrado: ", user%name
!   end do

  
!   print *, "Usuario encontrado: ", user%name
!   print *, "Usuario encontrado: ", user%DPI
!   print *, "Usuario encontrado: ", user%password
! end program main