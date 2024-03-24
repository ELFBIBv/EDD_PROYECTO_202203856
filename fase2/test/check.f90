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

program main
  use module_btree
  ! use module_users
  implicit none
  type(BTree) :: bst
  ! call bst%insert(ordinal_user(name="Juan", DPI=1234567890123_8,password="1234"))
  ! call bst%insert(ordinal_user(name="Pedro", DPI=1234567890124_8,password="1234"))
  ! call bst%insert(ordinal_user(name="Maria", DPI=1234567890125_8,password="1234"))
  ! call bst%insert(ordinal_user(name="Jose", DPI=1234567890126_8,password="1234"))
  ! call bst%insert(ordinal_user(name="Carlos", DPI=1234567890127_8,password="1234"))
  call bst%insert(ordinal_user(name="Juan", DPI=1,password="1234"))
  call bst%insert(ordinal_user(name="Pedro", DPI=2,password="1234"))
  call bst%insert(ordinal_user(name="Maria", DPI=3,password="1234"))
  call bst%insert(ordinal_user(name="Jose", DPI=4,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=5,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=6,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=7,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=8,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=9,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=10,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=11,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=12,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=13,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=14,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=15,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=16,password="1234"))
  call bst%insert(ordinal_user(name="Carlos", DPI=17,password="1234"))
  call bst%traversal(myNode=bst%returnRoot())
  call bst%remove(17_8)
  call bst%traversal(myNode=bst%returnRoot())

end program main

