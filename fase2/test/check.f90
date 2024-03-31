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
! program main
!   use module_btree
!   ! use module_users
!   implicit none
!   type(BTree) :: bst
!   type(ordinal_user),pointer :: user
!   character(len=50) :: password
!   integer(kind=8) :: i
!   logical :: found
!   ! do i = 1,17
!     call bst%insertImage(ordinal_user(name="Juan", DPI=15,password="1234"))
!   ! end do
!   call bst%remove(20_8)
!   call bst%remove(10_8)
!   call bst%remove(5_8)
!   call bst%remove(3_8)
!   call bst%graph(myNode=bst%returnRoot())
!   print *, ""
!   password = "1234"
!   do i = 1,17
!     print *, "Buscando usuario con DPI: ", i
!     user => bst%getUser(DPI=i,password=password,myNode=bst%returnRoot(),found=found)
!     if (found) then
!       print *, "Usuario encontrado: ", user%name
!     else
!       print *, "Usuario no encontrado"
!     end if
!   end do
! end program main

! !para probar los arboles abb
! program main
!   use module_layer
!   use module_abbtree_layers
!   use module_jsonReader_layers
!   implicit none
!   type(abbtree_layers) :: abbtree
!   type(jsonReader_layers) :: reader
!   call reader%readJson("ImagenMario.json", abbtree)
!   call abbtree%deleteLayer(5)
!   call abbtree%graphABBTree("prueba")

! end program main

program main
  character(:), allocatable :: recorrido
  recorrido = "preorden"
  recorrido = trim(recorrido) // "inorden"
  recorrido = trim(recorrido) // "postorden"
  print *, recorrido
end program main