program main
  use clientQueueModule
  use clientRegisterModule
  use imagesModule
  use waitingListModule
  use windowsModule

  implicit none
  
  type(Clientqueue) :: list_clients
  type(window_linked_list) :: listwindow
  
  !añadiendo clientes
  
  call list_clients%append(uid = 1,name = "alvaro", img_b = 5, img_s = 4)
  call list_clients%append(uid = 2,name = "luis", img_b = 2, img_s = 5)
  
  !añadiendo pasos
  call list_clients%addsteps()
  call list_clients%addsteps()
  
  call list_clients%append(uid = 7,name = "juan", img_b = 6, img_s = 1)
  call list_clients%append(uid = 4,name = "pedro", img_b = 3, img_s = 3)

  call list_clients%addsteps()
  call list_clients%addsteps()
  
  call list_clients%removeClient(2)
  call list_clients%removeClient(2)
  call list_clients%removeClient(2)
  call list_clients%removeClient(2)
  call list_clients%print()
  
  call list_clients%removeClient(2)
end program main

!en la lista de espera es donde voy a contar los pasos necesarios para que una persona se retire
!