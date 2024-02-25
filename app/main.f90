program main
  use Client_queue
  implicit none
  
  type(linked_list_clients) :: list_clients

  !añadiendo clientes
  
  call list_clients%append(uid = 1,name = "alvaro", img_b = 5, img_s = 4)
  call list_clients%append(uid = 2,name = "luis", img_b = 2, img_s = 5)
  call list_clients%append(uid = 7,name = "juan", img_b = 6, img_s = 1)
  call list_clients%append(uid = 4,name = "pedro", img_b = 3, img_s = 3)
  

  !añadiendo pasos
  call list_clients%addsteps("alvaro")
  call list_clients%addsteps("luis")
  call list_clients%addsteps("alvaro")
  call list_clients%addsteps("alvaro")
  
  call list_clients%removeClient(2)
  call list_clients%print()
end program main