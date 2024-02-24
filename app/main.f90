program main
  use Client_queue
  implicit none

  type(linked_list_clients) :: list_clients
  call list_clients%append("alvaro", 5, 4)
  call list_clients%append("juan", 8, 2)
  call list_clients%append("pedro", 3, 1)
  call list_clients%append("luis", 7, 3)
  call list_clients%append("maria", 2, 5)
  

  call list_clients%print()
end program main
