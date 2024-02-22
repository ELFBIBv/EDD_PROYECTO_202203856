module prueba
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, prueba!"
  end subroutine say_hello
end module prueba
