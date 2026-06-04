// exGC01.c -- a C example program illustrating dangling pointer.
// We force a situation with a dangling pointer.
// We only make use of C constructs.
// The gcc compiler correctly gives warning of returning a pointer on the stack.

#include <stdio.h>

int *f() {
  int i;  
  i = 42;

  return &i;
}

void g() {
  int i;

  i = 9999;
}

int main() {
  int *ip;
  
  ip = f(); // Now, ip is a dangling pointer.
  printf ("\n %d", *ip);

  g();
  printf ("\n %d", *ip); // Now, content of ip has been updated to content of i in g.
  printf ("\n");
}

