struct A {};

struct B {
  void b(A &a, int e) const // expected-warning {{swift_name cannot be used to import a non-static C++ method as a member of a different type}} expected-note {{while importing 'b'}}
    __attribute__((swift_name("A.c(self:d:)")));
  void other() const;
};
