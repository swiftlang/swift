struct A {};

struct B {
  // expected-to-warning@+2 {{swift_name cannot be used to import a non-static C++ method as a member of a different type}}
  // expected-to-note @+1 {{while importing 'renamedFrom0'}}
  void renamedFrom0(A &a) const
    __attribute__((swift_name("A.renamedFrom0(self:)")));

  // expected-to-warning@+2 {{swift_name cannot be used to import a non-static C++ method as a member of a different type}}
  // expected-to-note @+1 {{while importing 'renamedFrom1'}}
  void renamedFrom1(A &a, int i) const
    __attribute__((swift_name("A.renamedFrom1(self:j:)")));

  void other() const;
};
