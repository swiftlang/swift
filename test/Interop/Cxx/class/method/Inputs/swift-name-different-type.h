struct A {};

struct B {
  // expected-to-warning@+2 {{swift_name cannot be used to import a non-static C++ method as a member of a different type}}
  // expected-to-note @+1 {{while importing 'renamedFrom0'}}
  void renamedFrom0(A &a) const
    __attribute__((swift_name("A.renamedFrom0(self:)")));
  // expected-from-warning@-2 {{swift_name cannot be used to import a non-static C++ method as a member of a different type}}
  // expected-from-note @-3 {{while importing 'renamedFrom0'}}

  // expected-to-warning@+2 {{swift_name cannot be used to import a non-static C++ method as a member of a different type}}
  // expected-to-note @+1 {{while importing 'renamedFrom1'}}
  void renamedFrom1(A &a, int i) const
    __attribute__((swift_name("A.renamedFrom1(self:j:)")));
  // expected-from-warning@-2 {{swift_name cannot be used to import a non-static C++ method as a member of a different type}}
  // expected-from-note @-3 {{while importing 'renamedFrom1'}}

  void other() const;
};
