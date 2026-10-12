// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sil %t/test.swift -I %t/Inputs \
// RUN:   -cxx-interoperability-mode=default -o /dev/null

//--- Inputs/module.modulemap
module M {
  header "m.h"
  requires cplusplus
}

//--- Inputs/m.h
enum E { e0 };

struct Base1 {
  float m(E p0, E &p1) { return 1; }
  char c = 'b';
};

struct Derived1 : Base1 {
  Derived1(long long) {}
};

struct Base2 {
  void m(signed char &p) {}
  char c = 0;
};
struct Derived2 : Base2 {};

//--- test.swift
import M

func test() {
  var e = E(rawValue: 0)

  var d1 = Derived1(1)
  _ = d1.m(E(rawValue: 0), &e)

  var d2 = Derived2()
  var x: Int8 = 1
  d2.m(&x)
}
