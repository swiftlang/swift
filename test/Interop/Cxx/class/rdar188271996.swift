// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-silgen %t/test.swift -I %t/Inputs \
// RUN:   -cxx-interoperability-mode=default -o /dev/null

//--- Inputs/module.modulemap
module M {
  header "m.h"
  requires cplusplus
}

//--- Inputs/m.h
struct Base {
  int bits : 4;
  Base() : bits(1) {}
};
struct Derived : Base {};

//--- test.swift
import M

func test() {
  var d = Derived()
  d.bits = 3
}
