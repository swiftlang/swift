// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include)
// RUN: split-file %s %t
//
// RUN: %target-swift-frontend -typecheck -module-name a -cxx-interoperability-mode=default -I %t/include %t/a.swift

//--- include/module.modulemap
module cxx {
  header "header.h"
  export *
}

//--- include/header.h
struct S {
  S() {}
} __attribute__((swift_attr("conforms_to:a.P")));

struct S2 {
  S2() {}
  using A = S2;
} __attribute__((swift_attr("conforms_to:a.P")));

//--- a.swift
import cxx
public protocol P {
  associatedtype A = Int
  func foo(_: A)
}
extension P {
  func foo(_: A) {}
}
func test(s: S) {
  let _ = s.foo(0)
}
func test2(s: S2) {
  let _ = s.foo(s)
}
