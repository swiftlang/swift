// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -I %t %t/Main.swift -enable-experimental-feature Embedded -cxx-interoperability-mode=default -c -o %t/a.o -Rmodule-loading

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// BEGIN header.h

struct Base { int field; };
struct Derived : Base {};

// BEGIN module.modulemap

module MyModule {
  header "header.h"
}

// BEGIN Main.swift

import MyModule

public func foo() {
  var d = Derived()
  d.field = 123
}

foo()
