// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -I %t/Inputs  %t/test.swift  -enable-cxx-interop

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
#include <new>
#include <stdlib.h>

struct __attribute__((swift_attr("import_as_ref"))) Empty {
  static Empty *create() { return new (malloc(sizeof(Empty))) Empty(); }
};

//--- test.swift

import Test;

public func test(_ _: AnyObject) {}

// TODO: make this a better error.
test(Empty.create()) // expected-error {{type of expression is ambiguous without more context}}
