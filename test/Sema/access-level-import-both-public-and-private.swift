// Regression test for rdar://164588082

// RUN: %empty-directory(%t)
// RUN: split-file %s %t 

// RUN: %target-swift-frontend -verify -verify-additional-file %t%{fs-sep}bar.h -typecheck %t/test.swift -I %t

//--- test.swift
// expected-note@+1{{struct 'Baz' imported as 'private' from 'Bar' here}}
private import Foo.Bar
public import Foo

// expected-error@+2{{function cannot be declared public because its parameter uses a private type}}
// expected-note@+1{{struct 'Baz' is imported by this file as 'private' from 'Bar'}}
public func test(x: Baz) {}

//--- foo.h
#pragma once
#include "bar.h"

//--- bar.h
#pragma once
// expected-note@+1{{type declared here}}
struct Baz {};

//--- module.modulemap
module Foo {
  header "foo.h"
  export *

  explicit module Bar {
    header "bar.h"
    export *
  }
}
