// Regression test for rdar://164588082

// RUN: %empty-directory(%t)
// RUN: split-file %s %t 

// RUN: %target-swift-frontend -verify -verify-additional-file %t%{fs-sep}bar.h -typecheck %t/test.swift -I %t

//--- test.swift
private import Foo.Bar
public import Foo

public func test(x: Baz) {}

//--- foo.h
#pragma once
#include "bar.h"

//--- bar.h
#pragma once
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
