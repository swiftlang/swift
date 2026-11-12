// rdar://181117220 (https://github.com/swiftlang/swift/issues/90322)
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs %t/test.swift

//--- Inputs/module.modulemap
module Test {
  header "test.h"
}

//--- Inputs/test.h
struct __attribute__((swift_attr("import_reference")))
       __attribute__((swift_attr("retain:immortal")))
       __attribute__((swift_attr("release:immortal"))) Empty {
  int x;
};

struct Empty *EmptyCreate(void) __attribute__((swift_name("Empty.create()")));

//--- test.swift
import Test

func test(_: AnyObject) {}

test([Empty.create()][0]) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
