// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -verify-ignore-unrelated -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct __attribute__((swift_attr("import_reference")))
       __attribute__((swift_attr("retain:immortal")))
       __attribute__((swift_attr("release:immortal"))) Empty {
  static Empty *create() { return new Empty(); }
};

//--- test.swift

import Test;

public func test(_ _: AnyObject) {}

// TODO: make this a better error.
test(Empty.create()) // expected-error {{failed to produce diagnostic for expression}}
test([Empty.create()][0]) // expected-error * {{no exact matches in call to subscript}}
// expected-error@-1 * {{argument type 'Any' expected to be an instance of a class or class-constrained type}}
// expected-error@-2 * {{argument type 'ArraySlice<Empty?>' expected to be an instance of a class or class-constrained type}}

// FIXME: We emit different diagnostics here on macOS than Linux or Windows.
