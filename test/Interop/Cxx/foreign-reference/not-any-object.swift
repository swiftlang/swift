// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t/Inputs  %t/test2.swift  -enable-experimental-cxx-interop

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

// TODO: make this a better error (https://github.com/swiftlang/swift/issues/90322).
test(Empty.create()) // expected-error {{argument type 'Empty?' expected to be an instance of a class or class-constrained type}}
test([Empty.create()][0]) // expected-error {{failed to produce diagnostic for expression}}

//--- test2.swift

import Test

protocol RequiresAnyObject : AnyObject {}

@available(SwiftStdlib 5.8, *)
extension Empty : RequiresAnyObject {} // expected-error {{type 'Empty' does not conform to protocol 'RequiresAnyObject'}}
// expected-error@-1 {{'RequiresAnyObject' requires that 'Empty' be a class type}}
// expected-note@-2 {{requirement specified as 'Self' : 'AnyObject' [with Self = Empty]}}
