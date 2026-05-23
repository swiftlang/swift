// REQUIRES: swift_feature_SuppressedAssociatedTypes

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:     -o %t/basic_suppressed_assoc.swiftmodule \
// RUN:     -emit-module-interface-path %t/basic_suppressed_assoc.swiftinterface \
// RUN:     %S/Inputs/basic_suppressed_assoc.swift

// We expect clients of the library to be broken. While the library was built with the legacy feature enabled,
// the client isn't building with that feature, and the compiler now has SE-503 enabled by default, which will infer
// includes defaults in places it did not before.
// RUN: %target-swift-frontend -verify -emit-sil -I %t %s -o %t/final.sil -verify-additional-prefix new-

// Workaround for clients: enable the legacy experimental version of the feature
// RUN: %target-swift-frontend -verify -emit-sil -I %t %s -o %t/final.sil -enable-experimental-feature SuppressedAssociatedTypes

import basic_suppressed_assoc

struct NC: ~Copyable {}

struct MyType: P {
  typealias Primary = NC
  typealias Secondary = NC
}

func breakage() {
  let mt = MyType()
  mt.worksForAll() // expected-new-error {{referencing instance method 'worksForAll()' on 'P' requires that 'MyType.Primary' (aka 'NC') conform to 'Copyable'}}
  testForAll(mt) // expected-new-error {{global function 'testForAll' requires that 'MyType.Primary' (aka 'NC') conform to 'Copyable'}}
}

extension P {  // expected-new-note {{where 'Self.Primary' = 'MyType.Primary' (aka 'NC')}}
  func worksForAll() {}
}

func testForAll<T: P>(_ t: T) { // expected-new-note {{where 'T.Primary' = 'MyType.Primary' (aka 'NC')}}
  t.worksForAll()
}
