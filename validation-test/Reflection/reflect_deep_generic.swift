// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_deep_generic
// RUN: %target-codesign %t/reflect_deep_generic

// RUN: %target-run %target-swift-reflection-test %t/reflect_deep_generic | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

protocol Nester {
  func nest() -> Nester
}

class C<T>: Nester {
  func nest() -> Nester {
    return C<Self>()
  }
}

var nester: Nester = C<Int>()
for _ in 0..<10000 {
  nester = nester.nest()
}

// This will fail due to the excessively nested type, but we're making sure it
// fails gracefully and doesn't crash.
reflect(object: nester as AnyObject)

doneReflecting()

// CHECK: Done.
