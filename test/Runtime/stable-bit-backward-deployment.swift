// RUN: %empty-directory(%t)
// -- Deployment target is set to pre-10.14.4 so that we use the "old"
//    Swift runtime bit in compiler-emitted classes
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.9 %s -module-name main -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx

import Foundation

// A fixed-layout class should be considered Swift metadata by the OS runtime.
class FixedLayout { }

debugPrint(FixedLayout.self) // CHECK: main.FixedLayout

// A generic class
class GenericBase<T> { }
debugPrint(GenericBase<Int>.self) // CHECK-NEXT: main.GenericBase<Swift.Int>

// A singleton-initialized class
class SingletonInit: GenericBase<Int> { }
debugPrint(SingletonInit.self) // CHECK-NEXT: main.SingletonInit

// A resilient-heritage class
class ResilientSubInit: JSONEncoder {}
debugPrint(ResilientSubInit.self) // CHECK-NEXT: main.ResilientSubInit

print("nailed it") // CHECK-NEXT: nailed it
