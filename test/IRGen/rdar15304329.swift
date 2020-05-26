// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-NOT: @_TWvi{{.*}}
// CHECK: $s12rdar153043293BarC3fooAA3FooVySiGvpWvd
// CHECK-NOT: @_TWvi{{.*}}

import Foundation

struct Foo<T> { var x: T }

class Bar : NSObject { 
  var foo: Foo<Int> 

  init(foo: Foo<Int>) {
    self.foo = foo
    super.init()
  }
}
