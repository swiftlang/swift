// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -sdk %S/Inputs -I %t %s -emit-ir | FileCheck %s

// CHECK-NOT: @_TWvi{{.*}}
// CHECK: _TWvdvC12rdar153043293Bar3fooGVS_3FooSi_
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
