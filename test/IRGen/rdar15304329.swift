// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -new-mangling-for-tests %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-NOT: @_TWvi{{.*}}
// CHECK: _T012rdar153043293BarC3fooAA3FooVySiGvWvd
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
