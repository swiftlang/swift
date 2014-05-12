// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

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
