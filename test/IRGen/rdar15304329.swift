// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

// CHECK-NOT: @_TWvi{{.*}}
// CHECK: _TWvdCSo3Bar3fooGV12rdar153043293FooSi_
// CHECK-NOT: @_TWvi{{.*}}

import Foundation

struct Foo<T> { var x: T }

class Bar : NSObject { var foo: Foo<Int> }
