// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -sil-serialize-all -module-link-name swiftCore
// RUN: %target-swift-frontend %s -O -I %t -sil-debug-serialization -o - -emit-sil | %FileCheck %s

// CHECK: sil public_external [serialized] @_T0s11doSomethingyyF : $@convention(thin) () -> () {
doSomething()

// Make sure we are not linking doSomething2 because it is marked with 'noimport'

// CHECK: sil [_semantics "stdlib_binary_only"] @_T0s12doSomething2yyF : $@convention(thin) () -> ()
// CHECK-NOT: return
doSomething2()

// CHECK: sil public_external [serialized] [noinline] @{{.*}}callDoSomething3{{.*}}

// CHECK: sil @unknown

// CHECK: sil [serialized] [noinline] [_semantics "stdlib_binary_only"] @{{.*}}doSomething3{{.*}}
// CHECK-NOT: return

// CHECK: sil {{.*}} @_T0s1AV{{[_0-9a-zA-Z]*}}fC

callDoSomething3()
