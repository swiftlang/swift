// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -sil-serialize-all -module-link-name swiftCore
// RUN: %target-swift-frontend %s -O -I %t -sil-debug-serialization -o - -emit-sil | FileCheck %s

// CHECK: sil public_external [fragile] @_TFSs11doSomethingFT_T_ : $@convention(thin) () -> () {
doSomething()

// Make sure we are not linking doSomethign2 because it is marked with 'noimport'

// CHECK: sil [_semantics "stdlib_binary_only"] @_TFSs12doSomething2FT_T_ : $@convention(thin) () -> ()
// CHECK-NOT: return
doSomething2()

// CHECK: sil public_external [fragile] [noinline] @{{.*}}callDoSomething3{{.*}}

// CHECK: sil @unknown

// CHECK: sil [fragile] [noinline] [_semantics "stdlib_binary_only"] @{{.*}}doSomething3{{.*}}
// CHECK-NOT: return

// CHECK: sil {{.*}} @_TFVSs1ACfMS_FT_S_

callDoSomething3()
