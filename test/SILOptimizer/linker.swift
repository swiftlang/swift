// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -module-link-name swiftCore
// RUN: %target-swift-frontend %s -O -I %t -sil-debug-serialization -o - -emit-sil | %FileCheck %s

// CHECK: sil public_external [serialized] @_T0s11doSomethingyyF : $@convention(thin) () -> () {
doSomething()

// CHECK: sil @_T0s12doSomething2yyF : $@convention(thin) () -> ()
// CHECK-NOT: return
doSomething2()

// CHECK: sil public_external [serialized] [noinline] @_T0s16callDoSomething3yyF

// CHECK: sil @unknown

// CHECK: sil @_T0s1AVABycfC

// CHECK: sil [noinline] @_T0s12doSomething3yxlF
// CHECK-NOT: return

callDoSomething3()
