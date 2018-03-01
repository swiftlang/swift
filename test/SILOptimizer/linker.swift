// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -module-link-name swiftCore
// RUN: %target-swift-frontend %s -O -I %t -sil-debug-serialization -o - -emit-sil | %FileCheck %s

// CHECK: sil public_external [serialized] [canonical] @$Ss11doSomethingyyF : $@convention(thin) () -> () {
doSomething()

// CHECK: sil @$Ss12doSomething2yyF : $@convention(thin) () -> ()
// CHECK-NOT: return
doSomething2()

// CHECK: sil public_external [serialized] [noinline] [canonical] @$Ss16callDoSomething3yyF

// CHECK: sil [canonical] @unknown

// CHECK: sil [canonical] @$Ss1AVABycfC

// CHECK: sil [noinline] [canonical] @$Ss12doSomething3yyxlF
// CHECK-NOT: return

callDoSomething3()
