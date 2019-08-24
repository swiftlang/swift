// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/linker_pass_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -module-link-name swiftCore
// RUN: %target-swift-frontend %s -I %t -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -I %t -O -emit-sil | %FileCheck %s --check-prefix=OPT

// CHECK: sil [serialized] [noinline] @$ss11doSomethingyyF : $@convention(thin) () -> (){{$}}
// OPT: sil public_external [noinline] @$ss11doSomethingyyF : $@convention(thin) () -> () {
doSomething()

// CHECK: sil @$ss12doSomething2yyF : $@convention(thin) () -> ()
// CHECK-NOT: return
doSomething2()

// CHECK: sil [serialized] [noinline] @$ss16callDoSomething3yyF : $@convention(thin) () -> (){{$}}
// OPT: sil public_external [noinline] @$ss16callDoSomething3yyF : $@convention(thin) () -> () {

// OPT: sil @unknown

// OPT: sil @$ss1AVABycfC

// OPT: sil [noinline] @$ss12doSomething3yyxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (){{$}}

callDoSomething3()
