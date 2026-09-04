// RUN: %target-run-simple-swift(-Xfrontend  -disable-availability-checking) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64

struct A<let N: Int> {
  func foo() {
    print(N)
  }
}

// CHECK: 2147483649
A<2_147_483_649>().foo()

// CHECK: -2147483649
A< -2_147_483_649>().foo()

// CHECK: 4294967296
A<4_294_967_296>().foo()

// CHECK: 9223372036854775807
A<9_223_372_036_854_775_807>().foo()
