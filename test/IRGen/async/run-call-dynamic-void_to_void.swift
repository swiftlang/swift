// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -g %s -emit-ir -parse-as-library -module-name main | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -module-name main -o %t/main %target-rpath(%t) 
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// LLVM does not support swifttailcc for WebAssembly target for now
// See https://github.com/apple/swift/issues/69333
// UNSUPPORTED: CPU=wasm32

import _Concurrency


// CHECK: running
// CHECK-LL: @"$s4main3runyyY{{.*}}FTu" = hidden global %swift.async_func_pointer

// CHECK-LL: define {{.*}}hidden swift{{(tail)?}}cc void @"$s4main3runyyY{{.*}}F"(ptr swiftasync {{%[0-9]+}}) {{#[0-9]*}}
// CHECK-LL: musttail call swifttailcc void
dynamic func run() async {
  print("running")
}

@main struct Main {
  static func main() async {
    await run()
  }
}
