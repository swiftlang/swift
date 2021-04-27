// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency -g %s -emit-ir -parse-as-library -module-name main | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency -g %s -parse-as-library -module-name main -o %t/main %target-rpath(%t) 
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Windows does not do swiftailcc
// XFAIL: OS=windows-msvc

import _Concurrency


// CHECK: running
// CHECK-LL: @"$s4main3runyyY{{.*}}FTu" = hidden global %swift.async_func_pointer

// CHECK-LL: define {{.*}}hidden swift{{(tail)?}}cc void @"$s4main3runyyY{{.*}}F"(%swift.context* swiftasync {{%[0-9]+}}) {{#[0-9]*}}
// CHECK-LL: musttail call swifttailcc void
dynamic func run() async {
  print("running")
}

@main struct Main {
  static func main() async {
    await run()
  }
}
