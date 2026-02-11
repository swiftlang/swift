// RUN: %empty-directory(%t)
// RUN: %target-clang -std=c++17 -fno-exceptions %target-threading-opt %target-rtti-opt %target-pic-opt %target-msvc-runtime-opt -DSWIFT_INLINE_NAMESPACE=__runtime -g -c %S/Inputs/RoundTrip/RoundTrip.cpp -I%swift_obj_root/include -I%swift_src_root/include -I%swift_src_root/stdlib/include -I%swift_src_root/stdlib/public/runtime -I %swift_src_root/stdlib/public/SwiftShims/ -I%llvm_src_root/include -I%llvm_obj_root/include -o %t/RoundTrip.o
// RUN: %target-build-swift -g -static -emit-module-path %t/RoundTrip.swiftmodule -emit-module -emit-library -module-name RoundTrip -o %t/%target-static-library-name(RoundTrip) %S/Inputs/RoundTrip/RoundTrip.swift %t/RoundTrip.o
// RUN: %{python} %S/Inputs/build-modules.py %t %S/Inputs/testcases "%target-build-swift" %target-static-library-name(PLACEHOLDER)
// RUN: %target-build-swift -g -I%t -o %t/round-trip %s %t/all-tests.swift -L%t %target-cxx-lib @%t/link.txt %if !OS=windows-msvc %{ -lm %} -lRoundTrip -L%swift-lib-dir/swift/%target-sdk-name/%target-arch -lswiftRemoteInspection
// RUN: %target-codesign %t/round-trip
// RUN: %target-run %t/round-trip | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: remote_mirror
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// CHECK-NOT: FAIL

@main
struct Test {
  static func main() throws {
    try runAllTests()
  }
}
