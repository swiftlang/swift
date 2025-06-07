// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar80847020.m -I %S/Inputs -c -o %t/rdar80847020.o
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -import-objc-header %S/Inputs/rdar80847020.h -Xlinker %t/rdar80847020.o -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: concurrency

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run(_ s: Clazz) async throws {
    let res: (String, String) = try await s.doSomethingMultiResultFlaggy()
    // CHECK: ("hi", "bye")
    print(res)
}

@main struct Main {
  static func main() async throws {
    try await run(Clazz())
  }
}
