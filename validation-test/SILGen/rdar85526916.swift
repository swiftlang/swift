// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar85526916.m -I %S/Inputs -c -o %t/rdar85526916.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar85526916.h -Xlinker %t/rdar85526916.o -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run(on object: PFXObject) async throws {
  // CHECK: howdy
  print(await object.performGetStringIdentity()("howdy"))
  // CHECK: mundo
  print(await object.performGetStringAppend()("mun", "do"))
  // CHECK: -9035768
  print(await object.performGetIntegerIdentity()(-9035768))
  // CHECK: 57
  print(await object.performGetIntegerSubtract()(60, 3))
  // CHECK: 9035768
  print(await object.performGetUIntegerIdentity()(9035768))
  // CHECK: 3
  print(await object.performGetUIntegerAdd()(1+1, 1))
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run(on: object)
  }
}

