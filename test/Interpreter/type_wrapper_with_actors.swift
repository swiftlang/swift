// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -enable-experimental-feature TypeWrappers -parse-as-library -emit-library -emit-module-path %t/type_wrapper_defs.swiftmodule -module-name type_wrapper_defs %S/Inputs/type_wrapper_defs.swift -o %t/%target-library-name(type_wrapper_defs)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -ltype_wrapper_defs -module-name main -I %t -L %t %s -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(type_wrapper_defs) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: asserts
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: OS=macosx

// This requires executable tests to be run on the same machine as the compiler,
// as it links with a dylib that it doesn't arrange to get uploaded to remote executors.
// (rdar://99051588)
// UNSUPPORTED: remote_run || device_run

import type_wrapper_defs

@Wrapper
public actor Actor {
  public var name: String
  @PropWrapper public var age: Int? = nil

  public func setAge(_ newAge: Int) async {
    age = newAge
  }
}

let a = Actor(name: "Arhtur Dent")
await print(a.name)
// CHECK: in getter
// CHECK-NEXT: Arhtur Dent
await print(a.age)
// CHECK: in getter
// CHECK-NEXT: nil

await a.setAge(30)
// CHECK: in getter
// CHECK-NEXT: in setter => PropWrapper<Optional<Int>>(value: Optional(30))

await print(a.age)
// CHECK: in getter
// CHECK-NEXT: 30
