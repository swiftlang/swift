// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Stages)) %S/Inputs/async_typed_throws_generic_error_slot_lib.swift -emit-module -emit-module-path %t/Stages.swiftmodule -module-name Stages -target %target-swift-5.1-abi-triple
// RUN: %target-build-swift -parse-as-library %s -lStages -I %t -L %t -o %t/main %target-rpath(%t) -target %target-swift-5.1-abi-triple
// RUN: %target-codesign %t/main %t/%target-library-name(Stages)
// RUN: %target-run %t/main %t/%target-library-name(Stages) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: CPU=wasm32

// rdar://187088035
// The caller passed the callee an error slot for the unsubstituted error type
// but read the error back from a separate, uninitialized slot for the
// substituted error type.

import Stages

@main
struct Main {
  static func main() async {
    do {
      try await MyStage<Int>().run()
      print("no error")
    } catch {
      // CHECK: could not run a b c
      print(error.reason, error.a, error.b, error.c)
    }
  }
}
