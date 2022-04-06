// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -emit-library -emit-module-path %t/LimitedAvailOpaque.swiftmodule -module-name LimitedAvailOpaque %S/Inputs/opaque_with_limited_availability.swift -o %t/%target-library-name(LimitedAvailOpaque)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -lLimitedAvailOpaque -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
// REQUIRES: executable_test

import LimitedAvailOpaque

struct S: P {
  func hello() { print("Hello from S") }
}

@available(macOS 100.0.1, *)
struct NewS: P {
  func hello() { print("Hello from NewS") }
}

public struct Test {
  @Example
  var body: some P {
    // TODO(diagnostics): This is incorrect warning due to `some P` return of `buildWithLimitedAvailability`
    // expected-warning@+1 {{result builder 'Example' does not implement 'buildLimitedAvailability'; this code may crash on earlier versions of the OS}}
    if #available(macOS 100.0.1, *) {
      NewS()
    }

    S()
  }

  func sayHello() {
    body.hello()
  }
}

let result = LimitedAvailOpaque.test()
result.hello()
// CHECK: Hello from Empty

Test().sayHello()
// CHECK: Hello from Empty
// CHECK: Hello from Tuple

let conditionalR = LimitedAvailOpaque.test_return_from_conditional()
conditionalR.hello()
// CHECK: Hello from Named
