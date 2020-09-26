// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/concurrency -emit-module -Xfrontend -enable-experimental-concurrency

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/concurrency -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

func blackHole(_: Any...) {}

public var lookAtMeeee: [(Int) async -> Void] = []

func foo() {
  do {
    let x1 = [(Int) async -> Void]()
    let x2 = [(Int) async throws -> Void]()

    blackHole(x1, x2)
  }
}

// DEMANGLE-TYPE: $sSayySiYcG
// CHECK-TYPE: Array<(Int) async -> ()>

// DEMANGLE-TYPE: $sSayySiYKcG
// CHECK-TYPE: Array<(Int) async throws -> ()>

// DEMANGLE-TYPE: $sIegH_D
// CHECK-TYPE: @async @callee_guaranteed () -> ()
