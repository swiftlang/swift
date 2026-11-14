// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/concurrency -emit-module -Xfrontend -enable-round-trip-debug-types

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/concurrency -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/concurrency -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | %FileCheck %s --check-prefix=CHECK-RECONSTRUCT -implicit-check-not=FAILURE

// RUN: %target-swift-ide-test -print-local-types -I %t -module-to-print concurrency -source-filename %s | %FileCheck %s --check-prefix=CHECK-LOCAL

// REQUIRES: concurrency

func blackHole(_: Any...) {}

public var lookAtMeeee: [(Int) async -> Void] = []

func foo() {
  do {
    let x1 = [(Int) async -> Void]()
    let x2 = [(Int) async throws -> Void]()

    blackHole(x1, x2)
  }
}

// DEMANGLE-TYPE: $sSayySiYacG
// CHECK-TYPE: Array<(Int) async -> ()>

// DEMANGLE-TYPE: $sSayySiYaKcG
// CHECK-TYPE: Array<(Int) async throws -> ()>

// DEMANGLE-TYPE: $sIegH_D
// CHECK-TYPE: @async @callee_guaranteed () -> ()

// The mangling of a local type mentions the signature of the enclosing
// function. Debug info mangles types from _Concurrency using the module's ABI
// name, Swift, but USRs use _Concurrency, so both manglings must find the
// local type.
@available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *)
func takesClock(_: ContinuousClock) {
  struct Outer {
    struct Inner {}
  }
  typealias Alias = Int

  let inner = Outer.Inner()
  let alias: Alias = 0
  blackHole(inner, alias)
}

// DEMANGLE-TYPE: $s11concurrency10takesClockyys010ContinuousC0VF5OuterL_V5InnerVD
// CHECK-TYPE: Outer.Inner

// DEMANGLE-TYPE: $s11concurrency10takesClockyys010ContinuousC0VF5AliasL_aD
// CHECK-TYPE: Alias

// DEMANGLE-DECL: $s11concurrency10takesClockyys010ContinuousC0VF5OuterL_V5InnerV
// CHECK-DECL: concurrency.(file).takesClock(_:).Outer.Inner

// DEMANGLE-DECL: $s11concurrency10takesClockyy12_Concurrency010ContinuousC0VF5OuterL_V5InnerV
// CHECK-DECL: concurrency.(file).takesClock(_:).Outer.Inner

// CHECK-RECONSTRUCT: decl: struct Inner{{.*}}usr=s:{{.*}}12_Concurrency{{.*}}5InnerV
// CHECK-RECONSTRUCT: decl: typealias Alias = Int{{.*}}usr=s:{{.*}}12_Concurrency{{.*}}5AliasL_a
// CHECK-RECONSTRUCT: type: Outer.Inner{{.*}}mangled=${{.*}}s010Continuous{{.*}}5InnerVD
// CHECK-RECONSTRUCT: type: Alias{{.*}}mangled=${{.*}}s010Continuous{{.*}}5AliasL_aD

// Local types are listed once, even though they are recorded under both
// manglings.
// CHECK-LOCAL: 5OuterL_V
// CHECK-LOCAL-NOT: 5OuterL_V
