// RUN: %target-swift-emit-silgen %s -verify -swift-version 5 | %FileCheck %s

// REQUIRES: concurrency

// Check that isolation is erased from `@preconcurrency` declarations.

@preconcurrency public let nestedTypes: () -> @isolated(any) () -> Void = { { } }
// CHECK-LABEL: sil_global [let] @$s32preconcurrency_isolation_erasure11nestedTypesyycycvp

@preconcurrency func testGlobalActorErasure(_: @MainActor () -> Void) {}
// CHECK-LABEL: sil hidden [ossa] @$s32preconcurrency_isolation_erasure22testGlobalActorErasureyyyyXEF

@preconcurrency func testIsolatedAnyErasure(_: @isolated(any) () -> Void) {}
// CHECK-LABEL: sil hidden [ossa] @$s32preconcurrency_isolation_erasure22testIsolatedAnyErasureyyyyXEF

struct Data<T> {
}

struct S {
  @preconcurrency
  init(_: [Data<nonisolated(nonsending) @Sendable () async -> Void>]? = nil) {}
  // CHECK-LABEL: sil hidden [ossa] @$s32preconcurrency_isolation_erasure1SVyACSayAA4DataVyyyYacGGSgcfC
}
