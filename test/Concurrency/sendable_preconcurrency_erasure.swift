// RUN: %target-swift-emit-silgen %s -verify -swift-version 5 | %FileCheck %s

// REQUIRES: concurrency

// Test erasure in generic arguments
do {
  struct S<T> {
  }

  @preconcurrency func test(_: S<any Sendable>) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure4testL_yyAA1SL_VyypGF
  @preconcurrency func test(_: S<Array<@Sendable () -> Void>>) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure4testL0_yyAA1SL_VySayyycGGF
}
