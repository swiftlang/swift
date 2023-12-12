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
  @preconcurrency func test(_: Array<(any Sendable, @Sendable () -> Void)>) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure4testL1_yySayyp_yyctGF
}

public struct Data {
  @preconcurrency var test: (any Sendable, Array<(Int, any Sendable)>)? = nil
  // CHECK-LABEL: sil [transparent] [ossa] @$s31sendable_preconcurrency_erasure4DataV4testyp_SaySi_yptGtSgvpfi
}
