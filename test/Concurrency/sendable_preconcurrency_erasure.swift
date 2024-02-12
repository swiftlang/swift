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

// Test erase in sugared types
do {
  @preconcurrency func testArray1(_: [any Sendable]) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure10testArray1L2_yySayypGF
  @preconcurrency func testArray2(_: [@Sendable () -> Void]) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure10testArray2L2_yySayyycGF

  @preconcurrency func testDictionary(_: [Int: (any Sendable, (String, @Sendable (any Sendable) -> Void))]) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure14testDictionaryL2_yySDySiyp_SS_yypcttGF

  @preconcurrency func testDesugaredDict(_: Dictionary<Int, (any Sendable, (String, @Sendable (any Sendable) -> Void))>) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure17testDesugaredDictL2_yySDySiyp_SS_yypcttGF

  @preconcurrency func testVariadic(_: (any Sendable)...) {}
  // CHECK-LABEL: sil private [ossa] @$s31sendable_preconcurrency_erasure12testVariadicL2_yyypd_tF
}

public struct Data {
  @preconcurrency var test: (any Sendable, Array<(Int, any Sendable)>)? = nil
  // CHECK-LABEL: sil [transparent] [ossa] @$s31sendable_preconcurrency_erasure4DataV4testyp_SaySi_yptGtSgvpfi
}
