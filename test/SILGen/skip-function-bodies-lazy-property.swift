// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -experimental-skip-non-inlinable-function-bodies | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -experimental-skip-non-inlinable-function-bodies-without-types | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -experimental-skip-all-function-bodies | %FileCheck %s --check-prefix=CHECK-SKIP-ALL

public struct S {
  public lazy var x: Int = generateNumber()

  // CHECK-SKIP-ALL-NOT: s4main1SV1xSivg
  // CHECK: sil [lazy_getter]{{.*}} @$s4main1SV1xSivg : $@convention(method) (@inout S) -> Int {
  // CHECK:   function_ref @$s4main1SV14generateNumberSiyF
  // CHECK: } // end sil function '$s4main1SV1xSivg'

  func generateNumber() -> Int { return 1 }
}
