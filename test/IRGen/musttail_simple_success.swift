// RUN: %target-swift-frontend -primary-file %s -parse-stdlib -module-name Swift -emit-ir | %FileCheck %s

class Klass {}

// CHECK-LABEL: define hidden swiftcc %Ts5KlassC* @"$ss3fooys5KlassCACF"(
// CHECK: musttail call swiftcc %Ts5KlassC* @"$ss3fooys5KlassCACF14mustTailReturnL_yA2CF"(

func foo(_ x: Klass) -> Klass {
  @_semantics("optimize.sil.tail_always")
  func mustTailReturn(_ x: Klass) -> Klass {
    return x
  }
  
  return mustTailReturn(x)
}


foo(Klass())