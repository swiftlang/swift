// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

func f1(_ x: Double) {}
func g1() -> Double { 0 }

func f2(_ x: CGFloat) {}
func g2() -> CGFloat { 0 }

func test() {
  let _: (CGFloat) -> () = f1
  let _: () -> CGFloat = g1

  let _: (CGFloat) -> () = f2
  let _: () -> CGFloat = g2
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSdIegy_12CoreGraphics7CGFloatVIegy_TR : $@convention(thin) (CGFloat, @guaranteed @callee_guaranteed (Double) -> ()) -> () {
// CHECK: bb0(%0 : $CGFloat, %1 : @guaranteed $@callee_guaranteed (Double) -> ()):
// CHECK: [[META:%.*]] = metatype $@thin Double.Type
// CHECK: [[INIT:%.*]] = function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
// CHECK: [[ARG:%.*]] = apply [[INIT]](%0, [[META]]) : $@convention(method) (CGFloat, @thin Double.Type) -> Double
// CHECK: apply %1([[ARG]]) : $@callee_guaranteed (Double) -> ()
// CHECK: return

// CHECK-LABEL: sil [transparent] [serialized] [available {{.*}}] @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSdIegd_12CoreGraphics7CGFloatVIegd_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> Double) -> CGFloat {
// CHECK: bb0(%0 : @guaranteed $@callee_guaranteed () -> Double):
// CHECK: [[RESULT:%.*]] = apply %0() : $@callee_guaranteed () -> Double
// CHECK: [[META:%.*]] = metatype $@thin CGFloat.Type
// CHECK: [[INIT:%.*]] = function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
// CHECK: [[RET:%.*]] = apply [[INIT]]([[RESULT]], [[META]]) : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
// CHECK: return [[RET]]

// CHECK-LABEL: sil [transparent] [serialized] [available {{.*}}] @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
