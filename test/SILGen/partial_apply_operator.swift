
// RUN: %target-swift-emit-silgen -module-name partial_apply_protocol -primary-file %s | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name partial_apply_protocol -primary-file %s

protocol Group {
  static func +(_: Self, _: Self) -> Self
  static prefix func -(_: Self) -> Self
}

extension Int : Group {}

func takesBinaryOp(_: (Int, Int) -> Int) {}
func takesUnaryOp(_: (Int) -> Int) {}

func takesGenericBinaryOp<T : Group>(_: (T, T) -> T) {}
func takesGenericUnaryOp<T : Group>(_: (T) -> T) {}

func passOp() {
  takesBinaryOp(+)
  takesUnaryOp(-)
}

func passGenericOp<T : Group>(_: T) {
  takesGenericBinaryOp(+)

  // FIXME: Rejected by the type checker? rdar://problem/60607396
  // takesGenericUnaryOp(-)
}

// CHECK-LABEL: sil private [ossa] @$s22partial_apply_protocol6passOpyyFS2i_Sitcfu_ : $@convention(thin) (Int, Int) -> Int
// CHECK-LABEL: sil private [ossa] @$s22partial_apply_protocol6passOpyyFS2icfu0_ : $@convention(thin) (Int) -> Int

// CHECK-LABEL: sil private [ossa] @$s22partial_apply_protocol13passGenericOpyyxAA5GroupRzlFS2i_Sitcfu_ : $@convention(thin) @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @out τ_0_2 for <Int, Int, Int>
