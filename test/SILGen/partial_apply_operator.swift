
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

// CHECK-LABEL: sil private [ossa] @$s22partial_apply_protocol6passOpyyFS2i_SitcSimcfu_S2i_Sitcfu0_ : $@convention(thin) (Int, Int, @thin Int.Type) -> Int
// CHECK-LABEL: sil private [ossa] @$s22partial_apply_protocol6passOpyyFS2icSimcfu1_S2icfu2_ : $@convention(thin) (Int, @thin Int.Type) -> Int

// CHECK-LABEL: sil private [ossa] @$s22partial_apply_protocol13passGenericOpyyxAA5GroupRzlFS2i_SitcSimcfu_S2i_Sitcfu0_ : $@convention(thin) (Int, Int, @thin Int.Type) -> Int