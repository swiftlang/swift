// RUN: %target-build-swift -O %s

// SR-12548: SIL verification error regarding
// `CapturePropagation::rewritePartialApply` for `partial_apply` with
// `@convention(method)` callee.

import _Differentiation

protocol Protocol: Differentiable {
  @differentiable
  func method() -> Self
}

extension Protocol {
  @differentiable
  func method() -> Self { self }
}

struct Struct: Protocol {}

let _: @differentiable (Struct) -> Struct = { $0.method() }

// SIL verification failed: operand of thin_to_thick_function must be thin: opFTy->getRepresentation() == SILFunctionType::Representation::Thin
// Verifying instruction:
//      // function_ref specialized Protocol.method()
//   %5 = function_ref @$s7crasher8ProtocolPAAE6methodxyFAA6StructV_TG5 : $@convention(method) (@in_guaranteed Struct) -> @out Struct // user: %6
// ->   %6 = thin_to_thick_function %5 : $@convention(method) (@in_guaranteed Struct) -> @out Struct to $@callee_guaranteed (@in_guaranteed Struct) -> @out Struct // user: %11
