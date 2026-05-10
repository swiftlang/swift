// RUN: not %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify %s

// https://github.com/apple/swift/issues/55099
// Differentiation transform crashes for original function with opaque
// result type.
//
// If supporting this is non-trivial, we could reject earlier during Sema.

import _Differentiation

@differentiable(reverse)
func opaqueResult(_ x: Float) -> some Differentiable { x }

// swift: swift/lib/SILOptimizer/Differentiation/PullbackEmitter.cpp:244: void swift::autodiff::PullbackEmitter::addAdjointValue(swift::SILBasicBlock *, swift::SILValue, swift::autodiff::AdjointValue, swift::SILLocation): Assertion `newAdjointValue.getType() == getRemappedTangentType(originalValue->getType())' failed.
// Stack dump:
// ...
// 1.      Swift version 5.3-dev (LLVM f66b332548, Swift 05c47da664)
// 2.      While evaluating request ExecuteSILPipelineRequest(Run pipelines { Guaranteed Passes } on SIL for main.main)
// 3.      While running pass #15 SILModuleTransform "Differentiation".
// 4.      While processing // differentiability witness for opaqueResult(_:)
// sil_differentiability_witness hidden [reverse] [parameters 0] [results 0] @$s4main12opaqueResultyQrSfF : $@convention(thin) (Float) -> @out @_opaqueReturnTypeOf("$s4main12opaqueResultyQrSfF", 0) __ {
// }
//  on SIL function "@$s4main12opaqueResultyQrSfF".
//  for 'opaqueResult(_:)' (at swift/test/AutoDiff/compiler_crashers/sr12656-differentiation-opaque-result-type.swift:13:1)
// 5.      While generating VJP for SIL function "@$s4main12opaqueResultyQrSfF".
//  for 'opaqueResult(_:)' (at swift/test/AutoDiff/compiler_crashers/sr12656-differentiation-opaque-result-type.swift:13:1)
// 6.      While generating pullback for SIL function "@$s4main12opaqueResultyQrSfF".
//  for 'opaqueResult(_:)' (at swift/test/AutoDiff/compiler_crashers/sr12656-differentiation-opaque-result-type.swift:13:1)
