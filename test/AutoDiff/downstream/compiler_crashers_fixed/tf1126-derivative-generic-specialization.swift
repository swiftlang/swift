// RUN: %target-swift-frontend -O -emit-sil %s -verify
// REQUIRES: asserts

// TF-1126: Generic specialization crash during capture propagation.
// Related to `@differentiable` function with `partial_apply` operands,
// to be specialized. Occurs only with `-O`.

struct A: Differentiable{
  var b: SIMD8<Float>
}

@differentiable
func function(a: A) -> A {
  var a = a
  a.b = a.b - SIMD8<Float>(repeating: 1.0)
  return a
}

let masks: [SIMD8<Float>] = [[1,0,0,0,0,0,0,0],
                             [0,1,0,0,0,0,0,0],
                             [0,0,1,0,0,0,0,0],
                             [0,0,0,1,0,0,0,0],
                             [0,0,0,0,1,0,0,0],
                             [0,0,0,0,0,1,0,0],
                             [0,0,0,0,0,0,1,0],
                             [0,0,0,0,0,0,0,1]]

extension SIMD8 where Scalar == Float{
  @differentiable(where Scalar: Differentiable)
  func updated(at index: Int, with newValue: Scalar) -> Self {
    let mask = masks[index]
    let result = self - (self * mask) + (newValue * mask)
    return result
  }
}

// Looking for a function: $ss4SIMDPss14DifferentiableRzSB6Scalars11SIMDStoragePRpzsAA13TangentVectorsACPRpzSBAhI_AdFRPzrlE12_vjpSubtract3lhs3rhsx5value_AJ_AJtAJc8pullbacktx_xtFZs5SIMD8VySfG_Tg5
// Expected type: @convention(method) (@in_guaranteed SIMD8<Float>, @in_guaranteed SIMD8<Float>, @thick SIMD8<Float>.Type) -> (@out SIMD8<Float>, @owned @callee_guaranteed (@in_guaranteed SIMD8<Float>) -> (@out SIMD8<Float>, @out SIMD8<Float>))
// Found    type: @convention(method) (SIMD8<Float>, SIMD8<Float>, @thick SIMD8<Float>.Type) -> (@out SIMD8<Float>, @owned @callee_guaranteed (@in_guaranteed SIMD8<Float>) -> (@out SIMD8<Float>, @out SIMD8<Float>))
// Assertion failed: (ReInfo.getSpecializedType() == SpecializedF->getLoweredFunctionType() && "Previously specialized function does not match expected type."), function lookupSpecialization, file /Users/swiftninjas/s4tf/swift/lib/SILOptimizer/Utils/Generics.cpp, line 1833.
// Stack dump:
// ...
// 1.	Swift version 5.2-dev (Swift bf631dc2e4)
// 2.	While running pass #113021 SILFunctionTransform "CapturePropagation" on SILFunction "@AD__$ss5SIMD8V6deleteSfRszrlE7updated2at4withABySfGSi_SftF__vjp_src_0_wrt_1_2".
//  for 'updated(at:with:)' (at /Users/porter/Dropbox (PassiveLogic)/Team/Team Members Scratch Space/Porter/Experiments/Playgrounds/delete/delete/main.swift:75:5)
// llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 37
// llvm::sys::RunSignalHandlers() + 85
// SignalHandler(int) + 278
// _sigtramp + 29
// _sigtramp + 2821162056
// abort + 127
// basename_r + 0
// swift::GenericFuncSpecializer::lookupSpecialization() (.cold.1) + 35
// swift::GenericFuncSpecializer::lookupSpecialization() + 2109
// (anonymous namespace)::CapturePropagation::optimizePartialApply(swift::PartialApplyInst*) + 1301
// (anonymous namespace)::CapturePropagation::run() + 265
