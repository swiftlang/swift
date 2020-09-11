// RUN: %target-swift-emit-sil %s -verify
// REQUIRES: asserts

// TF-1204: Subset parameters thunk crash for original function with `inout`
// parameters.

struct Convolution<T>: Differentiable
where T: Differentiable, T == T.TangentVector {
  var bias: T

  @differentiable(wrt: self)
  @differentiable
  func callAsFunction(_ input: T) -> T {
    var result = withoutDerivative(at: bias)
    infer(result: &result, input: input, bias: bias)
    return result
  }

  @differentiable
  func infer(result: inout T, input: T, bias: T) {
    fatalError()
  }

  @derivative(of: infer)
  func _vjpInfer(result: inout T, input: T, bias: T)
    -> (
      value: Void, pullback: (inout T) -> (Convolution<T>.TangentVector, T, T)
    )
  {
    fatalError()
  }
}

// Original crasher:
// Assertion failed: (origFnType->getResults().size() == 1), function getOrCreateSubsetParametersThunkForDerivativeFunction, file /Users/swiftninjas/s4tf/swift/lib/SILOptimizer/Utils/Differentiation/Thunk.cpp, line 812.
// Stack dump:
// 1.	Swift version 5.2-dev (LLVM b3057cffb6, Swift c8bea53782)
// 2.	While running pass #135 SILModuleTransform "Differentiation".
// 3.	While canonicalizing `differentiable_function` SIL node   %22 = differentiable_function [parameters 0 2 3] %18 : $@callee_guaranteed (@inout τ_0_0, @in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @in_guaranteed Convolution<τ_0_0>) -> () // users: %27, %23
// 4.	While ...in SIL function "@AD__$s4conv11ConvolutionV14callAsFunctionyxxF__vjp_src_0_wrt_1_s14DifferentiableRz13TangentVectorsAAPQzRszl".
//  for 'callAsFunction(_:)' (at conv.swift:8:5)
// 0  swift                    0x0000000104c08e75 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 37
// 1  swift                    0x0000000104c080b5 llvm::sys::RunSignalHandlers() + 85
// 2  swift                    0x0000000104c0945c SignalHandler(int) + 268
// 3  libsystem_platform.dylib 0x00007fff6deebb5d _sigtramp + 29
// 4  libsystem_platform.dylib 0x0000000000005290 _sigtramp + 18446603338671822672
// 5  libsystem_c.dylib        0x00007fff6dda56a6 abort + 127
// 6  libsystem_c.dylib        0x00007fff6dd6e20d basename_r + 0
// 7  swift                    0x0000000104ec58a3 swift::autodiff::getOrCreateSubsetParametersThunkForDerivativeFunction(swift::SILOptFunctionBuilder&, swift::SILValue, swift::SILValue, swift::AutoDiffDerivativeFunctionKind, swift::SILAutoDiffIndices, swift::SILAutoDiffIndices) (.cold.10) + 35
// 8  swift                    0x000000010136f079 swift::autodiff::getOrCreateSubsetParametersThunkForDerivativeFunction(swift::SILOptFunctionBuilder&, swift::SILValue, swift::SILValue, swift::AutoDiffDerivativeFunctionKind, swift::SILAutoDiffIndices, swift::SILAutoDiffIndices) + 7545
