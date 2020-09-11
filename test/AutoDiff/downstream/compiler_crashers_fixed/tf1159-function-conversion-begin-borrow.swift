// RUN: %target-swift-frontend -emit-sil %s -verify
// REQUIRES: asserts

// TF-1159: `begin_borrow` instruction unhandled in the
// `reapplyFunctionConversion` helper function.

func id<T>(_ x: T) -> T { x }

@differentiable
func TF_1159(_ x: Float) -> Float {
  // Note: code below generates `partial_apply` and `begin_borrow`.
  let fn: (Float) -> Float = id
  return fn(x)
}

// Unhandled function conversion instruction
// UNREACHABLE executed at swift/lib/SILOptimizer/Mandatory/Differentiation.cpp:433!
// Stack dump:
// ...
// 1.	Swift version 5.2-dev (Swift 415d33b3f1)
// 2.	While running pass #27 SILModuleTransform "Differentiation".
// 3.	While canonicalizing `differentiable_function` SIL node   %12 = differentiable_function [parameters 0] %10 : $@callee_guaranteed (Float) -> Float // users: %17, %13
// 4.	While ...in SIL function "@AD__$s4main3fooyS2fF__vjp_src_0_wrt_0".
//  for 'foo(_:)' (at tf-1159.swift:4:1)
// 0  swift                    0x0000000107b15105 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 37
// 1  swift                    0x0000000107b14078 llvm::sys::RunSignalHandlers() + 248
// 2  swift                    0x0000000107b15706 SignalHandler(int) + 278
// 3  libsystem_platform.dylib 0x00007fff68ed2b5d _sigtramp + 29
// 4  libsystem_platform.dylib 0x0000000000000053 _sigtramp + 2534593811
// 5  libsystem_c.dylib        0x00007fff68d8c6a6 abort + 127
// 6  swift                    0x0000000108dcc09e llvm::llvm_unreachable_internal(char const*, char const*, unsigned int) + 462
// 7  swift                    0x0000000104047ec2 reapplyFunctionConversion(swift::autodiff::ADContext&, swift::SILValue, swift::SILValue, swift::SILValue, swift::SILBuilder&, swift::SILLocation, llvm::SmallVectorImpl<swift::AllocStackInst*>&, swift::IndexSubset*, swift::GenericSignature) + 1506
// 8  swift                    0x000000010402d5c0 (anonymous namespace)::DifferentiationTransformer::promoteToDifferentiableFunction(swift::DifferentiableFunctionInst*, swift::SILBuilder&, swift::SILLocation, swift::autodiff::DifferentiationInvoker) + 8880
// 9  swift                    0x00000001040292ea (anonymous namespace)::DifferentiationTransformer::processDifferentiableFunctionInst(swift::DifferentiableFunctionInst*) + 426
// 10 swift                    0x0000000104026b06 (anonymous namespace)::Differentiation::run() + 1174
