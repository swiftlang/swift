// RUN: not --crash %target-swift-frontend -emit-sil %s

// SR-12641: SILGen verification error regarding `ImmutableAddressUseVerifier` and AutoDiff-generated code.

import _Differentiation
import DifferentiationUnittest

class Class: Differentiable {
  var x: Tracked<Float>
  init(_ x: Tracked<Float>) {
    self.x = x
  }
}

func getter(_ c: Class) -> Tracked<Float> {
  return c.x
}
_ = gradient(at: Class(10), in: getter)

// Assertion failed: (conv.isIndirectConvention() && "Expect an indirect convention"), function isConsumingOrMutatingArgumentConvention, file swift/lib/SIL/Verifier/SILVerifier.cpp, line 453.
// Stack dump:
// ...
// 1.	Swift version 5.3-dev (LLVM ca0260ddec, Swift b17e1b23fe)
// 2.	While evaluating request SILGenWholeModuleRequest(SIL Generation for module main)
// 3.	While verifying SIL function "@$s4main5ClassC13TangentVectorV23DifferentiationUnittest7TrackedVySfGIeggr_AeIIegnr_TR".
//  for <<debugloc at "<compiler-generated>":0:0>>0  swift                    0x000000010d6b4138 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 40
// 1  swift                    0x000000010d6b30b8 llvm::sys::RunSignalHandlers() + 248
// 2  swift                    0x000000010d6b472d SignalHandler(int) + 285
// 3  libsystem_platform.dylib 0x00007fff718335fd _sigtramp + 29
// 4  libsystem_platform.dylib 000000000000000000 _sigtramp + 18446603338611739168
// 5  libsystem_c.dylib        0x00007fff71709808 abort + 120
// 6  libsystem_c.dylib        0x00007fff71708ac6 err + 0
// 7  swift                    0x000000010da31c23 (anonymous namespace)::ImmutableAddressUseVerifier::isConsumingOrMutatingApplyUse(swift::Operand*) (.cold.4) + 35
// 8  swift                    0x0000000109c74d11 (anonymous namespace)::ImmutableAddressUseVerifier::isConsumingOrMutatingApplyUse(swift::Operand*) + 289
// 9  swift                    0x0000000109c73c1d (anonymous namespace)::ImmutableAddressUseVerifier::isMutatingOrConsuming(swift::SILValue) + 157
// 10 swift                    0x0000000109c5cea9 (anonymous namespace)::SILVerifier::visitSILBasicBlock(swift::SILBasicBlock*) + 1161
