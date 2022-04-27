// RUN: %target-swift-frontend -emit-sil %s

// SR-15793: In the original reproducer, it swizzled the components of a
// gradient with multiple return values when there was mutating function inside
// of an encapsulating differentiable function. Changing the gradient types from
// all `Double` to some `Float` and some `Double` caused the compiler to crash, 
// exposing that the derivatives were being processed incorrectly during SILGen.

import _Differentiation

extension Double {
  mutating func addTwo(_ lhs: Float, _ mhs: Double) {
    self += Double(lhs) + mhs
  }

  @derivative(of: addTwo)
  mutating func _vjpAddTwo(
    _ lhs: Float,
    _ mhs: Double
  ) -> (value: Void, pullback: (inout Double) -> (Float, Double)) {
    addTwo(lhs, mhs)
    return ((), { v in (lhs, mhs) })
  }
}

// Original crash:

// Assertion failed: (fromRes.getInterfaceType() == toRes.getInterfaceType()), function getThunkedAutoDiffLinearMap, file SILGenPoly.cpp, line 3541.
// Stack dump:
// 0.	Program arguments: /Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2022-04-04-a.xctoolchain/usr/bin/swift-frontend -frontend -interpret script.swift -Xllvm -aarch64-use-tbi -enable-objc-interop -sdk /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk -color-diagnostics -new-driver-path /Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2022-04-04-a.xctoolchain/usr/bin/swift-driver -empty-abi-descriptor -resource-dir /Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2022-04-04-a.xctoolchain/usr/lib/swift -module-name script -target-sdk-version 12.3
// 1.	Apple Swift version 5.7-dev (LLVM aac709978dda363, Swift 8c308d0ff137dde)
// 2.	Compiling with the current language version
// 3.	While evaluating request ASTLoweringRequest(Lowering AST to SIL for module script)
// 4.	While silgen emitFunction SIL function "@$sSd6scriptE10_vjpAddTwoyyt5value_Sf_SdtSdzc8pullbacktSf_SdtF".
//  for '_vjpAddTwo(_:_:)' (at script.swift:9:12)
// Stack dump without symbol names (ensure you have llvm-symbolizer in your PATH or set the environment var `LLVM_SYMBOLIZER_PATH` to point to it):
// 0  swift-frontend           0x00000001093b9f14 llvm::sys::PrintStackTrace(llvm::raw_ostream&, int) + 56
// 1  swift-frontend           0x00000001093b9174 llvm::sys::RunSignalHandlers() + 128
// 2  swift-frontend           0x00000001093ba578 SignalHandler(int) + 304
// 3  libsystem_platform.dylib 0x000000019cd194c4 _sigtramp + 56
// 4  libsystem_pthread.dylib  0x000000019cd01ee0 pthread_kill + 288
// 5  libsystem_c.dylib        0x000000019cc3c340 abort + 168
// 6  libsystem_c.dylib        0x000000019cc3b754 err + 0
// 7  swift-frontend           0x000000010953f020 swift::Lowering::SILGenFunction::getThunkedAutoDiffLinearMap(swift::Lowering::ManagedValue, swift::AutoDiffLinearMapKind, swift::CanTypeWrapper<swift::SILFunctionType>, swift::CanTypeWrapper<swift::SILFunctionType>, bool) (.cold.52) + 0
// 8  swift-frontend           0x00000001054b0618 swift::Lowering::SILGenFunction::getThunkedAutoDiffLinearMap(swift::Lowering::ManagedValue, swift::AutoDiffLinearMapKind, swift::CanTypeWrapper<swift::SILFunctionType>, swift::CanTypeWrapper<swift::SILFunctionType>, bool) + 5312
// 9  swift-frontend           0x00000001054b1774 swift::Lowering::SILGenModule::getOrCreateCustomDerivativeThunk(swift::AbstractFunctionDecl*, swift::SILFunction*, swift::SILFunction*, swift::AutoDiffConfig const&, swift::AutoDiffDerivativeFunctionKind) + 2812
// 10 swift-frontend           0x0000000105415bc4 swift::Lowering::SILGenModule::emitDifferentiabilityWitness(swift::AbstractFunctionDecl*, swift::SILFunction*, swift::DifferentiabilityKind, swift::AutoDiffConfig const&, swift::SILFunction*, swift::SILFunction*, swift::DeclAttribute const*) + 560
// 11 swift-frontend           0x0000000105415950 swift::Lowering::SILGenModule::emitDifferentiabilityWitnessesForFunction(swift::SILDeclRef, swift::SILFunction*)::$_1::operator()(swift::DeclAttributes&) const + 832
// 12 swift-frontend           0x0000000105415608 swift::Lowering::SILGenModule::emitDifferentiabilityWitnessesForFunction(swift::SILDeclRef, swift::SILFunction*) + 180
// 13 swift-frontend           0x00000001054152bc swift::Lowering::SILGenModule::postEmitFunction(swift::SILDeclRef, swift::SILFunction*) + 260
// 14 swift-frontend           0x00000001054146e8 swift::Lowering::SILGenModule::emitFunctionDefinition(swift::SILDeclRef, swift::SILFunction*) + 6900
// 15 swift-frontend           0x00000001054161bc emitOrDelayFunction(swift::Lowering::SILGenModule&, swift::SILDeclRef, bool) + 384
// 16 swift-frontend           0x0000000105412bdc swift::Lowering::SILGenModule::emitFunction(swift::FuncDecl*) + 144
// 17 swift-frontend           0x00000001054d3a90 SILGenExtension::visitFuncDecl(swift::FuncDecl*) + 160
// 18 swift-frontend           0x00000001054d02ec SILGenExtension::emitExtension(swift::ExtensionDecl*) + 84
// 19 swift-frontend           0x00000001054d028c swift::Lowering::SILGenModule::visitExtensionDecl(swift::ExtensionDecl*) + 24
// 20 swift-frontend           0x000000010541831c swift::ASTLoweringRequest::evaluate(swift::Evaluator&, swift::ASTLoweringDescriptor) const + 1792
// 21 swift-frontend           0x00000001054c39f0 swift::SimpleRequest<swift::ASTLoweringRequest, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> > (swift::ASTLoweringDescriptor), (swift::RequestFlags)9>::evaluateRequest(swift::ASTLoweringRequest const&, swift::Evaluator&) + 156
// 22 swift-frontend           0x000000010541c2dc llvm::Expected<swift::ASTLoweringRequest::OutputType> swift::Evaluator::getResultUncached<swift::ASTLoweringRequest>(swift::ASTLoweringRequest const&) + 408
// 23 swift-frontend           0x0000000105418e50 swift::performASTLowering(swift::ModuleDecl*, swift::Lowering::TypeConverter&, swift::SILOptions const&) + 144
// 24 swift-frontend           0x0000000104ef128c swift::performCompileStepsPostSema(swift::CompilerInstance&, int&, swift::FrontendObserver*) + 992
// 25 swift-frontend           0x0000000104efc434 withSemanticAnalysis(swift::CompilerInstance&, swift::FrontendObserver*, llvm::function_ref<bool (swift::CompilerInstance&)>, bool) + 160
// 26 swift-frontend           0x0000000104ef2d50 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 2940
// 27 swift-frontend           0x0000000104e10494 swift::mainEntry(int, char const**) + 3748
// 28 dyld                     0x0000000111ce9088 start + 516
