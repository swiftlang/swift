// RUN: %target-swift-frontend -emit-sil -verify %s
// SR-15808: In AST, type checking skips a closure with non-differentiable input
// where `Void` is included as a parameter without being marked `@noDerivative`.
// It also crashes when the output is `Void` and no input is `inout`. As a
// result, the compiler crashes during Sema.
import _Differentiation

// expected-error @+1 {{'@differentiable' function type requires a differentiable result, i.e. a non-'Void' type that conforms to 'Differentiable'}}
func helloWorld(_ x: @differentiable(reverse) (()) -> Void) {}

func helloWorld(_ x: @differentiable(reverse) (()) -> Float) {}

// expected-error @+1 {{'@differentiable' function type requires a differentiable result, i.e. a non-'Void' type that conforms to 'Differentiable'}}
func helloWorld(_ x: @differentiable(reverse) (Float) -> Void) {}

func helloWorld(_ x: @differentiable(reverse) (@noDerivative Float, Void) -> Float) {}

// Original crash:
// Assertion failed: (!parameterIndices->isEmpty() && "Parameter indices must not be empty"), function getAutoDiffDerivativeFunctionType, file SILFunctionType.cpp, line 800.
// Stack dump:
// ...
// 1.  Apple Swift version 5.6-dev (LLVM 7b20e61dd04138a, Swift 9438cf6b2e83c5f)
// 2.  Compiling with the current language version
// 3.  While evaluating request ASTLoweringRequest(Lowering AST to SIL for file "/Users/philipturner/Desktop/Experimentation4/Experimentation4/main.swift")
// Stack dump without symbol names (ensure you have llvm-symbolizer in your PATH or set the environment var `LLVM_SYMBOLIZER_PATH` to point to it):
// 0  swift-frontend           0x0000000108d7a5c0 llvm::sys::PrintStackTrace(llvm::raw_ostream&, int) + 56
// 1  swift-frontend           0x0000000108d79820 llvm::sys::RunSignalHandlers() + 128
// 2  swift-frontend           0x0000000108d7ac24 SignalHandler(int) + 304
// 3  libsystem_platform.dylib 0x00000001bb5304e4 _sigtramp + 56
// 4  libsystem_pthread.dylib  0x00000001bb518eb0 pthread_kill + 288
// 5  libsystem_c.dylib        0x00000001bb456314 abort + 164
// 6  libsystem_c.dylib        0x00000001bb45572c err + 0
// 7  swift-frontend           0x0000000108d9ae3c swift::SILFunctionType::getAutoDiffDerivativeFunctionType(swift::IndexSubset*, swift::IndexSubset*, swift::AutoDiffDerivativeFunctionKind, swift::Lowering::TypeConverter&, llvm::function_ref<swift::ProtocolConformanceRef (swift::CanType, swift::Type, swift::ProtocolDecl*)>, swift::CanGenericSignature, bool, swift::CanType) (.cold.3) + 0
// 8  swift-frontend           0x0000000104abc35c swift::SILFunctionType::getAutoDiffDerivativeFunctionType(swift::IndexSubset*, swift::IndexSubset*, swift::AutoDiffDerivativeFunctionKind, swift::Lowering::TypeConverter&, llvm::function_ref<swift::ProtocolConformanceRef (swift::CanType, swift::Type, swift::ProtocolDecl*)>, swift::CanGenericSignature, bool, swift::CanType) + 152
// 9  swift-frontend           0x0000000104b496cc (anonymous namespace)::TypeClassifierBase<(anonymous namespace)::LowerType, swift::Lowering::TypeLowering*>::getNormalDifferentiableSILFunctionTypeRecursiveProperties(swift::CanTypeWrapper<swift::SILFunctionType>, swift::Lowering::AbstractionPattern) + 184
// 10 swift-frontend           0x0000000104b3b72c swift::CanTypeVisitor<(anonymous namespace)::LowerType, swift::Lowering::TypeLowering*, swift::Lowering::AbstractionPattern, swift::Lowering::IsTypeExpansionSensitive_t>::visit(swift::CanType, swift::Lowering::AbstractionPattern, swift::Lowering::IsTypeExpansionSensitive_t) + 1980
// 11 swift-frontend           0x0000000104b3c0e0 swift::Lowering::TypeConverter::getTypeLoweringForLoweredType(swift::Lowering::AbstractionPattern, swift::CanType, swift::TypeExpansionContext, swift::Lowering::IsTypeExpansionSensitive_t) + 648
// 12 swift-frontend           0x0000000104b3ae08 swift::Lowering::TypeConverter::getTypeLowering(swift::Lowering::AbstractionPattern, swift::Type, swift::TypeExpansionContext) + 708
// 13 swift-frontend           0x0000000104ac8544 (anonymous namespace)::DestructureInputs::visit(swift::ValueOwnership, bool, swift::Lowering::AbstractionPattern, swift::CanType, bool, bool) + 184
// 14 swift-frontend           0x0000000104ac6a1c getSILFunctionType(swift::Lowering::TypeConverter&, swift::TypeExpansionContext, swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::SILExtInfoBuilder, (anonymous namespace)::Conventions const&, swift::ForeignInfo const&, llvm::Optional<swift::SILDeclRef>, llvm::Optional<swift::SILDeclRef>, llvm::Optional<swift::SubstitutionMap>, swift::ProtocolConformanceRef, llvm::Optional<llvm::SmallBitVector>) + 2584
// 15 swift-frontend           0x0000000104ac5f98 getNativeSILFunctionType(swift::Lowering::TypeConverter&, swift::TypeExpansionContext, swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::SILExtInfoBuilder, llvm::Optional<swift::SILDeclRef>, llvm::Optional<swift::SILDeclRef>, llvm::Optional<swift::SubstitutionMap>, swift::ProtocolConformanceRef, llvm::Optional<llvm::SmallBitVector>)::$_12::operator()((anonymous namespace)::Conventions const&) const + 316
// 16 swift-frontend           0x0000000104abf55c getNativeSILFunctionType(swift::Lowering::TypeConverter&, swift::TypeExpansionContext, swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::SILExtInfoBuilder, llvm::Optional<swift::SILDeclRef>, llvm::Optional<swift::SILDeclRef>, llvm::Optional<swift::SubstitutionMap>, swift::ProtocolConformanceRef, llvm::Optional<llvm::SmallBitVector>) + 508
// 17 swift-frontend           0x0000000104ac0b44 getUncachedSILFunctionTypeForConstant(swift::Lowering::TypeConverter&, swift::TypeExpansionContext, swift::SILDeclRef, swift::Lowering::TypeConverter::LoweredFormalTypes) + 1920
// 18 swift-frontend           0x0000000104ac1474 swift::Lowering::TypeConverter::getConstantInfo(swift::TypeExpansionContext, swift::SILDeclRef) + 216
// 19 swift-frontend           0x0000000104ab9808 swift::SILFunctionBuilder::getOrCreateFunction(swift::SILLocation, swift::SILDeclRef, swift::ForDefinition_t, llvm::function_ref<swift::SILFunction* (swift::SILLocation, swift::SILDeclRef)>, swift::ProfileCounter) + 132
// 20 swift-frontend           0x0000000104f1d120 swift::Lowering::SILGenModule::getFunction(swift::SILDeclRef, swift::ForDefinition_t) + 328
// 21 swift-frontend           0x0000000104f2086c emitOrDelayFunction(swift::Lowering::SILGenModule&, swift::SILDeclRef, bool) + 344
// 22 swift-frontend           0x0000000104f1d828 swift::Lowering::SILGenModule::emitFunction(swift::FuncDecl*) + 140
// 23 swift-frontend           0x0000000104f2294c swift::ASTLoweringRequest::evaluate(swift::Evaluator&, swift::ASTLoweringDescriptor) const + 1612
// 24 swift-frontend           0x0000000104fcbca4 swift::SimpleRequest<swift::ASTLoweringRequest, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> > (swift::ASTLoweringDescriptor), (swift::RequestFlags)9>::evaluateRequest(swift::ASTLoweringRequest const&, swift::Evaluator&) + 156
// 25 swift-frontend           0x0000000104f2647c llvm::Expected<swift::ASTLoweringRequest::OutputType> swift::Evaluator::getResultUncached<swift::ASTLoweringRequest>(swift::ASTLoweringRequest const&) + 408
// 26 swift-frontend           0x0000000104f233b0 swift::performASTLowering(swift::FileUnit&, swift::Lowering::TypeConverter&, swift::SILOptions const&) + 104
// 27 swift-frontend           0x0000000104a12088 swift::performCompileStepsPostSema(swift::CompilerInstance&, int&, swift::FrontendObserver*) + 496
// 28 swift-frontend           0x0000000104a13d08 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 2936
// 29 swift-frontend           0x00000001049b213c swift::mainEntry(int, char const**) + 500
// 30 dyld                     0x00000001113c90f4 start + 520
