// RUN: %target-swift-frontend -emit-sil -verify %s
import _Differentiation

// expected-error @+1 {{@differentiable' function type requires a differentiable result, i.e. a non-'Void' type that conforms to 'Differentiable'}}
typealias MyType = @differentiable(reverse) (inout @noDerivative Float, Float) -> Void

@differentiable(reverse)
func myFunc(_ x: inout @noDerivative Float, _ q: Float) -> Void {}

let castedFunc = myFunc as MyType

// Original crash:
// Assertion failed: (Index < Length && "Invalid index!"), function operator[], file ArrayRef.h, line 257.
// Stack dump:
// ...
// 1.  Swift version 5.7-dev (LLVM a9a5fb525c61dd0, Swift f69d528185a8bfe)
// 2.  Compiling with the current language version
// 3.  While evaluating request ExecuteSILPipelineRequest(Run pipelines { Mandatory Diagnostic Passes + Enabling Optimization Passes } on SIL for file)
// 4.  While running pass #46 SILModuleTransform "Differentiation".
// 5.  While canonicalizing `differentiable_function` SIL node   %12 = differentiable_function [parameters 1] [results 0] %9 : $@callee_guaranteed (@inout Float, Float, Int) -> () // users: %23, %18, %13
// 6.  While ...in SIL function "@main".
// Stack dump without symbol names (ensure you have llvm-symbolizer in your PATH or set the environment var `LLVM_SYMBOLIZER_PATH` to point to it):
// 0  swift-frontend           0x0000000105719dc0 llvm::sys::PrintStackTrace(llvm::raw_ostream&, int) + 56
// 1  swift-frontend           0x0000000105718ef0 llvm::sys::RunSignalHandlers() + 128
// 2  swift-frontend           0x000000010571a424 SignalHandler(int) + 304
// 3  libsystem_platform.dylib 0x00000001bb5304e4 _sigtramp + 56
// 4  libsystem_pthread.dylib  0x00000001bb518eb0 pthread_kill + 288
// 5  libsystem_c.dylib        0x00000001bb456314 abort + 164
// 6  libsystem_c.dylib        0x00000001bb45572c err + 0
// 7  swift-frontend           0x0000000105a1bab8 swift::SILFunctionConventions::getSILArgumentConvention(unsigned int) const (.cold.4) + 0
// 8  swift-frontend           0x00000001016a1f9c swift::SILFunctionConventions::getSILArgumentConvention(unsigned int) const + 192
// 9  swift-frontend           0x0000000100ebc888 (anonymous namespace)::OperandOwnershipClassifier::visitFullApply(swift::FullApplySite) + 244
// 10 swift-frontend           0x0000000100ebae70 swift::Operand::getOperandOwnership() const + 148
// 11 swift-frontend           0x0000000100ebada0 swift::checkOperandOwnershipInvariants(swift::Operand const*) + 28
// 12 swift-frontend           0x0000000100fe2cc8 swift::SILInstruction::verifyOperandOwnership() const + 444
// 13 swift-frontend           0x00000001014f32f4 swift::autodiff::getOrCreateSubsetParametersThunkForLinearMap(swift::SILOptFunctionBuilder&, swift::SILFunction*, swift::CanTypeWrapper<swift::SILFunctionType>, swift::CanTypeWrapper<swift::SILFunctionType>, swift::CanTypeWrapper<swift::SILFunctionType>, swift::AutoDiffDerivativeFunctionKind, swift::AutoDiffConfig const&, swift::AutoDiffConfig const&, swift::autodiff::ADContext&) + 3800
// 14 swift-frontend           0x00000001014f455c swift::autodiff::getOrCreateSubsetParametersThunkForDerivativeFunction(swift::SILOptFunctionBuilder&, swift::SILValue, swift::SILValue, swift::AutoDiffDerivativeFunctionKind, swift::AutoDiffConfig const&, swift::AutoDiffConfig const&, swift::autodiff::ADContext&) + 2828
// 15 swift-frontend           0x00000001015f012c (anonymous namespace)::DifferentiationTransformer::promoteToDifferentiableFunction(swift::DifferentiableFunctionInst*, swift::SILBuilder&, swift::SILLocation, swift::autodiff::DifferentiationInvoker) + 7120
// 16 swift-frontend           0x00000001015ecae0 (anonymous namespace)::DifferentiationTransformer::processDifferentiableFunctionInst(swift::DifferentiableFunctionInst*) + 452
// 17 swift-frontend           0x00000001015eab14 (anonymous namespace)::Differentiation::run() + 1152
// 18 swift-frontend           0x0000000101659548 swift::SILPassManager::runModulePass(unsigned int) + 840
// 19 swift-frontend           0x000000010165eaa4 swift::SILPassManager::execute() + 628
// 20 swift-frontend           0x0000000101656428 swift::SILPassManager::executePassPipelinePlan(swift::SILPassPipelinePlan const&) + 68
// 21 swift-frontend           0x00000001016563c8 swift::ExecuteSILPipelineRequest::evaluate(swift::Evaluator&, swift::SILPipelineExecutionDescriptor) const + 52
// 22 swift-frontend           0x000000010167c574 swift::SimpleRequest<swift::ExecuteSILPipelineRequest, std::__1::tuple<> (swift::SILPipelineExecutionDescriptor), (swift::RequestFlags)1>::evaluateRequest(swift::ExecuteSILPipelineRequest const&, swift::Evaluator&) + 28
// 23 swift-frontend           0x0000000101664c2c llvm::Expected<swift::ExecuteSILPipelineRequest::OutputType> swift::Evaluator::getResultUncached<swift::ExecuteSILPipelineRequest>(swift::ExecuteSILPipelineRequest const&) + 252
// 24 swift-frontend           0x0000000101656618 swift::executePassPipelinePlan(swift::SILModule*, swift::SILPassPipelinePlan const&, bool, swift::irgen::IRGenModule*) + 68
// 25 swift-frontend           0x0000000101669014 swift::runSILDiagnosticPasses(swift::SILModule&) + 92
// 26 swift-frontend           0x0000000100e81634 swift::CompilerInstance::performSILProcessing(swift::SILModule*) + 72
// 27 swift-frontend           0x0000000100e273cc performCompileStepsPostSILGen(swift::CompilerInstance&, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> >, llvm::PointerUnion<swift::ModuleDecl*, swift::SourceFile*>, swift::PrimarySpecificPaths const&, int&, swift::FrontendObserver*) + 676
// 28 swift-frontend           0x0000000100e26db8 swift::performCompileStepsPostSema(swift::CompilerInstance&, int&, swift::FrontendObserver*) + 1028
// 29 swift-frontend           0x0000000100e28870 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 3012
// 30 swift-frontend           0x0000000100d62830 swift::mainEntry(int, char const**) + 484
// 31 dyld                     0x00000001179d90f4 start + 520
