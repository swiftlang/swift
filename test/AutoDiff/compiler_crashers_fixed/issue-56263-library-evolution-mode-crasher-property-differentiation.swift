// RUN: %target-swift-frontend -c -enable-library-evolution %s

// https://github.com/apple/swift/issues/56263
// AutoDiff crasher on property derivatives in library evolution mode

import _Differentiation

public struct Struct: Differentiable {
  var stored: Float

  // Test property.
  @differentiable(reverse)
  public var property: Float {
    stored
  }

  @differentiable(reverse)
  public var property2: Float {
    stored + stored
  }

  @differentiable(reverse)
  public var property3: Float {
    stored.squareRoot()
  }
}

// Original crasher:
// Assertion failed: ((!dyn_cast_or_null<VarDecl>(Loc.getAsASTNode<Decl>()) || Var) && "location is a VarDecl, but SILDebugVariable is empty"), function createAllocStack, file .../swift/include/swift/SIL/SILBuilder.h, line 418.
// Please submit a bug report (https://swift.org/contributing/#reporting-bugs) and include the project and the crash backtrace.
// Stack dump:
// 0.	Program arguments: swift-frontend -c test2.swift -enable-library-evolution
// 1.	Swift version 5.3-dev (LLVM f681f671e2e9538, Swift 36090faaded56c2)
// 2.	While evaluating request ExecuteSILPipelineRequest(Run pipelines { Mandatory Diagnostic Passes + Enabling Optimization Passes } on SIL for test2.test2)
// 3.	While running pass #157 SILModuleTransform "Differentiation".
// 4.	While processing // differentiability witness for Struct.property.getter
// sil_differentiability_witness [serialized] [reverse] [parameters 0] [results 0] @$s5test26StructV8propertySfvg : $@convention(method) (@in_guaranteed Struct) -> Float {
// }
// 
//  on SIL function "@$s5test26StructV8propertySfvg".
//  for getter for property (at test2.swift:8:14)
// 5.	While generating VJP for SIL function "@$s5test26StructV8propertySfvg".
//  for getter for property (at test2.swift:8:14)
// 6.	While generating pullback for SIL function "@$s5test26StructV8propertySfvg".
//  for getter for property (at test2.swift:8:14)
// 0  swift-frontend           0x000000010de4a185 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 37
// 1  swift-frontend           0x000000010de490c8 llvm::sys::RunSignalHandlers() + 248
// 2  swift-frontend           0x000000010de4a766 SignalHandler(int) + 262
// 3  libsystem_platform.dylib 0x00007fff2035bd7d _sigtramp + 29
// 4  libsystem_platform.dylib 000000000000000000 _sigtramp + 18446603339975770784
// 5  libsystem_c.dylib        0x00007fff2026c741 abort + 120
// 6  libsystem_c.dylib        0x00007fff2026bb18 err + 0
// 7  swift-frontend           0x000000010e1cb063 swift::SILBuilder::createAllocStack(swift::SILLocation, swift::SILType, llvm::Optional<swift::SILDebugVariable>, bool) (.cold.2) + 35
// 8  swift-frontend           0x000000010a06716b swift::SILBuilder::createAllocStack(swift::SILLocation, swift::SILType, llvm::Optional<swift::SILDebugVariable>, bool) + 315
// 9  swift-frontend           0x0000000109af96d3 swift::autodiff::PullbackCloner::Implementation::visitLoadOperation(swift::SingleValueInstruction*) + 275
// 10 swift-frontend           0x0000000109aec37b swift::autodiff::PullbackCloner::Implementation::visit(swift::SILInstruction*) + 203
// 11 swift-frontend           0x0000000109ae8196 swift::autodiff::PullbackCloner::Implementation::visitSILBasicBlock(swift::SILBasicBlock*) + 838
// 12 swift-frontend           0x0000000109ae5504 swift::autodiff::PullbackCloner::Implementation::run() + 7268
// 13 swift-frontend           0x0000000109b077d3 swift::autodiff::VJPCloner::Implementation::run() + 1539
// 14 swift-frontend           0x0000000109c4e0b4 (anonymous namespace)::DifferentiationTransformer::canonicalizeDifferentiabilityWitness(swift::SILFunction*, swift::SILDifferentiabilityWitness*, swift::autodiff::DifferentiationInvoker, swift::SerializedKind_t) + 7172
// 15 swift-frontend           0x0000000109c4bafa (anonymous namespace)::Differentiation::run() + 1530
// 16 swift-frontend           0x0000000109c9c86e swift::SILPassManager::runModulePass(unsigned int) + 558
// 17 swift-frontend           0x0000000109ca144a swift::SILPassManager::execute() + 666
// 18 swift-frontend           0x0000000109c996a8 swift::SILPassManager::executePassPipelinePlan(swift::SILPassPipelinePlan const&) + 72
// 19 swift-frontend           0x0000000109c99643 swift::ExecuteSILPipelineRequest::evaluate(swift::Evaluator&, swift::SILPipelineExecutionDescriptor) const + 51
// 20 swift-frontend           0x0000000109cbc83d swift::SimpleRequest<swift::ExecuteSILPipelineRequest, std::__1::tuple<> (swift::SILPipelineExecutionDescriptor), (swift::RequestFlags)1>::evaluateRequest(swift::ExecuteSILPipelineRequest const&, swift::Evaluator&) + 29
// 21 swift-frontend           0x0000000109ca3a37 llvm::Expected<swift::ExecuteSILPipelineRequest::OutputType> swift::Evaluator::getResultUncached<swift::ExecuteSILPipelineRequest>(swift::ExecuteSILPipelineRequest const&) + 375
// 22 swift-frontend           0x0000000109c998d4 swift::executePassPipelinePlan(swift::SILModule*, swift::SILPassPipelinePlan const&, bool, swift::irgen::IRGenModule*) + 68
// 23 swift-frontend           0x0000000109ca6507 swift::runSILDiagnosticPasses(swift::SILModule&) + 87
// 24 swift-frontend           0x00000001096dd7bc swift::CompilerInstance::performSILProcessing(swift::SILModule*) + 60
// 25 swift-frontend           0x00000001095c4aa5 performCompileStepsPostSILGen(swift::CompilerInstance&, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> >, llvm::PointerUnion<swift::ModuleDecl*, swift::SourceFile*>, swift::PrimarySpecificPaths const&, int&, swift::FrontendObserver*) + 901
// 26 swift-frontend           0x00000001095c44fc performCompileStepsPostSema(swift::CompilerInstance&, int&, swift::FrontendObserver*) + 636
// 27 swift-frontend           0x00000001095ba328 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 4632
// 28 swift-frontend           0x0000000109551fee main + 846
// 29 libdyld.dylib            0x00007fff20332689 start + 1
// 30 libdyld.dylib            0x0000000000000004 start + 18446603339975940476
// [1]    21458 abort      xcrun $SWIFT_NINJA_BUILD_DIR/bin/swift-frontend -c test2.swift
