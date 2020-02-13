// RUN: %target-swift-frontend -O -emit-ir %s
// REQUIRES: asserts

// TF-891: Generic specialization crash during capture propagation.
// Related to `@differentiable` function with `partial_apply` operands,
// to be specialized. Occurs only with `-O`.

public protocol Protocol: Differentiable {
  @differentiable
  func requirement1<T: Protocol>(_ arg: T) -> Float

  @differentiable
  func requirement2() -> Float
}

public extension Protocol {
  @differentiable
  func requirement1<T: Protocol>(_ arg: T) -> Float {
    return arg.requirement2()
  }

  @differentiable
  func requirement2() -> Float {
    return 0
  }
}

public struct Struct: Protocol {}

@differentiable
public func func1<T: Protocol>(_ arg1: Float, _ arg2: T) -> Float {
  return arg2.requirement1(arg2)
}

@differentiable
public func func2(_ arg: Struct) -> Float {
  return func1(0.0, arg)
}

// swift: /usr/local/google/home/marcrasi/swift-base/swift/lib/AST/ProtocolConformance.cpp:78: swift::ProtocolDecl *swift::ProtocolConformanceRef::getRequirement() const: Assertion `!isInvalid()' failed.
// Stack dump:
// 0.	Program arguments: /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift -frontend -target x86_64-unknown-linux-gnu -module-cache-path /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/swift-test-results/x86_64-unknown-linux-gnu/clang-module-cache -swift-version 4 -ignore-module-source-info -typo-correction-limit 10 -O -emit-ir /usr/local/google/home/marcrasi/swift-base/swift/test/AutoDiff/generated/generated0002.swift 
// 1.	Swift version 5.1.1-dev (LLVM 6e04008c7f, Swift 439808dd48)
// 2.	While running pass #10183 SILFunctionTransform "CapturePropagation" on SILFunction "@AD__orig_$s13generated00025func1yS2f_xtAA8ProtocolRzlF_$sSf13generated00026StructVS3fAC13TangentVectorVIegydr_Iegyndo_SfACS2fAEIegyr_Iegyndo_TR_src_0_wrt_1_vjp_subset_parameters_thunk".
//  for expression at [/usr/local/google/home/marcrasi/swift-base/swift/test/AutoDiff/generated/generated0002.swift:35:10 - line:35:24] RangeText="func1(0.0, arg"
//  #0 0x0000000004bebbc4 PrintStackTraceSignalHandler(void*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4bebbc4)
//  #1 0x0000000004be97de llvm::sys::RunSignalHandlers() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4be97de)
//  #2 0x0000000004bebe78 SignalHandler(int) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4bebe78)
//  #3 0x00007f1ffb1283a0 __restore_rt (/lib/x86_64-linux-gnu/libpthread.so.0+0x123a0)
//  #4 0x00007f1ffa25fcfb raise (/lib/x86_64-linux-gnu/libc.so.6+0x36cfb)
//  #5 0x00007f1ffa24a8ad abort (/lib/x86_64-linux-gnu/libc.so.6+0x218ad)
//  #6 0x00007f1ffa24a77f (/lib/x86_64-linux-gnu/libc.so.6+0x2177f)
//  #7 0x00007f1ffa258542 (/lib/x86_64-linux-gnu/libc.so.6+0x2f542)
//  #8 0x000000000171bf3d (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x171bf3d)
//  #9 0x0000000000ecf489 swift::WitnessMethodInst::create(swift::SILDebugLocation, swift::CanType, swift::ProtocolConformanceRef, swift::SILDeclRef, swift::SILType, swift::SILFunction*, swift::SILOpenedArchetypesState&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xecf489)
// #10 0x0000000000e0cdbc swift::SILCloner<swift::GenericCloner>::visitWitnessMethodInst(swift::WitnessMethodInst*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xe0cdbc)
// #11 0x0000000000e0972c swift::SILCloner<swift::GenericCloner>::visitBlocksDepthFirst(swift::SILBasicBlock*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xe0972c)
// #12 0x0000000000e0761e swift::SILCloner<swift::GenericCloner>::cloneFunctionBody(swift::SILFunction*, swift::SILBasicBlock*, llvm::ArrayRef<swift::SILValue>) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xe0761e)
// #13 0x0000000000e07171 swift::GenericCloner::populateCloned() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xe07171)
// #14 0x0000000000b2ea3f swift::GenericCloner::cloneFunction(swift::SILOptFunctionBuilder&, swift::SILFunction*, swift::ReabstractionInfo const&, swift::SubstitutionMap, llvm::StringRef, std::function<void (swift::SILInstruction*, swift::SILInstruction*)>) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xb2ea3f)
// #15 0x0000000000b2e7ba swift::GenericFuncSpecializer::tryCreateSpecialization() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xb2e7ba)
// #16 0x0000000000c5569c (anonymous namespace)::CapturePropagation::optimizePartialApply(swift::PartialApplyInst*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xc5569c)
// #17 0x0000000000c55158 (anonymous namespace)::CapturePropagation::run() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xc55158)
// #18 0x000000000099288e swift::SILPassManager::runPassOnFunction(unsigned int, swift::SILFunction*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x99288e)
// #19 0x0000000000993913 swift::SILPassManager::runFunctionPasses(unsigned int, unsigned int) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x993913)
// #20 0x0000000000994c1f swift::SILPassManager::execute() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x994c1f)
// #21 0x000000000056892b swift::SILPassManager::executePassPipelinePlan(swift::SILPassPipelinePlan const&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x56892b)
// #22 0x000000000099d0dc swift::runSILOptimizationPasses(swift::SILModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x99d0dc)
// #23 0x0000000000769d37 swift::CompilerInstance::performSILProcessing(swift::SILModule*, swift::UnifiedStatsReporter*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x769d37)
// #24 0x00000000004f3d14 performCompileStepsPostSILGen(swift::CompilerInstance&, swift::CompilerInvocation&, std::unique_ptr<swift::SILModule, std::default_delete<swift::SILModule> >, bool, llvm::PointerUnion<swift::ModuleDecl*, swift::SourceFile*>, swift::PrimarySpecificPaths const&, bool, int&, swift::FrontendObserver*, swift::UnifiedStatsReporter*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4f3d14)
// #25 0x00000000004e9f76 performCompile(swift::CompilerInstance&, swift::CompilerInvocation&, llvm::ArrayRef<char const*>, int&, swift::FrontendObserver*, swift::UnifiedStatsReporter*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4e9f76)
// #26 0x00000000004e7749 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4e7749)
// #27 0x0000000000487e21 main (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x487e21)
// #28 0x00007f1ffa24c52b __libc_start_main (/lib/x86_64-linux-gnu/libc.so.6+0x2352b)
// #29 0x0000000000487a6a _start (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x487a6a)
// /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/test-linux-x86_64/AutoDiff/generated/Output/generated0002.swift.script: line 1: 18696 Aborted                 /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift -frontend -target x86_64-unknown-linux-gnu -module-cache-path '/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/swift-test-results/x86_64-unknown-linux-gnu/clang-module-cache' -swift-version 4 -ignore-module-source-info -typo-correction-limit 10 -O -emit-ir /usr/local/google/home/marcrasi/swift-base/swift/test/AutoDiff/generated/generated0002.swift

