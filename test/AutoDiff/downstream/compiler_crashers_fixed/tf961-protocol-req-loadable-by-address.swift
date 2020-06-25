// RUN: %target-swift-frontend -emit-ir %s
// REQUIRES: asserts

public protocol Protocol00023: Differentiable {
  @differentiable
  func requirement00024(_ arg00026: Float) -> Float
}

public extension Protocol00023 {
  @differentiable
  func requirement00024(_ arg00026: Float) -> Float {
    return 0
  }
}

public struct Struct00042: Protocol00023 {
  public var field00043: Int
  public var field00045: Float
  public var field00046: Int
}

public struct Struct00063 {
  public var field00064: Struct00042
  public var field00065: Struct00042
}

// swift: /usr/local/google/home/marcrasi/swift-base/swift/lib/IRGen/LoadableByAddress.cpp:104: bool isLargeLoadableType(swift::GenericEnvironment *, swift::SILType, irgen::IRGenModule &): Assertion `GenericEnv && "Expected a GenericEnv"' failed.
// Stack dump:
// 0.	Program arguments: /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift -frontend -target x86_64-unknown-linux-gnu -module-cache-path /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/swift-test-results/x86_64-unknown-linux-gnu/clang-module-cache -swift-version 4 -ignore-module-source-info -typo-correction-limit 10 -emit-ir /usr/local/google/home/marcrasi/swift-base/swift/test/AutoDiff/generated/generated0001.swift
// 1.	Swift version 5.1.1-dev (LLVM 6e04008c7f, Swift 439808dd48)
// 2.	While running pass #192 SILModuleTransform "LoadableByAddress".
//  #0 0x0000000004bebbc4 PrintStackTraceSignalHandler(void*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4bebbc4)
//  #1 0x0000000004be97de llvm::sys::RunSignalHandlers() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4be97de)
//  #2 0x0000000004bebe78 SignalHandler(int) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4bebe78)
//  #3 0x00007f276b2e63a0 __restore_rt (/lib/x86_64-linux-gnu/libpthread.so.0+0x123a0)
//  #4 0x00007f276a41dcfb raise (/lib/x86_64-linux-gnu/libc.so.6+0x36cfb)
//  #5 0x00007f276a4088ad abort (/lib/x86_64-linux-gnu/libc.so.6+0x218ad)
//  #6 0x00007f276a40877f (/lib/x86_64-linux-gnu/libc.so.6+0x2177f)
//  #7 0x00007f276a416542 (/lib/x86_64-linux-gnu/libc.so.6+0x2f542)
//  #8 0x000000000058185a isLargeLoadableType(swift::GenericEnvironment*, swift::SILType, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x58185a)
//  #9 0x0000000000580b43 LargeSILTypeMapper::getNewSILType(swift::GenericEnvironment*, swift::SILType, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x580b43)
// #10 0x00000000005819d3 LargeSILTypeMapper::getNewTupleType(swift::GenericEnvironment*, swift::irgen::IRGenModule&, swift::SILType const&, swift::SILType const&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x5819d3)
// #11 0x0000000000580b6d LargeSILTypeMapper::getNewSILType(swift::GenericEnvironment*, swift::SILType, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x580b6d)
// #12 0x00000000005806cc LargeSILTypeMapper::shouldTransformResults(swift::GenericEnvironment*, swift::CanTypeWrapper<swift::SILFunctionType>, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x5806cc)
// #13 0x00000000005804ec LargeSILTypeMapper::shouldTransformFunctionType(swift::GenericEnvironment*, swift::CanTypeWrapper<swift::SILFunctionType>, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x5804ec)
// #14 0x0000000000580bf5 LargeSILTypeMapper::getNewSILType(swift::GenericEnvironment*, swift::SILType, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x580bf5)
// #15 0x0000000000580e50 LargeSILTypeMapper::getNewResults(swift::GenericEnvironment*, swift::CanTypeWrapper<swift::SILFunctionType>, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x580e50)
// #16 0x00000000005810e0 LargeSILTypeMapper::getNewSILFunctionType(swift::GenericEnvironment*, swift::CanTypeWrapper<swift::SILFunctionType>, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x5810e0)
// #17 0x0000000000584e71 (anonymous namespace)::LoadableByAddress::run() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x584e71)
// #18 0x00000000009941ef swift::SILPassManager::runModulePass(unsigned int) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x9941ef)
// #19 0x0000000000994c3a swift::SILPassManager::execute() (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x994c3a)
// #20 0x000000000056892b swift::SILPassManager::executePassPipelinePlan(swift::SILPassPipelinePlan const&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x56892b)
// #21 0x0000000000568694 runIRGenPreparePasses(swift::SILModule&, swift::irgen::IRGenModule&) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x568694)
// #22 0x00000000005667b0 performIRGeneration(swift::IRGenOptions&, swift::ModuleDecl*, std::unique_ptr<swift::SILModule, std::default_delete<swift::SILModule> >, llvm::StringRef, swift::PrimarySpecificPaths const&, llvm::LLVMContext&, swift::SourceFile*, llvm::GlobalVariable**) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x5667b0)
// #23 0x00000000005650ae swift::performIRGeneration(swift::IRGenOptions&, swift::ModuleDecl*, std::unique_ptr<swift::SILModule, std::default_delete<swift::SILModule> >, llvm::StringRef, swift::PrimarySpecificPaths const&, llvm::LLVMContext&, llvm::ArrayRef<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, llvm::GlobalVariable**) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x5650ae)
// #24 0x00000000004f4215 performCompileStepsPostSILGen(swift::CompilerInstance&, swift::CompilerInvocation&, std::unique_ptr<swift::SILModule, std::default_delete<swift::SILModule> >, bool, llvm::PointerUnion<swift::ModuleDecl*, swift::SourceFile*>, swift::PrimarySpecificPaths const&, bool, int&, swift::FrontendObserver*, swift::UnifiedStatsReporter*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4f4215)
// #25 0x00000000004e9f76 performCompile(swift::CompilerInstance&, swift::CompilerInvocation&, llvm::ArrayRef<char const*>, int&, swift::FrontendObserver*, swift::UnifiedStatsReporter*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4e9f76)
// #26 0x00000000004e7749 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x4e7749)
// #27 0x0000000000487e21 main (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x487e21)
// #28 0x00007f276a40a52b __libc_start_main (/lib/x86_64-linux-gnu/libc.so.6+0x2352b)
// #29 0x0000000000487a6a _start (/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x487a6a)
// /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/test-linux-x86_64/AutoDiff/generated/Output/generated0001.swift.script: line 1: 18425 Aborted                 /usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift -frontend -target x86_64-unknown-linux-gnu -module-cache-path '/usr/local/google/home/marcrasi/swift-base/build/Ninja-ReleaseAssert/swift-linux-x86_64/swift-test-results/x86_64-unknown-linux-gnu/clang-module-cache' -swift-version 4 -ignore-module-source-info -typo-correction-limit 10 -emit-ir /usr/local/google/home/marcrasi/swift-base/swift/test/AutoDiff/generated/generated0001.swift

