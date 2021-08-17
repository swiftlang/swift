// RUN: %empty-directory(%t)
// RUN: not --crash %target-build-swift -emit-module -module-name pr32302 -emit-module-path %t/pr32302.swiftmodule -swift-version 5 -c %S/pr32302-autodiff-generictypeparamdecl-has-incorrect-depth.swift -Xfrontend -requirement-machine=off
// XFAIL: *

// pr32302 / pr32343 / pr38745 : reproduce assert with _Differentiation where
// ASTVerifier.cpp asserts "GenericTypeParamDecl has incorrect depth"

import _Differentiation

public protocol Layer: Differentiable {
  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  var differentiableVectorView: TangentVector { get }
  
  @differentiable(reverse)
  func callAsFunction(_ input: Input) -> Output
}

extension Differentiable {
  @differentiable(reverse)
  public func sequenced<L1: Layer, L2: Layer>(through l1: L1, _ l2: L2) -> L2.Output
  where L1.Input == Self, L1.Output == L2.Input {
    let o1 = l1(self)
    return l2(o1)
  }
}

// GenericTypeParamDecl has incorrect depth
// Please submit a bug report (https://swift.org/contributing/#reporting-bugs) and include the project and the crash backtrace.
// Stack dump:
// 0.	Program arguments: /work/software/swift-stocktoolchain/build/ds/swift-linux-x86_64/bin/swift-frontend -frontend -merge-modules -emit-module /tmp/pr32302-autodiff-generictypeparamdecl-has-incorrect-depth-acc95c.swiftmodule -parse-as-library -disable-diagnostic-passes -disable-sil-perf-optzns -target x86_64-unknown-linux-gnu -warn-on-potentially-unavailable-enum-case -disable-objc-interop -module-cache-path /work/software/swift-stocktoolchain/build/ds/swift-linux-x86_64/swift-test-results/x86_64-unknown-linux-gnu/clang-module-cache -swift-version 5 -define-availability "SwiftStdlib 5.5:macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0" -requirement-machine=off -emit-module-doc-path /work/software/swift-stocktoolchain/build/ds/swift-linux-x86_64/test-linux-x86_64/AutoDiff/compiler_crashers/Output/pr32302-autodiff-generictypeparamdecl-has-incorrect-depth.swift.tmp/pr32302.swiftdoc -emit-module-source-info-path /work/software/swift-stocktoolchain/build/ds/swift-linux-x86_64/test-linux-x86_64/AutoDiff/compiler_crashers/Output/pr32302-autodiff-generictypeparamdecl-has-incorrect-depth.swift.tmp/pr32302.swiftsourceinfo -module-name pr32302 -o /work/software/swift-stocktoolchain/build/ds/swift-linux-x86_64/test-linux-x86_64/AutoDiff/compiler_crashers/Output/pr32302-autodiff-generictypeparamdecl-has-incorrect-depth.swift.tmp/pr32302.swiftmodule
// 1.	Swift version 5.6-dev (LLVM ba0b85f590c1ba2, Swift 319b3e64aaeb252)
// 2.	Compiling with the current language version
// 3.	While verifying GenericTypeParamDecl 'τ_1_0' (in module 'pr32302')
//  #0 0x000000000632fb13 llvm::sys::PrintStackTrace(llvm::raw_ostream&, int) /work/software/swift-stocktoolchain/llvm-project/llvm/lib/Support/Unix/Signals.inc:563:13
//  #1 0x000000000632dda0 llvm::sys::RunSignalHandlers() /work/software/swift-stocktoolchain/llvm-project/llvm/lib/Support/Signals.cpp:72:18
//  #2 0x000000000632fe95 SignalHandler(int) /work/software/swift-stocktoolchain/llvm-project/llvm/lib/Support/Unix/Signals.inc:0:3
//  #3 0x00007ffbfc0ab3c0 __restore_rt (/lib/x86_64-linux-gnu/libpthread.so.0+0x153c0)
//  #4 0x00007ffbfbb3618b raise (/lib/x86_64-linux-gnu/libc.so.6+0x4618b)
//  #5 0x00007ffbfbb15859 abort (/lib/x86_64-linux-gnu/libc.so.6+0x25859)
//  #6 0x0000000002417d95 llvm::MutableArrayRef<swift::GenericTypeParamDecl*>::operator[](unsigned long) const /work/software/swift-stocktoolchain/llvm-project/llvm/include/llvm/ADT/ArrayRef.h:425:7
//  #7 0x0000000002417d95 (anonymous namespace)::Verifier::verifyCheckedAlways(swift::GenericTypeParamDecl*) /work/software/swift-stocktoolchain/swift/lib/AST/ASTVerifier.cpp:2866:11
//  #8 0x0000000002417d95 swift::GenericTypeParamDecl* (anonymous namespace)::Verifier::dispatchVisitPost<swift::GenericTypeParamDecl*>(swift::GenericTypeParamDecl*) /work/software/swift-stocktoolchain/swift/lib/AST/ASTVerifier.cpp:426:9
//  #9 0x0000000002417d95 (anonymous namespace)::Verifier::walkToDeclPost(swift::Decl*) /work/software/swift-stocktoolchain/swift/include/swift/AST/DeclNodes.def:159:7
// #10 0x00000000024214c7 (anonymous namespace)::Traversal::doIt(swift::Decl*) /work/software/swift-stocktoolchain/swift/lib/AST/ASTWalker.cpp:1275:12
// #11 0x0000000002421433 swift::Decl::walk(swift::ASTWalker&) /work/software/swift-stocktoolchain/swift/lib/AST/ASTWalker.cpp:1909:3
// #12 0x000000000240c6dd swift::verify(swift::Decl*) /work/software/swift-stocktoolchain/swift/lib/AST/ASTVerifier.cpp:3761:1
// #13 0x0000000001285157 swift::ModuleFile::verify() const /work/software/swift-stocktoolchain/swift/lib/Serialization/ModuleFile.cpp:1244:3
// #14 0x00000000011ead9a swift::SerializedModuleLoaderBase::verifyAllModules() /work/software/swift-stocktoolchain/swift/lib/Serialization/SerializedModuleLoader.cpp:1268:39
// #15 0x000000000236546a swift::ASTContext::verifyAllLoadedModules() const /work/software/swift-stocktoolchain/swift/lib/AST/ASTContext.cpp:1794:21
// #16 0x000000000058259d performEndOfPipelineActions(swift::CompilerInstance&) /work/software/swift-stocktoolchain/swift/lib/FrontendTool/FrontendTool.cpp:933:37
// #17 0x000000000057faa7 performCompile(swift::CompilerInstance&, int&, swift::FrontendObserver*) /work/software/swift-stocktoolchain/swift/lib/FrontendTool/FrontendTool.cpp:1252:10
// #18 0x000000000057ef21 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) /work/software/swift-stocktoolchain/swift/lib/FrontendTool/FrontendTool.cpp:2154:8
// #19 0x00000000004b36e7 run_driver(llvm::StringRef, llvm::ArrayRef<char const*>) /work/software/swift-stocktoolchain/swift/lib/DriverTool/driver.cpp:196:7
// #20 0x00000000004b3312 swift::mainEntry(int, char const**) /work/software/swift-stocktoolchain/swift/lib/DriverTool/driver.cpp:402:5
// #21 0x00000000004b2c72 main /work/software/swift-stocktoolchain/swift/tools/driver/driver.cpp:20:3
// #22 0x00007ffbfbb170b3 __libc_start_main (/lib/x86_64-linux-gnu/libc.so.6+0x270b3)
// #23 0x00000000004b2b7e _start (/work/software/swift-stocktoolchain/build/ds/swift-linux-x86_64/bin/swift-frontend+0x4b2b7e)
