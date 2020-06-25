// RUN: %target-swift-frontend -c %S/Inputs/tf960-irgen-diff-witness-no-derivatives-other-module.swift %s -O -module-name main -num-threads 36
// REQUIRES: asserts

// TF-960: IRGen crash for uncanonicalized differentiability witnesses.
// This issue will become obsolete after TF-894, when the differentiation
// transform canonicalizes differentiability witnesses to have derivative
// functions and assertions are added to IRGen.

// Stack dump:
// ...
// 1.	Swift version 5.1.1-dev (Swift af915c09de)
// 0  swiftc                   0x0000000109acda65 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 37
// 1  swiftc                   0x0000000109acca68 llvm::sys::RunSignalHandlers() + 248
// 2  swiftc                   0x0000000109ace058 SignalHandler(int) + 264
// 3  libsystem_platform.dylib 0x00007fff728e4b5d _sigtramp + 29
// 4  libsystem_platform.dylib 0x00007ff639560230 _sigtramp + 3334977264
// 5  swiftc                   0x0000000105b316bb swift::irgen::IRGenerator::emitGlobalTopLevel() + 1307
// 6  swiftc                   0x0000000105bf2572 swift::performIRGeneration(swift::IRGenOptions&, swift::ModuleDecl*, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> >, llvm::StringRef, swift::PrimarySpecificPaths const&, llvm::LLVMContext&, llvm::ArrayRef<std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > >, llvm::GlobalVariable**) + 1682
