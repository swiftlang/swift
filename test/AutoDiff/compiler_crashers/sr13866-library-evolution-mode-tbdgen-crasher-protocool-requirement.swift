// RUN: not --crash %target-swift-frontend -c -enable-library-evolution %s

// SR-13866: TBDGen crasher on protocol with differentiable requirement

import _Differentiation

public protocol P: Differentiable {
    @differentiable(wrt: self)
    func foo(_ input: Float) -> Float
}

// TBDGen duplicate symbol: ...
// Assertion failed: (false && "TBDGen symbol appears twice"), function addSymbolInternal, file .../swift/lib/TBDGen/TBDGen.cpp, line 79.
// Stack dump:
// 0.	Program arguments: swift-frontend -c test.swift -enable-library-evolution
// 1.	Swift version 5.3-dev (LLVM 618cb952e0f199a, Swift db830811093c5d2)
// 2.	While evaluating request PublicSymbolsRequest(Generate TBD for module test.test)
// 0  swift-frontend           0x000000010d5fb0e5 llvm::sys::PrintStackTrace(llvm::raw_ostream&) + 37
// 1  swift-frontend           0x000000010d5fa028 llvm::sys::RunSignalHandlers() + 248
// 2  swift-frontend           0x000000010d5fb6c6 SignalHandler(int) + 262
// 3  libsystem_platform.dylib 0x00007fff203dad7d _sigtramp + 29
// 4  libsystem_platform.dylib 000000000000000000 _sigtramp + 18446603339975250592
// 5  libsystem_c.dylib        0x00007fff202e9720 abort + 120
// 6  libsystem_c.dylib        0x00007fff202e89d6 err + 0
// 7  swift-frontend           0x000000010d76f5d7 swift::tbdgen::TBDGenVisitor::addSymbolInternal(llvm::StringRef, llvm::MachO::SymbolKind, swift::SymbolSource) (.cold.1) + 87
// 8  swift-frontend           0x00000001090e084f swift::tbdgen::TBDGenVisitor::addSymbolInternal(llvm::StringRef, llvm::MachO::SymbolKind, swift::SymbolSource) + 111
// 9  swift-frontend           0x00000001090e3287 swift::tbdgen::TBDGenVisitor::addSymbol(llvm::StringRef, swift::SymbolSource, llvm::MachO::SymbolKind) + 119
// 10 swift-frontend           0x00000001090e34f0 swift::tbdgen::TBDGenVisitor::addDispatchThunk(swift::SILDeclRef) + 272
// 11 swift-frontend           0x00000001090ea556 swift::SILWitnessVisitor<swift::tbdgen::TBDGenVisitor::visitProtocolDecl(swift::ProtocolDecl*)::WitnessVisitor>::addAutoDiffDerivativeMethodsIfRequired(swift::AbstractFunctionDecl*, swift::SILDeclRef::Kind) + 326
// 12 swift-frontend           0x00000001090ea086 swift::SILWitnessVisitor<swift::tbdgen::TBDGenVisitor::visitProtocolDecl(swift::ProtocolDecl*)::WitnessVisitor>::visitProtocolDecl(swift::ProtocolDecl*) + 934
// 13 swift-frontend           0x00000001090e786b swift::tbdgen::TBDGenVisitor::visitProtocolDecl(swift::ProtocolDecl*) + 283
// 14 swift-frontend           0x00000001090e6c82 swift::tbdgen::TBDGenVisitor::visit(swift::Decl*) + 274
// 15 swift-frontend           0x00000001090e7e7b swift::tbdgen::TBDGenVisitor::visitFile(swift::FileUnit*) + 283
// 16 swift-frontend           0x00000001090e817c swift::tbdgen::TBDGenVisitor::visit(swift::TBDGenDescriptor const&) + 620
// 17 swift-frontend           0x00000001090e88ca swift::PublicSymbolsRequest::evaluate(swift::Evaluator&, swift::TBDGenDescriptor) const + 250
// 18 swift-frontend           0x00000001090ef0ed swift::SimpleRequest<swift::PublicSymbolsRequest, std::__1::vector<std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >, std::__1::allocator<std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > > > (swift::TBDGenDescriptor), (swift::RequestFlags)1>::evaluateRequest(swift::PublicSymbolsRequest const&, swift::Evaluator&) + 77
// 19 swift-frontend           0x00000001090ebeee llvm::Expected<swift::PublicSymbolsRequest::OutputType> swift::Evaluator::getResultUncached<swift::PublicSymbolsRequest>(swift::PublicSymbolsRequest const&) + 494
// 20 swift-frontend           0x00000001090e8977 swift::getPublicSymbols(swift::TBDGenDescriptor) + 135
// 21 swift-frontend           0x0000000108d98481 swift::validateTBD(swift::ModuleDecl*, llvm::Module const&, swift::TBDGenOptions const&, bool) + 97
// 22 swift-frontend           0x0000000108d8362d performCompileStepsPostSILGen(swift::CompilerInstance&, std::__1::unique_ptr<swift::SILModule, std::__1::default_delete<swift::SILModule> >, llvm::PointerUnion<swift::ModuleDecl*, swift::SourceFile*>, swift::PrimarySpecificPaths const&, int&, swift::FrontendObserver*) + 2925
// 23 swift-frontend           0x0000000108d8289c performCompileStepsPostSema(swift::CompilerInstance&, int&, swift::FrontendObserver*) + 636
// 24 swift-frontend           0x0000000108d7ae31 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 4625
// 25 swift-frontend           0x0000000108d127fe main + 846
// 26 libdyld.dylib            0x00007fff203b1631 start + 1
// [1]    77664 abort      xcrun $SWIFT_NINJA_BUILD_DIR/bin/swift-frontend -c test.swift
