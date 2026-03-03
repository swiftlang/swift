//===--- CrashReduceTests.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@testable import CrashReduce
import Testing

func assertShortSig(
  _ expected: String, expectedNamespace: String? = nil,
  _ sym: String, sourceLocation: SourceLocation = #_sourceLocation
) throws {
  let short = try SymbolParser.parse(sym).shortSignature
  #expect(expected == short.symbol, sourceLocation: sourceLocation)
  if let expectedNamespace {
    #expect(expectedNamespace == short.namespace, sourceLocation: sourceLocation)
  }
}

func assertShortSig(
  _ expected: String, crash: String,
  sourceLocation: SourceLocation = #_sourceLocation
) {
  guard let crashLog = CrashLog(from: crash),
  let short = crashLog.signature.short else {
    Issue.record("failed to parse crash", sourceLocation: sourceLocation)
    return
  }
  #expect(expected == short.symbol, sourceLocation: sourceLocation)
}

@Suite
struct ShortSignatureTests {
  @Test
  func testShortSignature() throws {
    try assertShortSig("getTypeForSymbolRange", """
      getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)
      """)

    try assertShortSig("DefaultAndMaxAccessLevelRequest::cacheResult", """
      swift::DefaultAndMaxAccessLevelRequest::cacheResult(std::__1::pair<swift::AccessLevel, swift::AccessLevel>) const
      """)

    try assertShortSig("simplifyLocator", """
      swift::constraints::simplifyLocator(swift::ASTNode&, llvm::ArrayRef<swift::constraints::ConstraintLocator::PathElement>&, swift::SourceRange&)
      """)

    try assertShortSig("ExprRewriter::coerceToType", """
      (anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)
      """)

    try assertShortSig("TypeResolution::applyUnboundGenericArguments", """
      swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>) const::$_0 const>(long, swift::SubstitutableType*)
      """)

    try assertShortSig("AssociatedTypeInference::getPotentialTypeWitnessesByMatchingTypes::MatchVisitor", """
      swift::CanTypeVisitor<swift::TypeMatcher<(anonymous namespace)::AssociatedTypeInference::getPotentialTypeWitnessesByMatchingTypes(swift::ValueDecl*, swift::ValueDecl*)::MatchVisitor>::MatchVisitor, bool, swift::Type, swift::Type>::visit(swift::CanType, swift::Type, swift::Type)
      """)

    try assertShortSig("typeEraseOpenedArchetypes", """
      void llvm::function_ref<void (swift::Type)>::callback_fn<(anonymous namespace)::typeEraseOpenedArchetypes(swift::Type)::$_0>(long, swift::Type)
      """)

    try assertShortSig("DenseMapBase::try_emplace", """
      std::__1::pair<llvm::DenseMapIterator<swift::Identifier, unsigned int, llvm::DenseMapInfo<swift::Identifier, void>, llvm::detail::DenseMapPair<swift::Identifier, unsigned int>, false>, bool> llvm::DenseMapBase<llvm::SmallDenseMap<swift::Identifier, unsigned int, 8u, llvm::DenseMapInfo<swift::Identifier, void>, llvm::detail::DenseMapPair<swift::Identifier, unsigned int>>, swift::Identifier, unsigned int, llvm::DenseMapInfo<swift::Identifier, void>, llvm::detail::DenseMapPair<swift::Identifier, unsigned int>>::try_emplace<unsigned int const&>(swift::Identifier const&, unsigned int const&)
      """)

    try assertShortSig("diagnoseAttrWithRemovalFixIt", """
      swift::InFlightDiagnostic swift::diagnoseAttrWithRemovalFixIt<swift::Diag<>&>(swift::Decl const*, swift::DeclAttribute const*, swift::Diag<>&)
      """)

    try assertShortSig("performFrontend", """
      swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*)::$_1::operator()(int, bool) const
      """)

    try assertShortSig("deriveBodyDecodable_enum_init", """
      std::__1::__function::__func<deriveBodyDecodable_enum_init(swift::AbstractFunctionDecl*, void*)::$_0, std::__1::allocator<deriveBodyDecodable_enum_init(swift::AbstractFunctionDecl*, void*)::$_0>, std::__1::tuple<swift::EnumElementDecl*, swift::BraceStmt*> (swift::EnumElementDecl*, swift::EnumElementDecl*, llvm::ArrayRef<swift::VarDecl*>)>::operator()(swift::EnumElementDecl*&&, swift::EnumElementDecl*&&, llvm::ArrayRef<swift::VarDecl*>&&)
      """)

    try assertShortSig("MapVector::try_emplace", """
      std::__1::pair<std::__1::pair<swift::AnyFunctionRef, llvm::SmallVector<swift::PackExpansionExpr*, 1u>>*, bool> llvm::MapVector<swift::AnyFunctionRef, llvm::SmallVector<swift::PackExpansionExpr*, 1u>, llvm::DenseMap<swift::AnyFunctionRef, unsigned int, llvm::DenseMapInfo<swift::AnyFunctionRef, void>, llvm::detail::DenseMapPair<swift::AnyFunctionRef, unsigned int>>, llvm::SmallVector<std::__1::pair<swift::AnyFunctionRef, llvm::SmallVector<swift::PackExpansionExpr*, 1u>>, 0u>>::try_emplace<llvm::SmallVector<swift::PackExpansionExpr*, 1u>>(swift::AnyFunctionRef&&, llvm::SmallVector<swift::PackExpansionExpr*, 1u>&&)
      """)

    try assertShortSig("ModuleDependencyScanner::resolveSwiftImportsForModule", """
      std::__1::__function::__func<std::__1::__bind<swift::ModuleDependencyScanner::resolveSwiftImportsForModule(swift::ModuleDependencyID const&, swift::ModuleDependenciesCache&, llvm::SetVector<swift::ModuleDependencyID, std::__1::vector<swift::ModuleDependencyID, std::__1::allocator<swift::ModuleDependencyID>>, std::__1::set<swift::ModuleDependencyID, std::__1::less<swift::ModuleDependencyID>, std::__1::allocator<swift::ModuleDependencyID>>, 0u>&)::$_0&, swift::Identifier, bool>, std::__1::allocator<std::__1::__bind<swift::ModuleDependencyScanner::resolveSwiftImportsForModule(swift::ModuleDependencyID const&, swift::ModuleDependenciesCache&, llvm::SetVector<swift::ModuleDependencyID, std::__1::vector<swift::ModuleDependencyID, std::__1::allocator<swift::ModuleDependencyID>>, std::__1::set<swift::ModuleDependencyID, std::__1::less<swift::ModuleDependencyID>, std::__1::allocator<swift::ModuleDependencyID>>, 0u>&)::$_0&, swift::Identifier, bool>>, void ()>::~__func()
      """)

    try assertShortSig("dyn_cast", expectedNamespace: "llvm", """
      decltype(auto) llvm::dyn_cast<swift::ClassDecl, swift::NominalTypeDecl>(swift::NominalTypeDecl*)
      """)
  }

  @Test
  func testCrashLog() {
    assertShortSig(
      "MissingOptionalUnwrapFailure::diagnoseAsError",
      crash: """
      Assertion failed: (isa<To>(Val) && "cast<Ty>() argument of incompatible type!"), function cast, file Casting.h, line 578.
      Please submit a bug report (https://swift.org/contributing/#reporting-bugs) and include the crash backtrace.
      Stack dump:
      0.      Program arguments: /Users/hamish/src/swift-dev/build/Release/swift-macosx-arm64/bin/swift-frontend -typecheck /Users/hamish/src/swift-dev/swift/validation-test/compiler_crashers/8b32e8ab4ca6b321.swift -sdk /Users/hamish/src/MacOSX.sdk -debug-diagnostic-names -diagnostic-style=llvm
      1.      Swift version 6.3-dev (LLVM 2a87a9d6a0f9e19, Swift 24eef289da1e8bd)
      2.      Compiling with effective version 5.10
      3.      While evaluating request TypeCheckPrimaryFileRequest(source_file "/Users/hamish/src/swift-dev/swift/validation-test/compiler_crashers/8b32e8ab4ca6b321.swift")
      4.      While evaluating request TypeCheckFunctionBodyRequest(main.(file).subscript(_:).getter@/Users/hamish/src/swift-dev/swift/validation-test/compiler_crashers/8b32e8ab4ca6b321.swift:3:32)
      5.      While type-checking statement at [/Users/hamish/src/swift-dev/swift/validation-test/compiler_crashers/8b32e8ab4ca6b321.swift:3:32 - line:6:1] RangeText="{
        var b = a
        Float(b)
      "
      6.      While type-checking expression at [/Users/hamish/src/swift-dev/swift/validation-test/compiler_crashers/8b32e8ab4ca6b321.swift:5:3 - line:5:10] RangeText="Float(b"
      7.      While type-checking-target starting at /Users/hamish/src/swift-dev/swift/validation-test/compiler_crashers/8b32e8ab4ca6b321.swift:5:3
      Stack dump without symbol names (ensure you have llvm-symbolizer in your PATH or set the environment var `LLVM_SYMBOLIZER_PATH` to point to it):
      0  swift-frontend           0x000000010ad3fe6c llvm::sys::PrintStackTrace(llvm::raw_ostream&, int) + 56
      1  swift-frontend           0x000000010ad3dc1c llvm::sys::RunSignalHandlers() + 164
      2  swift-frontend           0x000000010ad40998 SignalHandler(int, __siginfo*, void*) + 340
      3  libsystem_platform.dylib 0x0000000183269764 _sigtramp + 56
      4  libsystem_pthread.dylib  0x000000018325f888 pthread_kill + 296
      5  libsystem_c.dylib        0x0000000183164850 abort + 124
      6  libsystem_c.dylib        0x0000000183163a84 err + 0
      7  swift-frontend           0x000000010aefc034 swift::constraints::RValueTreatedAsLValueFailure::diagnoseAsError() (.cold.1) + 0
      8  swift-frontend           0x00000001057e820c swift::constraints::RValueTreatedAsLValueFailure::diagnoseAsError() + 0
      9  swift-frontend           0x00000001057e77ec swift::constraints::MissingOptionalUnwrapFailure::diagnoseAsError() + 1908
      10 swift-frontend           0x00000001057c3efc swift::constraints::ForceOptional::diagnose(swift::constraints::Solution const&, bool) const + 68
      11 swift-frontend           0x00000001058439c8 swift::constraints::ConstraintSystem::diagnoseAmbiguityWithFixes(llvm::SmallVectorImpl<swift::constraints::Solution>&) + 4504
      12 swift-frontend           0x00000001058424ac swift::constraints::ConstraintSystem::salvage() + 432
      13 swift-frontend           0x00000001057a2484 swift::constraints::ConstraintSystem::solve(swift::constraints::SyntacticElementTarget&, swift::FreeTypeVariableBinding) + 236
      14 swift-frontend           0x000000010596f3d4 swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*) + 324
      15 swift-frontend           0x000000010596f234 swift::TypeChecker::typeCheckExpression(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*) + 196
      16 swift-frontend           0x000000010596f0fc swift::TypeChecker::typeCheckExpression(swift::Expr*&, swift::DeclContext*, swift::constraints::ContextualTypeInfo, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>) + 96
      17 swift-frontend           0x0000000105a528fc (anonymous namespace)::StmtChecker::typeCheckASTNode(swift::ASTNode&) + 224
      18 swift-frontend           0x0000000105a559d0 swift::ASTVisitor<(anonymous namespace)::StmtChecker, void, swift::Stmt*, void, void, void, void>::visit(swift::Stmt*) + 288
      19 swift-frontend           0x0000000105a54110 bool (anonymous namespace)::StmtChecker::typeCheckStmt<swift::BraceStmt>(swift::BraceStmt*&) + 136
      20 swift-frontend           0x0000000105a53b64 (anonymous namespace)::StmtChecker::typeCheckBody(swift::BraceStmt*&) + 32
      21 swift-frontend           0x0000000105a53930 swift::TypeCheckFunctionBodyRequest::evaluate(swift::Evaluator&, swift::AbstractFunctionDecl*) const + 1004
      22 swift-frontend           0x0000000105ec1c00 swift::TypeCheckFunctionBodyRequest::OutputType swift::Evaluator::getResultUncached<swift::TypeCheckFunctionBodyRequest, swift::TypeCheckFunctionBodyRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckFunctionBodyRequest>(swift::Evaluator&, swift::TypeCheckFunctionBodyRequest, swift::TypeCheckFunctionBodyRequest::OutputType)::'lambda'()>(swift::TypeCheckFunctionBodyRequest const&, swift::TypeCheckFunctionBodyRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckFunctionBodyRequest>(swift::Evaluator&, swift::TypeCheckFunctionBodyRequest, swift::TypeCheckFunctionBodyRequest::OutputType)::'lambda'())::'lambda'()::operator()() const + 76
      23 swift-frontend           0x0000000105ec1508 swift::TypeCheckFunctionBodyRequest::OutputType swift::Evaluator::getResultUncached<swift::TypeCheckFunctionBodyRequest, swift::TypeCheckFunctionBodyRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckFunctionBodyRequest>(swift::Evaluator&, swift::TypeCheckFunctionBodyRequest, swift::TypeCheckFunctionBodyRequest::OutputType)::'lambda'()>(swift::TypeCheckFunctionBodyRequest const&, swift::TypeCheckFunctionBodyRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckFunctionBodyRequest>(swift::Evaluator&, swift::TypeCheckFunctionBodyRequest, swift::TypeCheckFunctionBodyRequest::OutputType)::'lambda'()) + 216
      24 swift-frontend           0x0000000105e2f024 swift::AbstractFunctionDecl::getTypecheckedBody() const + 108
      25 swift-frontend           0x0000000105f76ff8 swift::SourceFile::typeCheckDelayedFunctions() + 92
      26 swift-frontend           0x0000000105aa2328 swift::TypeCheckPrimaryFileRequest::evaluate(swift::Evaluator&, swift::SourceFile*) const + 332
      27 swift-frontend           0x0000000105aa5dbc swift::TypeCheckPrimaryFileRequest::OutputType swift::Evaluator::getResultUncached<swift::TypeCheckPrimaryFileRequest, swift::TypeCheckPrimaryFileRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckPrimaryFileRequest>(swift::Evaluator&, swift::TypeCheckPrimaryFileRequest, swift::TypeCheckPrimaryFileRequest::OutputType)::'lambda'()>(swift::TypeCheckPrimaryFileRequest const&, swift::TypeCheckPrimaryFileRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckPrimaryFileRequest>(swift::Evaluator&, swift::TypeCheckPrimaryFileRequest, swift::TypeCheckPrimaryFileRequest::OutputType)::'lambda'())::'lambda'()::operator()() const + 72
      28 swift-frontend           0x0000000105aa56d0 swift::TypeCheckPrimaryFileRequest::OutputType swift::Evaluator::getResultUncached<swift::TypeCheckPrimaryFileRequest, swift::TypeCheckPrimaryFileRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckPrimaryFileRequest>(swift::Evaluator&, swift::TypeCheckPrimaryFileRequest, swift::TypeCheckPrimaryFileRequest::OutputType)::'lambda'()>(swift::TypeCheckPrimaryFileRequest const&, swift::TypeCheckPrimaryFileRequest::OutputType swift::evaluateOrDefault<swift::TypeCheckPrimaryFileRequest>(swift::Evaluator&, swift::TypeCheckPrimaryFileRequest, swift::TypeCheckPrimaryFileRequest::OutputType)::'lambda'()) + 204
      29 swift-frontend           0x0000000105aa21b4 swift::performTypeChecking(swift::SourceFile&) + 76
      30 swift-frontend           0x00000001048762d4 bool llvm::function_ref<bool (swift::SourceFile&)>::callback_fn<swift::CompilerInstance::performSema()::$_0>(long, swift::SourceFile&) + 16
      31 swift-frontend           0x000000010486ce3c swift::CompilerInstance::forEachFileToTypeCheck(llvm::function_ref<bool (swift::SourceFile&)>) + 156
      32 swift-frontend           0x000000010486cd80 swift::CompilerInstance::performSema() + 144
      33 swift-frontend           0x00000001045d4f30 withSemanticAnalysis(swift::CompilerInstance&, swift::FrontendObserver*, llvm::function_ref<bool (swift::CompilerInstance&)>, bool) + 60
      34 swift-frontend           0x00000001045c86b0 performCompile(swift::CompilerInstance&, int&, swift::FrontendObserver*, llvm::ArrayRef<char const*>) + 560
      35 swift-frontend           0x00000001045c60c4 swift::performFrontend(llvm::ArrayRef<char const*>, char const*, void*, swift::FrontendObserver*) + 2180
      36 swift-frontend           0x00000001043444f0 swift::mainEntry(int, char const**) + 3164
      37 dyld                     0x0000000182e95d54 start + 7184
      """
    )
  }
}
