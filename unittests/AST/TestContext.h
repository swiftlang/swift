//===--- TestContext.h - Helper for setting up ASTContexts ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/SymbolGraphGen/SymbolGraphOptions.h"

#include "llvm/TargetParser/Host.h"

namespace swift {
namespace unittest {

/// Helper class used to set the LangOpts target before initializing the
/// ASTContext.
///
/// \see TestContext
class TestContextBase {
public:
  LangOptions LangOpts;
  TypeCheckerOptions TypeCheckerOpts;
  SILOptions SILOpts;
  SearchPathOptions SearchPathOpts;
  ClangImporterOptions ClangImporterOpts;
  symbolgraphgen::SymbolGraphOptions SymbolGraphOpts;
  CASOptions CASOpts;
  SerializationOptions SerializationOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diags;

  TestContextBase(llvm::Triple target) : Diags(SourceMgr) {
    LangOpts.Target = target;
  }
};

enum ShouldDeclareOptionalTypes : bool {
  DoNotDeclareOptionalTypes,
  DeclareOptionalTypes
};

/// Owns an ASTContext and the associated types.
class TestContext : public TestContextBase {
  SourceFile *FileForLookups;

public:
  ASTContext &Ctx;

  TestContext(
      ShouldDeclareOptionalTypes optionals = DoNotDeclareOptionalTypes,
      llvm::Triple target = llvm::Triple(llvm::sys::getProcessTriple()));

  TestContext(llvm::Triple target)
      : TestContext(DoNotDeclareOptionalTypes, target) {};

  template <typename Nominal>
  typename std::enable_if<!std::is_same<Nominal, swift::ClassDecl>::value,
                          Nominal *>::type
  makeNominal(StringRef name, GenericParamList *genericParams = nullptr) {
    auto result = new (Ctx) Nominal(SourceLoc(), Ctx.getIdentifier(name),
                                    SourceLoc(), /*inherited*/{},
                                    genericParams, FileForLookups);
    result->setAccess(AccessLevel::Internal);
    return result;
  }

  template <typename Nominal>
  typename std::enable_if<std::is_same<Nominal, swift::ClassDecl>::value,
                          swift::ClassDecl *>::type
  makeNominal(StringRef name, GenericParamList *genericParams = nullptr) {
    auto result = new (Ctx) ClassDecl(SourceLoc(), Ctx.getIdentifier(name),
                                      SourceLoc(), /*inherited*/{},
                                      genericParams, FileForLookups,
                                      /*isActor*/false);
    result->setAccess(AccessLevel::Internal);
    return result;
  }

};

} // end namespace unittest
} // end namespace swift
