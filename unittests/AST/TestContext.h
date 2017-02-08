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
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"

namespace swift {
namespace unittest {

/// Helper class used to set the LangOpts target before initializing the
/// ASTContext.
///
/// \see TestContext
class TestContextBase {
public:
  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diags;

  TestContextBase() : Diags(SourceMgr) {
    LangOpts.Target = llvm::Triple(llvm::sys::getProcessTriple());
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
  ASTContext Ctx;

  TestContext(ShouldDeclareOptionalTypes optionals = DoNotDeclareOptionalTypes);

  template <typename Nominal>
  Nominal *makeNominal(StringRef name,
                       GenericParamList *genericParams = nullptr) {
    auto result = new (Ctx) Nominal(SourceLoc(), Ctx.getIdentifier(name),
                                    SourceLoc(), /*inherited*/{},
                                    genericParams, FileForLookups);
    result->setAccessibility(Accessibility::Internal);
    return result;
  }
};

} // end namespace unittest
} // end namespace swift
