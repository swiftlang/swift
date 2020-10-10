//===--- SemaFixture.h - Helper for setting up Sema context -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
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
#include "llvm/Support/Host.h"
#include "gtest/gtest.h"

namespace swift {
namespace unittest {

class SemaTestBase : public ::testing::Test {
public:
  LangOptions LangOpts;
  TypeCheckerOptions TypeCheckerOpts;
  SearchPathOptions SearchPathOpts;
  ClangImporterOptions ClangImporterOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diags;

  SemaTestBase() : Diags(SourceMgr) {
    LangOpts.Target = llvm::Triple(llvm::sys::getProcessTriple());
  }
};

/// Owns an ASTContext and the associated types.
class SemaTest : public SemaTestBase {
  SourceFile *FileForLookups;

public:
  ASTContext &Context;
  DeclContext *DC;

  SemaTest();
};

} // end namespace unittest
} // end namespace swift
