//===--- SemaFixture.cpp - Helper for setting up Sema context --------------===//
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

#include "SemaFixture.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParseRequests.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"

using namespace swift;
using namespace swift::unittest;

SemaTest::SemaTest()
    : Context(*ASTContext::get(LangOpts, TypeCheckerOpts, SearchPathOpts,
                               ClangImporterOpts, SourceMgr, Diags)) {
  registerParseRequestFunctions(Context.evaluator);
  registerTypeCheckerRequestFunctions(Context.evaluator);
  auto stdlibID = Context.getIdentifier(STDLIB_NAME);
  auto *module = ModuleDecl::create(stdlibID, Context);
  Context.addLoadedModule(module);

  FileForLookups = new (Context) SourceFile(*module, SourceFileKind::Library,
                                            /*buffer*/ None);
  module->addFile(*FileForLookups);

  DC = module;
}
