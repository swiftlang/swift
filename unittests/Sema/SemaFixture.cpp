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
#include "swift/AST/SourceFile.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"

using namespace swift;
using namespace swift::unittest;

using ModuleDecl = SourceFile::ImportedModuleDesc;

SemaTest::SemaTest()
    : Context(*ASTContext::get(LangOpts, TypeCheckerOpts, SearchPathOpts,
                               ClangImporterOpts, SourceMgr, Diags)) {
  INITIALIZE_LLVM();

  registerParseRequestFunctions(Context.evaluator);
  registerTypeCheckerRequestFunctions(Context.evaluator);

  Context.addModuleLoader(ImplicitSerializedModuleLoader::create(Context));
  Context.addModuleLoader(ClangImporter::create(Context), /*isClang=*/true);

  auto *stdlib = Context.getStdlibModule(/*loadIfAbsent=*/true);
  assert(stdlib && "Failed to load standard library");

  auto *module =
      ModuleDecl::create(Context.getIdentifier("SemaTests"), Context);

  MainFile = new (Context) SourceFile(*module, SourceFileKind::Main,
                                      /*buffer=*/None);

  auto stdlibImport =
      ModuleDesc({ImportPath::Access(), stdlib}, /*options=*/{});

  MainFile->setImports(stdlibImport);
  module->addFile(*MainFile);

  DC = module;
}
