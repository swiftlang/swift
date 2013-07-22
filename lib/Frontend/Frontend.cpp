//===-- Frontend.cpp - frontend utility methods ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains utility methods for parsing and performing semantic
// on modules.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

swift::CompilerInvocation::CompilerInvocation()
    : DriverDiagnostics(DriverDiagsSourceMgr) {
  TargetTriple = llvm::sys::getDefaultTargetTriple();
}

std::string swift::CompilerInvocation::getRuntimeIncludePath() const {
  llvm::SmallString<128> LibPath(MainExecutablePath);
  llvm::sys::path::remove_filename(LibPath); // Remove /swift
  llvm::sys::path::remove_filename(LibPath); // Remove /bin
  llvm::sys::path::append(LibPath, "lib", "swift");
  return LibPath.str();
}

void swift::CompilerInstance::createSILModule() {
  TheSILModule.reset(SILModule::createEmptyModule(getASTContext()));
}

void swift::CompilerInstance::setup() {
  for (auto DC : Invocation->getDiagnosticConsumers())
    Diagnostics.addConsumer(*DC);

  Context.reset(new ASTContext(Invocation->getLangOptions(), SourceMgr, Diagnostics));

  // Give the context the list of search paths to use for modules.
  Context->ImportSearchPaths = Invocation->getImportSearchPaths();
  Context->addModuleLoader(SourceLoader::create(*Context));
  Context->addModuleLoader(SerializedModuleLoader::create(*Context));

  // If the user has specified an SDK, wire up the Clang module importer
  // and point it at that SDK.
  if (!Invocation->getSDKPath().empty()) {
    auto ImporterCtor = Invocation->getClangImporterCtor();
    auto clangImporter =
        ImporterCtor(*Context, Invocation->getSDKPath(),
                     Invocation->getTargetTriple(),
                     Invocation->getClangModuleCachePath(),
                     Invocation->getImportSearchPaths());
    if (!clangImporter)
      return; // FIXME: error reporting

    Context->addModuleLoader(clangImporter, /*isClang*/true);
  }

  // Add the runtime include path (which contains swift.swift)
  Context->ImportSearchPaths.push_back(Invocation->getRuntimeIncludePath());

  assert(Lexer::isIdentifier(Invocation->getModuleName()));

  if (Invocation->getTUKind() == TranslationUnit::SIL)
    createSILModule();
}

void swift::CompilerInstance::doIt() {
  const TranslationUnit::TUKind Kind = Invocation->getTUKind();
  Component *Comp = new (Context->Allocate<Component>(1)) Component();
  Identifier ID = Context->getIdentifier(Invocation->getModuleName());
  TU = new (*Context) TranslationUnit(ID, Comp, *Context, Kind);
  Context->LoadedModules[ID.str()] = TU;

  TU->HasBuiltinModuleAccess = Invocation->getParseStdlib();

  // If we're in SIL mode, don't auto import any libraries.
  // Also don't perform auto import if we are not going to do semantic
  // analysis.
  if (Kind != TranslationUnit::SIL && !Invocation->getParseOnly())
    performAutoImport(TU);

  if (Kind == TranslationUnit::Library) {
    // Parse all of the files into one big translation unit.
    for (auto &BufferID : BufferIDs) {
      bool Done;
      parseIntoTranslationUnit(TU, BufferID, &Done, nullptr, &TheParser);
      assert(Done && "Parser returned early?");
      (void) Done;
      performDelayedParsing(TU, TheParser.get(),
                            Invocation->getCodeCompletionFactory());
      TheParser.reset(nullptr);
    }

    // Finally, if enabled, type check the whole thing in one go.
    if (!Invocation->getParseOnly())
      performTypeChecking(TU);
    return;
  }

  // This may only be a main module or SIL, which requires pumping the parser.
  assert(Kind == TranslationUnit::Main || Kind == TranslationUnit::SIL);
  assert(BufferIDs.size() == 1 && "This mode only allows one input");
  unsigned BufferID = BufferIDs[0];

  SILParserState SILContext(TheSILModule.get());

  unsigned CurTUElem = 0;
  bool Done;
  do {
    // Pump the parser multiple times if necessary.  It will return early
    // after parsing any top level code in a main module, or in SIL mode when
    // there are chunks of swift decls (e.g. imports and types) interspersed
    // with 'sil' definitions.
    parseIntoTranslationUnit(TU, BufferID, &Done,
                             TheSILModule ? &SILContext : nullptr,
                             &TheParser);
    if (!Invocation->getParseOnly())
      performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Decls.size();
  } while (!Done);
  performDelayedParsing(TU, TheParser.get(),
                        Invocation->getCodeCompletionFactory());
}

