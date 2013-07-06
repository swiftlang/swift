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
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

/// \param SIL is non-null when we're parsing a .sil file instead of a .swift
/// file.
TranslationUnit*
swift::buildSingleTranslationUnit(ASTContext &Context,
                                  StringRef ModuleName,
                                  ArrayRef<unsigned> BufferIDs,
                                  bool ParseOnly,
                                  bool AllowBuiltinModule,
                                  TranslationUnit::TUKind Kind,
                                  SILModule *SIL) {
  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  Identifier ID = Context.getIdentifier(ModuleName);
  TranslationUnit *TU = new (Context) TranslationUnit(ID, Comp, Context, Kind);
  Context.LoadedModules[ID.str()] = TU;

  TU->HasBuiltinModuleAccess = AllowBuiltinModule;

  // If we're in SIL mode, don't auto import any libraries.
  // Also don't perform auto import if we are not going to do semantic
  // analysis.
  if (Kind != TranslationUnit::SIL && !ParseOnly)
    performAutoImport(TU);

  // If we have multiple source files, we must be building a module.  Parse each
  // file before type checking the union of them.
  if (BufferIDs.size() > 1) {
    assert(Kind == TranslationUnit::Library &&
           "Multiple file mode can't handle early returns from the parser");

    // Parse all of the files into one big translation unit.
    for (auto &BufferID : BufferIDs) {
      auto *Buffer = Context.SourceMgr.getMemoryBuffer(BufferID);

      unsigned BufferOffset = 0;
      parseIntoTranslationUnit(TU, BufferID, &BufferOffset);
      assert(BufferOffset == Buffer->getBufferSize() &&
             "Parser returned early?");
      (void)Buffer;
    }

    // Finally, if enabled, type check the whole thing in one go.
    if (!ParseOnly)
      performTypeChecking(TU);
    return TU;
  }

  // If there is only a single input file, it may be SIL or a main module,
  // which requires pumping the parser.
  assert(BufferIDs.size() == 1 && "This mode only allows one input");
  unsigned BufferID = BufferIDs[0];

  SILParserState SILContext(SIL);

  unsigned CurTUElem = 0;
  unsigned BufferOffset = 0;
  auto *Buffer = Context.SourceMgr.getMemoryBuffer(BufferID);
  do {
    // Pump the parser multiple times if necessary.  It will return early
    // after parsing any top level code in a main module, or in SIL mode when
    // there are chunks of swift decls (e.g. imports and types) interspersed
    // with 'sil' definitions.
    parseIntoTranslationUnit(TU, BufferID, &BufferOffset, 0,
                             SIL ? &SILContext : nullptr);
    if (!ParseOnly)
      performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Decls.size();
  } while (BufferOffset != Buffer->getBufferSize());

  return TU;
}

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
    auto clangImporter =
        ClangImporter::create(*Context, Invocation->getSDKPath(),
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
  TU = buildSingleTranslationUnit(*Context,
                                  Invocation->getModuleName(),
                                  BufferIDs,
                                  Invocation->getParseOnly(),
                                  Invocation->getParseStdlib(),
                                  Invocation->getTUKind(),
                                  TheSILModule.get());
}

