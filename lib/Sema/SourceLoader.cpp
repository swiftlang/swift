//===--- SourceLoader.cpp - Import .swift files as modules ----------------===//
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
///
/// \file
/// \brief A simple module loader that loads .swift source files.
///
//===----------------------------------------------------------------------===//

#include "swift/Sema/SourceLoader.h"
#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Module.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include <system_error>

using namespace swift;

// FIXME: Basically the same as SerializedModuleLoader.
using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;

static FileOrError findModule(ASTContext &ctx, StringRef moduleID,
                              SourceLoc importLoc) {
  llvm::SmallString<128> inputFilename;

  for (auto Path : ctx.SearchPathOpts.ImportSearchPaths) {
    inputFilename = Path;
    llvm::sys::path::append(inputFilename, moduleID);
    inputFilename.append(".swift");
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFile(inputFilename.str());

    // Return if we loaded a file
    if (FileBufOrErr)
      return FileBufOrErr;
    // Or if we get any error other than the file not existing
    auto err = FileBufOrErr.getError();
    if (err != std::errc::no_such_file_or_directory)
      return FileBufOrErr;
  }

  return make_error_code(std::errc::no_such_file_or_directory);
}

namespace {

/// Don't parse any function bodies except those that are transparent.
class SkipNonTransparentFunctions : public DelayedParsingCallbacks {
  bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                      AbstractFunctionDecl *AFD,
                                      const DeclAttributes &Attrs,
                                      SourceRange BodyRange) override {
    return Attrs.hasAttribute<TransparentAttr>();
  }
};

} // unnamed namespace

bool SourceLoader::canImportModule(std::pair<Identifier, SourceLoc> ID) {
  // Search the memory buffers to see if we can find this file on disk.
  FileOrError inputFileOrError = findModule(Ctx, ID.first.str(),
                                            ID.second);
  if (!inputFileOrError) {
    auto err = inputFileOrError.getError();
    if (err != std::errc::no_such_file_or_directory) {
      Ctx.Diags.diagnose(ID.second, diag::sema_opening_import,
                         ID.first, err.message());
    }

    return false;
  }
  return true;
}

ModuleDecl *SourceLoader::loadModule(SourceLoc importLoc,
                                     ArrayRef<std::pair<Identifier,
                                     SourceLoc>> path) {
  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];

  FileOrError inputFileOrError = findModule(Ctx, moduleID.first.str(),
                                            moduleID.second);
  if (!inputFileOrError) {
    auto err = inputFileOrError.getError();
    if (err != std::errc::no_such_file_or_directory) {
      Ctx.Diags.diagnose(moduleID.second, diag::sema_opening_import,
                         moduleID.first, err.message());
    }

    return nullptr;
  }
  std::unique_ptr<llvm::MemoryBuffer> inputFile =
    std::move(inputFileOrError.get());

  addDependency(inputFile->getBufferIdentifier());

  // Turn off debugging while parsing other modules.
  llvm::SaveAndRestore<bool> turnOffDebug(Ctx.LangOpts.DebugConstraintSolver,
                                          false);

  unsigned bufferID;
  if (auto BufID =
       Ctx.SourceMgr.getIDForBufferIdentifier(inputFile->getBufferIdentifier()))
    bufferID = BufID.getValue();
  else
    bufferID = Ctx.SourceMgr.addNewSourceBuffer(std::move(inputFile));

  auto *importMod = ModuleDecl::create(moduleID.first, Ctx);
  if (EnableResilience)
    importMod->setResilienceStrategy(ResilienceStrategy::Resilient);
  Ctx.LoadedModules[moduleID.first] = importMod;

  auto implicitImportKind = SourceFile::ImplicitModuleImportKind::Stdlib;
  if (!Ctx.getStdlibModule())
    implicitImportKind = SourceFile::ImplicitModuleImportKind::None;

  auto *importFile = new (Ctx) SourceFile(*importMod, SourceFileKind::Library,
                                          bufferID, implicitImportKind,
                                          Ctx.LangOpts.CollectParsedToken,
                                          Ctx.LangOpts.BuildSyntaxTree);
  importMod->addFile(*importFile);

  bool done;
  PersistentParserState persistentState;
  SkipNonTransparentFunctions delayCallbacks;
  parseIntoSourceFile(*importFile, bufferID, &done, nullptr, &persistentState,
                      SkipBodies ? &delayCallbacks : nullptr);
  assert(done && "Parser returned early?");
  (void)done;

  if (SkipBodies)
    performDelayedParsing(importMod, persistentState, nullptr);

  // FIXME: Support recursive definitions in immediate modes by making type
  // checking even lazier.
  if (SkipBodies)
    performNameBinding(*importFile);
  else
    performTypeChecking(*importFile, persistentState.getTopLevelContext(),
                        None);
  importMod->setHasResolvedImports();
  return importMod;
}

void SourceLoader::loadExtensions(NominalTypeDecl *nominal,
                                  unsigned previousGeneration) {
  // Type-checking the source automatically loads all extensions; there's
  // nothing to do here.
}
