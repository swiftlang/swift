//===--- Bridging/ASTContextBridging.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTContextGlobalCache.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Bridging/BasicSwift.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: ASTContext
//===----------------------------------------------------------------------===//

Identifier BridgedASTContext_getIdentifier(BridgedASTContext cContext,
                                           BridgedStringRef cStr) {
  return cContext.unbridged().getIdentifier(cStr.unbridged());
}

Identifier BridgedASTContext__getIdentifier(BridgedASTContext cContext,
                                            BridgedStringRef cStr) {
  return cContext.unbridged().getIdentifier(cStr.unbridged());
}

Identifier BridgedASTContext_getDollarIdentifier(BridgedASTContext cContext,
                                                 size_t idx) {
  return cContext.unbridged().getDollarIdentifier(idx);
}

BridgedLangOptions BridgedASTContext_langOpts(BridgedASTContext cContext) {
  return cContext.unbridged().LangOpts;
}

unsigned BridgedASTContext::getMajorLanguageVersion() const {
  return unbridged().LangOpts.EffectiveLanguageVersion[0];
}

BridgedDiagnosticEngine BridgedASTContext::getDiags() const {
  return &unbridged().Diags;
}

bool BridgedASTContext_canImport(BridgedASTContext cContext,
                                 BridgedStringRef importPath,
                                 SourceLoc canImportLoc,
                                 BridgedCanImportVersion versionKind,
                                 const SwiftInt *_Nullable versionComponents,
                                 SwiftInt numVersionComponents) {
  // Map the version.
  llvm::VersionTuple version;
  switch (numVersionComponents) {
  case 0:
    break;
  case 1:
    version = llvm::VersionTuple(versionComponents[0]);
    break;
  case 2:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1]);
    break;
  case 3:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2]);
    break;
  case 4:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2], versionComponents[3]);
    break;
  default:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2], versionComponents[3],
                                 versionComponents[4]);
    break;
  }

  ImportPath::Module::Builder builder(cContext.unbridged(),
                                      importPath.unbridged(), /*separator=*/'.',
                                      canImportLoc);
  return cContext.unbridged().canImportModule(
      builder.get(), canImportLoc, version,
      versionKind == CanImportUnderlyingVersion);
}

bool BridgedASTContext_testCanImport(BridgedASTContext cContext,
                                     BridgedStringRef importPath,
                                     BridgedCanImportVersion versionKind,
                                     const SwiftInt *_Nullable versionComponents,
                                     SwiftInt numVersionComponents) {
  llvm::VersionTuple version;
  switch (numVersionComponents) {
  case 0:
    break;
  case 1:
    version = llvm::VersionTuple(versionComponents[0]);
    break;
  case 2:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1]);
    break;
  case 3:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2]);
    break;
  case 4:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2], versionComponents[3]);
    break;
  default:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2], versionComponents[3],
                                 versionComponents[4]);
    break;
  }

  ImportPath::Module::Builder builder(cContext.unbridged(),
                                      importPath.unbridged(), /*separator=*/'.',
                                      SourceLoc());
  return cContext.unbridged().testImportModule(
      builder.get(), version, versionKind == CanImportUnderlyingVersion);
}

BridgedAvailabilityMacroMap BridgedASTContext::getAvailabilityMacroMap() const {
  return &unbridged().getAvailabilityMacroMap();
}

void *BridgedASTContext_staticBuildConfiguration(BridgedASTContext cContext) {
  ASTContext &ctx = cContext.unbridged();
  void *staticBuildConfiguration = ctx.getGlobalCache().StaticBuildConfiguration;
  if (!staticBuildConfiguration) {
    staticBuildConfiguration =
        swift_Basic_createStaticBuildConfiguration(ctx.LangOpts);
    ctx.addCleanup([staticBuildConfiguration] {
      swift_Basic_freeStaticBuildConfiguration(staticBuildConfiguration);
    });
  }

  return staticBuildConfiguration;
}

BridgedMacroExpansionEnclosingDecl
BridgedASTContext_macroExpansionEnclosingDecl(BridgedASTContext cContext,
                                              SourceLoc location) {
  BridgedMacroExpansionEnclosingDecl none{nullptr, SourceLoc()};

  if (location.isInvalid())
    return none;

  ASTContext &ctx = cContext.unbridged();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto bufferID = sourceMgr.findBufferContainingLoc(location);
  auto *generatedInfo = sourceMgr.getGeneratedSourceInfo(bufferID);
  if (!generatedInfo)
    return none;

  // The declaration context recorded for the buffer is where the expansion
  // logically resides, which already accounts for the macro role, e.g. peer
  // macro expansions are recorded against the context of the declaration the
  // macro is attached to rather than that declaration.
  auto *dc = generatedInfo->declContext;
  if (!dc || dc->isModuleScopeContext())
    return none;

  auto *decl = dc->getAsDecl();
  if (!decl)
    decl = dc->getInnermostDeclarationDeclContext();
  if (!decl)
    return none;

  auto declLoc = decl->getStartLoc();
  if (declLoc.isInvalid())
    return none;

  auto *sourceFile =
      decl->getModuleContext()->getSourceFileContainingLocation(declLoc);
  if (!sourceFile)
    return none;

  auto *exportedSourceFile = sourceFile->getExportedSourceFile();
  if (!exportedSourceFile)
    return none;

  return {exportedSourceFile, declLoc};
}
