//===--- ASTGen.h -----------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#ifdef __cplusplus
extern "C" {
#endif

void *_Nonnull swift_ASTGen_createQueuedDiagnostics();
void swift_ASTGen_destroyQueuedDiagnostics(void *_Nonnull queued);
void swift_ASTGen_addQueuedSourceFile(
    void *_Nonnull queuedDiagnostics, ssize_t bufferID,
    void *_Nonnull sourceFile, const uint8_t *_Nonnull displayNamePtr,
    intptr_t displayNameLength, ssize_t parentID, ssize_t positionInParent);
void swift_ASTGen_addQueuedDiagnostic(
    void *_Nonnull queued, const char *_Nonnull text, ptrdiff_t textLength,
    BridgedDiagnosticSeverity severity, const void *_Nullable sourceLoc,
    const void *_Nullable *_Nullable highlightRanges,
    ptrdiff_t numHighlightRanges);
void swift_ASTGen_renderQueuedDiagnostics(
    void *_Nonnull queued, ssize_t contextSize, ssize_t colorize,
    BridgedStringRef *_Nonnull renderedString);

// FIXME: Hack because we cannot easily get to the already-parsed source
// file from here. Fix this egregious oversight!
void *_Nullable swift_ASTGen_parseSourceFile(const char *_Nonnull buffer,
                                             size_t bufferLength,
                                             const char *_Nonnull moduleName,
                                             const char *_Nonnull filename,
                                             void *_Nullable ctx);
void swift_ASTGen_destroySourceFile(void *_Nonnull sourceFile);

void *_Nullable swift_ASTGen_resolveMacroType(const void *_Nonnull macroType);
void swift_ASTGen_destroyMacro(void *_Nonnull macro);

void swift_ASTGen_freeBridgedString(BridgedStringRef);

void *_Nonnull swift_ASTGen_resolveExecutableMacro(
    const char *_Nonnull moduleName, const char *_Nonnull typeName,
    void *_Nonnull opaquePluginHandle);
void swift_ASTGen_destroyExecutableMacro(void *_Nonnull macro);

ptrdiff_t swift_ASTGen_checkMacroDefinition(
    void *_Nonnull diagEngine, void *_Nonnull sourceFile,
    const void *_Nonnull macroSourceLocation,
    BridgedStringRef *_Nonnull expansionSourceOutPtr,
    ptrdiff_t *_Nullable *_Nonnull replacementsPtr,
    ptrdiff_t *_Nonnull numReplacements);
void swift_ASTGen_freeExpansionReplacements(
    ptrdiff_t *_Nullable replacementsPtr, ptrdiff_t numReplacements);

ptrdiff_t swift_ASTGen_expandFreestandingMacro(
    void *_Nonnull diagEngine, const void *_Nonnull macro, uint8_t externalKind,
    const char *_Nonnull discriminator, uint8_t rawMacroRole,
    void *_Nonnull sourceFile, const void *_Nullable sourceLocation,
    BridgedStringRef *_Nonnull evaluatedSourceOut);

ptrdiff_t swift_ASTGen_expandAttachedMacro(
    void *_Nonnull diagEngine, const void *_Nonnull macro, uint8_t externalKind,
    const char *_Nonnull discriminator, const char *_Nonnull qualifiedType,
    const char *_Nonnull conformances, uint8_t rawMacroRole,
    void *_Nonnull customAttrSourceFile,
    const void *_Nullable customAttrSourceLocation,
    void *_Nonnull declarationSourceFile,
    const void *_Nullable declarationSourceLocation,
    void *_Nullable parentDeclSourceFile,
    const void *_Nullable parentDeclSourceLocation,
    BridgedStringRef *_Nonnull evaluatedSourceOut);

bool swift_ASTGen_initializePlugin(void *_Nonnull handle,
                                   void *_Nullable diagEngine);
void swift_ASTGen_deinitializePlugin(void *_Nonnull handle);
bool swift_ASTGen_pluginServerLoadLibraryPlugin(
    void *_Nonnull handle, const char *_Nonnull libraryPath,
    const char *_Nonnull moduleName, BridgedStringRef *_Nullable errorOut);

#ifdef __cplusplus
}
#endif
