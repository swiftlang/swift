//===--- ASTGen.h -----------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BRIDGING_ASTGEN_H
#define SWIFT_BRIDGING_ASTGEN_H

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
    void *_Nonnull queued, void *_Nonnull state,
    BridgedStringRef text,
    swift::DiagnosticKind severity,
    swift::SourceLoc sourceLoc,
    BridgedStringRef categoryName,
    BridgedStringRef documentationPath,
    const swift::CharSourceRange *_Nullable highlightRanges,
    ptrdiff_t numHighlightRanges,
    BridgedArrayRef /*BridgedFixIt*/ fixIts);
void swift_ASTGen_renderSingleDiagnostic(
    void *_Nonnull state,
    BridgedStringRef text,
    swift::DiagnosticKind severity,
    BridgedStringRef categoryName,
    BridgedStringRef documentationPath,
    ssize_t colorize,
    BridgedStringRef *_Nonnull renderedString);
void swift_ASTGen_renderQueuedDiagnostics(
    void *_Nonnull queued, ssize_t contextSize, ssize_t colorize,
    BridgedStringRef *_Nonnull renderedString);

void *_Nonnull swift_ASTGen_createPerFrontendDiagnosticState();
void swift_ASTGen_destroyPerFrontendDiagnosticState(void * _Nonnull state);
void swift_ASTGen_renderCategoryFootnotes(
    void * _Nonnull state, ssize_t colorize,
    BridgedStringRef *_Nonnull renderedString);

// FIXME: Hack because we cannot easily get to the already-parsed source
// file from here. Fix this egregious oversight!
void *_Nullable swift_ASTGen_parseSourceFile(BridgedStringRef buffer,
                                             BridgedStringRef moduleName,
                                             BridgedStringRef filename,
                                             void *_Nullable declContextPtr,
                                             BridgedGeneratedSourceFileKind);
void swift_ASTGen_destroySourceFile(void *_Nonnull sourceFile);

/// Check whether the given source file round-trips correctly. Returns 0 if
/// round-trip succeeded, non-zero otherwise.
int swift_ASTGen_roundTripCheck(void *_Nonnull sourceFile);

/// Emit parser diagnostics for given source file.. Returns non-zero if any
/// diagnostics were emitted.
int swift_ASTGen_emitParserDiagnostics(
    BridgedASTContext astContext,
    void *_Nonnull diagEngine, void *_Nonnull sourceFile, int emitOnlyErrors,
    int downgradePlaceholderErrorsToWarnings);

// Build AST nodes for the top-level entities in the syntax.
void swift_ASTGen_buildTopLevelASTNodes(
    BridgedDiagnosticEngine diagEngine, void *_Nonnull sourceFile,
    BridgedDeclContext declContext, BridgedNullableDecl attachedDecl,
    BridgedASTContext astContext, void *_Nonnull outputContext,
    void (*_Nonnull)(BridgedASTNode, void *_Nonnull));

BridgedFingerprint
swift_ASTGen_getSourceFileFingerprint(void *_Nonnull sourceFile,
                                      BridgedASTContext astContext);

void swift_ASTGen_freeBridgedString(BridgedStringRef);

// MARK: - Regex parsing

bool swift_ASTGen_lexRegexLiteral(const char *_Nonnull *_Nonnull curPtrPtr,
                                  const char *_Nonnull bufferEndPtr,
                                  bool mustBeRegex,
                                  BridgedNullableDiagnosticEngine diagEngine);

bool swift_ASTGen_parseRegexLiteral(
    BridgedStringRef inputPtr, size_t *_Nonnull versionOut,
    void *_Nonnull UnsafeMutableRawPointer, size_t captureStructureSize,
    BridgedRegexLiteralPatternFeatures *_Nonnull featuresOut,
    swift::SourceLoc diagLoc, BridgedDiagnosticEngine diagEngine);

void swift_ASTGen_freeBridgedRegexLiteralPatternFeatures(
    BridgedRegexLiteralPatternFeatures features);

void swift_ASTGen_getSwiftVersionForRegexPatternFeature(
    BridgedRegexLiteralPatternFeatureKind kind,
    BridgedSwiftVersion *_Nonnull versionOut);

void swift_ASTGen_getDescriptionForRegexPatternFeature(
    BridgedRegexLiteralPatternFeatureKind kind, BridgedASTContext astContext,
    BridgedStringRef *_Nonnull descriptionOut);

intptr_t swift_ASTGen_configuredRegions(
    BridgedASTContext astContext,
    void *_Nonnull sourceFile,
    BridgedIfConfigClauseRangeInfo *_Nullable *_Nonnull);
void swift_ASTGen_freeConfiguredRegions(
    BridgedIfConfigClauseRangeInfo *_Nullable regions, intptr_t numRegions);

bool swift_ASTGen_validateUnqualifiedLookup(
    void *_Nonnull sourceFile,
    BridgedASTContext astContext,
    swift::SourceLoc sourceLoc,
    bool finishInSequentialScope,
    BridgedArrayRef astScopeResultRef);

size_t
swift_ASTGen_virtualFiles(void *_Nonnull sourceFile,
                          BridgedVirtualFile *_Nullable *_Nonnull virtualFiles);
void swift_ASTGen_freeBridgedVirtualFiles(
    BridgedVirtualFile *_Nullable virtualFiles, size_t numFiles);

bool swift_ASTGen_parseAvailabilityMacroDefinition(
    BridgedASTContext ctx, BridgedDeclContext dc, BridgedDiagnosticEngine diags,
    BridgedStringRef buffer,
    BridgedAvailabilityMacroDefinition *_Nonnull outPtr);

void swift_ASTGen_freeAvailabilityMacroDefinition(
    BridgedAvailabilityMacroDefinition *_Nonnull definition);

#ifdef __cplusplus
}
#endif

#endif // SWIFT_BRIDGING_ASTGEN_H
