//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ContextFinder.h"
#include "RefactoringActions.h"
#include "swift/AST/TypeCheckRequests.h"

using namespace swift::refactoring;

/// Retrieve the macro expansion buffer for the given macro expansion
/// expression.
static std::optional<unsigned>
getMacroExpansionBuffer(SourceManager &sourceMgr,
                        MacroExpansionExpr *expansion) {
  return evaluateOrDefault(
      expansion->getDeclContext()->getASTContext().evaluator,
      ExpandMacroExpansionExprRequest{expansion}, {});
}

/// Retrieve the macro expansion buffer for the given macro expansion
/// declaration.
static std::optional<unsigned>
getMacroExpansionBuffer(SourceManager &sourceMgr,
                        MacroExpansionDecl *expansion) {
  return evaluateOrDefault(expansion->getASTContext().evaluator,
                           ExpandMacroExpansionDeclRequest{expansion}, {});
}

/// Retrieve the macro expansion buffers for the given attached macro reference.
static llvm::SmallVector<unsigned, 2>
getMacroExpansionBuffers(MacroDecl *macro, const CustomAttr *attr, Decl *decl) {
  auto roles = macro->getMacroRoles() & getAttachedMacroRoles();
  if (!roles)
    return {};

  ASTContext &ctx = macro->getASTContext();
  llvm::SmallVector<unsigned, 2> allBufferIDs;
  if (roles.contains(MacroRole::Accessor)) {
    if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
      auto bufferIDs =
          evaluateOrDefault(ctx.evaluator, ExpandAccessorMacros{storage}, {});
      allBufferIDs.append(bufferIDs.begin(), bufferIDs.end());
    }
  }

  if (roles.contains(MacroRole::MemberAttribute)) {
    if (auto idc = dyn_cast<IterableDeclContext>(decl)) {
      for (auto memberDecl : idc->getAllMembers()) {
        auto bufferIDs = evaluateOrDefault(
            ctx.evaluator, ExpandMemberAttributeMacros{memberDecl}, {});
        allBufferIDs.append(bufferIDs.begin(), bufferIDs.end());
      }
    }
  }

  if (roles.contains(MacroRole::Member)) {
    auto bufferIDs = evaluateOrDefault(
        ctx.evaluator, ExpandSynthesizedMemberMacroRequest{decl}, {});
    allBufferIDs.append(bufferIDs.begin(), bufferIDs.end());
  }

  if (roles.contains(MacroRole::Peer)) {
    auto bufferIDs =
        evaluateOrDefault(ctx.evaluator, ExpandPeerMacroRequest{decl}, {});
    allBufferIDs.append(bufferIDs.begin(), bufferIDs.end());
  }

  if (roles.contains(MacroRole::Conformance) ||
      roles.contains(MacroRole::Extension)) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
      auto bufferIDs =
          evaluateOrDefault(ctx.evaluator, ExpandExtensionMacros{nominal}, {});
      allBufferIDs.append(bufferIDs.begin(), bufferIDs.end());
    }
  }

  if (roles.contains(MacroRole::Preamble)) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      auto bufferIDs =
          evaluateOrDefault(ctx.evaluator, ExpandPreambleMacroRequest{func}, {});
      allBufferIDs.append(bufferIDs.begin(), bufferIDs.end());
    }
  }

  if (roles.contains(MacroRole::Body)) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      auto bufferID =
          evaluateOrDefault(ctx.evaluator, ExpandBodyMacroRequest{func}, {});
      if (bufferID)
        allBufferIDs.push_back(*bufferID);
    }
  }

  // Drop any buffers that come from other macros. We could eliminate this
  // step by adding more fine-grained requests above, which only expand for a
  // single custom attribute.
  SourceManager &sourceMgr = ctx.SourceMgr;
  auto removedAt = std::remove_if(
      allBufferIDs.begin(), allBufferIDs.end(), [&](unsigned bufferID) {
        auto generatedInfo = sourceMgr.getGeneratedSourceInfo(bufferID);
        if (!generatedInfo)
          return true;

        return generatedInfo->attachedMacroCustomAttr != attr;
      });
  allBufferIDs.erase(removedAt, allBufferIDs.end());
  return allBufferIDs;
}

/// Given a resolved cursor, determine whether it is for a macro expansion and
/// return the list of macro expansion buffer IDs that are associated with the
/// macro reference here.
static llvm::SmallVector<unsigned, 2>
getMacroExpansionBuffers(SourceManager &sourceMgr, ResolvedCursorInfoPtr Info) {
  auto *refInfo = dyn_cast<ResolvedValueRefCursorInfo>(Info);
  if (!refInfo || !refInfo->isRef())
    return {};

  auto *macro = dyn_cast_or_null<MacroDecl>(refInfo->getValueD());
  if (!macro)
    return {};

  // Attached macros
  if (auto customAttrRef = refInfo->getCustomAttrRef()) {
    auto macro = cast<MacroDecl>(refInfo->getValueD());
    return getMacroExpansionBuffers(macro, customAttrRef->first,
                                    customAttrRef->second);
  }

  // FIXME: A resolved cursor should contain a slice up to its reference.
  // We shouldn't need to find it again.
  ContextFinder Finder(*Info->getSourceFile(), Info->getLoc(), [&](ASTNode N) {
    if (auto *expr =
            dyn_cast_or_null<MacroExpansionExpr>(N.dyn_cast<Expr *>())) {
      return expr->getStartLoc() == Info->getLoc() ||
             expr->getMacroNameLoc().getBaseNameLoc() == Info->getLoc();
    } else if (auto *decl =
                   dyn_cast_or_null<MacroExpansionDecl>(N.dyn_cast<Decl *>())) {
      return decl->getStartLoc() == Info->getLoc() ||
             decl->getMacroNameLoc().getBaseNameLoc() == Info->getLoc();
    }

    return false;
  });
  Finder.resolve();

  if (!Finder.getContexts().empty()) {
    std::optional<unsigned> bufferID;
    if (auto *target = dyn_cast_or_null<MacroExpansionExpr>(
            Finder.getContexts()[0].dyn_cast<Expr *>())) {
      bufferID = getMacroExpansionBuffer(sourceMgr, target);
    } else if (auto *target = dyn_cast_or_null<MacroExpansionDecl>(
                   Finder.getContexts()[0].dyn_cast<Decl *>())) {
      bufferID = getMacroExpansionBuffer(sourceMgr, target);
    }

    if (bufferID)
      return {*bufferID};
  }

  return {};
}

static bool expandMacro(SourceManager &SM, ResolvedCursorInfoPtr cursorInfo,
                        SourceEditConsumer &editConsumer,
                        bool adjustExpansion) {
  auto bufferIDs = getMacroExpansionBuffers(SM, cursorInfo);
  if (bufferIDs.empty())
    return true;

  SourceFile *containingSF = cursorInfo->getSourceFile();
  if (!containingSF)
    return true;

  // Send all of the rewritten buffer snippets.
  for (auto bufferID : bufferIDs) {
    editConsumer.acceptMacroExpansionBuffer(SM, bufferID, containingSF,
                                            adjustExpansion,
                                            /*includeBufferName=*/true);
  }

  // For an attached macro, remove the custom attribute; it's been fully
  // subsumed by its expansions.
  if (auto attrRef =
          cast<ResolvedValueRefCursorInfo>(cursorInfo)->getCustomAttrRef()) {
    const CustomAttr *attachedMacroAttr = attrRef->first;
    SourceRange range = attachedMacroAttr->getRangeWithAt();
    auto charRange = Lexer::getCharSourceRangeFromSourceRange(SM, range);
    editConsumer.remove(SM, charRange);
  }

  return false;
}

bool RefactoringActionExpandMacro::isApplicable(ResolvedCursorInfoPtr Info,
                                                DiagnosticEngine &Diag) {
  // Never list in available refactorings. Only allow requesting directly.
  return false;
}

bool RefactoringActionExpandMacro::performChange() {
  return expandMacro(SM, CursorInfo, EditConsumer, /*adjustExpansion=*/false);
}

bool RefactoringActionInlineMacro::isApplicable(ResolvedCursorInfoPtr Info,
                                                DiagnosticEngine &Diag) {
  return !getMacroExpansionBuffers(Diag.SourceMgr, Info).empty();
}

bool RefactoringActionInlineMacro::performChange() {
  return expandMacro(SM, CursorInfo, EditConsumer, /*adjustExpansion=*/true);
}
