//===---- InlinableText.cpp - Extract inlinable source text -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#include "InlinableText.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Parse/Lexer.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

/// Gets the last token that exists inside this IfConfigClause, ignoring
/// hoisted elements.
///
/// If the clause is the last element, this returns the beginning of the line
/// before the parent IfConfigDecl's #endif token. Otherwise, it's the beginning
/// of the line before the next clause's #else or #elseif token.
static SourceLoc
getEffectiveEndLoc(SourceManager &sourceMgr, const IfConfigClause *clause,
                   const IfConfigDecl *decl) {
  auto clauses = decl->getClauses();
  if (clause == &clauses.back())
    return Lexer::getLocForStartOfLine(sourceMgr, decl->getEndLoc());

  assert(clause >= clauses.begin() && clause < clauses.end() &&
         "clauses must be contiguous");

  auto *nextClause = clause + 1;
  return Lexer::getLocForStartOfLine(sourceMgr, nextClause->Loc);
}

namespace {
/// A walker that searches through #if declarations, finding all text that does
/// not contribute to the final evaluated AST.
///
/// For example, in the following code:
/// ```
/// #if true
/// print("true")
/// #else
/// print("false")
/// #endif
/// ```
/// ExtractInactiveRanges will return the ranges (with leading newlines) of:
/// ```
/// #if true
/// #else
/// print("false")
/// #endif
/// ```
/// Leaving behind just 'print("true")'s range.
struct ExtractInactiveRanges : public ASTWalker {
  SmallVector<CharSourceRange, 4> ranges;
  SourceManager &sourceMgr;

  explicit ExtractInactiveRanges(SourceManager &sourceMgr)
    : sourceMgr(sourceMgr) {}

  /// Adds the two SourceLocs as a CharSourceRange to the set of ignored
  /// ranges.
  /// \note: This assumes each of these locs is a character location, not a
  ///        token location.
  void addRange(SourceLoc start, SourceLoc end) {
    auto charRange = CharSourceRange(sourceMgr, start, end);
    ranges.push_back(charRange);
  }

  bool walkToDeclPre(Decl *d) {
    auto icd = dyn_cast<IfConfigDecl>(d);
    if (!icd) return true;

    auto start = Lexer::getLocForStartOfLine(sourceMgr, icd->getStartLoc());
    auto end = Lexer::getLocForEndOfLine(sourceMgr, icd->getEndLoc());

    auto clause = icd->getActiveClause();

    // If there's no active clause, add the entire #if...#endif block.
    if (!clause) {
      addRange(start, end);
      return false;
    }

    // Ignore range from beginning of '#if', '#elseif', or '#else' to the
    // beginning of the elements of this clause.
    auto elementsBegin = clause->Loc;
    // If there's a condition (e.g. this isn't a '#else' block), then ignore
    // everything up to the end of the condition.
    if (auto cond = clause->Cond) {
      elementsBegin = cond->getEndLoc();
    }
    addRange(start, Lexer::getLocForEndOfLine(sourceMgr, elementsBegin));

    // Ignore range from effective end of the elements of this clause to the
    // end of the '#endif'
    addRange(getEffectiveEndLoc(sourceMgr, clause, icd), end);

    // Walk into direct children of this node that are IfConfigDecls, because
    // the standard walker won't walk into them.
    for (auto &elt : clause->Elements)
      if (elt.isDecl(DeclKind::IfConfig))
        elt.get<Decl *>()->walk(*this);

    return false;
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *e) {
    // For CollectionExprs, the IfconfigDecls related to elements in the
    // collection are stored in a map keyed on the element they occur before.
    // Passing them to walkToDeclPre is sufficient to remove all conditionals.
    if (auto *collection = dyn_cast<CollectionExpr>(e))
      for (auto &icds : collection->ConditionalsMap)
        for (IfConfigDecl *icd : icds.second)
          walkToDeclPre(icd);

    return {true, e};
  }

  /// Gets the ignored ranges in source order.
  ArrayRef<CharSourceRange> getSortedRanges() {
    std::sort(ranges.begin(), ranges.end(),
              [&](CharSourceRange r1, CharSourceRange r2) {
                assert(!r1.overlaps(r2) && "no overlapping ranges");
                return sourceMgr.isBeforeInBuffer(r1.getStart(), r2.getStart());
              });
    return ranges;
  }
};
} // end anonymous namespace

StringRef swift::extractInlinableText(SourceManager &sourceMgr, ASTNode node,
                                      SmallVectorImpl<char> &scratch) {
  // Extract inactive ranges from the text of the node.
  ExtractInactiveRanges extractor(sourceMgr);
  node.walk(extractor);

  // If there were no inactive ranges, then there were no #if configs.
  // Return an unowned buffer directly into the source file.
  if (extractor.ranges.empty()) {
    auto range =
      Lexer::getCharSourceRangeFromSourceRange(
        sourceMgr, node.getSourceRange());
    return sourceMgr.extractText(range);
  }

  // Begin piecing together active code ranges.

  // Get the full start and end of the provided node, as character locations.
  SourceLoc start = node.getStartLoc();
  SourceLoc end = Lexer::getLocForEndOfToken(sourceMgr, node.getEndLoc());
  for (auto &range : extractor.getSortedRanges()) {
    // Add the text from the current 'start' to this ignored range's start.
    auto charRange = CharSourceRange(sourceMgr, start, range.getStart());
    auto chunk = sourceMgr.extractText(charRange);
    scratch.append(chunk.begin(), chunk.end());

    // Set 'start' to the end of this range, effectively skipping it.
    start = range.getEnd();
  }

  // If there's leftover unignored text, add it.
  if (start != end) {
    auto range = CharSourceRange(sourceMgr, start, end);
    auto chunk = sourceMgr.extractText(range);
    scratch.append(chunk.begin(), chunk.end());
  }
  return { scratch.data(), scratch.size() };
}
