//===- SyntaxColoring.h - Routines for syntax coloring --------------------===//
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

#ifndef SWIFT_IDE_SYNTAX_COLORING_H
#define SWIFT_IDE_SYNTAX_COLORING_H

#include "swift/Basic/SourceLoc.h"

namespace swift {
  class SourceManager;
  class TranslationUnit;

namespace ide {

enum class SyntaxColor {
  Keyword,
  DollarIdent,
  Integer,
  Floating,
  String,
  Character,
  CommentLine,
  CommentBlock,
  TypeId
};

struct SyntaxNode {
  SyntaxColor Kind;
  SourceRange Range;

  SyntaxNode(SyntaxColor Kind, SourceRange Range)
    : Kind(Kind), Range(Range) { }
};

class SyntaxColorWalker {
  virtual void anchor();

public:
  virtual ~SyntaxColorWalker() {}

  /// \brief Called when first visiting a syntax node, before walking into its
  /// children.  If it returns false, the subtree is skipped.
  ///
  virtual bool walkToNodePre(SyntaxNode Node) { return true; }

  /// \brief Called after visiting the children of a syntax node. If it returns
  /// false, the remaining traversal is terminated and returns failure.
  virtual bool walkToNodePost(SyntaxNode Node) { return true; }
};

class SyntaxColoringContext {
  struct Implementation;
  Implementation &Impl;
  TranslationUnit &TU;

public:
  SyntaxColoringContext(SourceManager &SM, unsigned BufferID,
                        TranslationUnit &TU);
  ~SyntaxColoringContext();

  bool walk(SyntaxColorWalker &Walker);
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_SYNTAX_COLORING_H

