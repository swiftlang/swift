//===- SyntaxModel.h - Routines for IDE syntax model  ---------------------===//
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

#ifndef SWIFT_IDE_SYNTAX_MODEL_H
#define SWIFT_IDE_SYNTAX_MODEL_H

#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Attr.h"

#include <vector>

namespace swift {
  class Module;
  class SourceFile;

namespace ide {

enum class SyntaxNodeKind : uint8_t {
  Keyword,
  Identifier,
  DollarIdent,
  Integer,
  Floating,
  String,
  Character,
  CommentLine,
  CommentBlock,
  TypeId,
  Attribute
};

struct SyntaxNode {
  SyntaxNodeKind Kind;
  CharSourceRange Range;

  SyntaxNode(SyntaxNodeKind Kind, CharSourceRange Range)
    : Kind(Kind), Range(Range) { }
};

enum class SyntaxStructureKind : uint8_t {
  Class,
  Struct,
  Protocol,
  Enum,
  FreeFunction,
  InstanceFunction,
  StaticFunction,
  InstanceVariable,
};

struct SyntaxStructureNode {
  SyntaxStructureKind Kind;
  DeclAttributes Attrs;
  CharSourceRange Range;
  CharSourceRange NameRange;
  CharSourceRange TypeRange;
  std::vector<CharSourceRange> InheritedTypeRanges;
};

class SyntaxModelWalker {
  virtual void anchor();

public:
  virtual ~SyntaxModelWalker() {}

  /// \brief Called when first visiting a syntax node, before walking into its
  /// children.  If it returns false, the subtree is skipped.
  ///
  virtual bool walkToNodePre(SyntaxNode Node) { return true; }

  /// \brief Called after visiting the children of a syntax node. If it returns
  /// false, the remaining traversal is terminated and returns failure.
  virtual bool walkToNodePost(SyntaxNode Node) { return true; }

  /// \brief Called when first visiting a sub-structure node, before walking
  /// into its children. If it returns false, the subtree is skipped.
  ///
  virtual bool walkToSubStructurePre(SyntaxStructureNode Node) { return true; }

  /// \brief Called after visiting the children of a sub-structure node. If it
  /// returns false, the remaining traversal is terminated and returns failure.
  ///
  virtual bool walkToSubStructurePost(SyntaxStructureNode Node) { return true; }
};

class SyntaxModelContext {
  struct Implementation;
  Implementation &Impl;

public:
  explicit SyntaxModelContext(SourceFile &SrcFile);
  ~SyntaxModelContext();

  bool walk(SyntaxModelWalker &Walker);
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_SYNTAX_MODEL_H

