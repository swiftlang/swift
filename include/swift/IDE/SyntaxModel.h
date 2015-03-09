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
  class Decl;
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
  /// Marks the parens for a string interpolation.
  StringInterpolationAnchor,
  Character,
  CommentLine,
  CommentBlock,
  /// A marker like 'FIXME:' or 'TODO:' inside a comment.
  CommentMarker,
  CommentURL,
  TypeId,
  /// #if/#else/#endif occurence.
  BuildConfigKeyword,
  /// An identifier in a #if condition.
  BuildConfigId,
  /// Any occurence of '@<attribute-name>' anywhere.
  AttributeId,
  /// A "resolved/active" attribute. Mis-applied attributes will be AttributeId.
  AttributeBuiltin
};

struct SyntaxNode {
  SyntaxNodeKind Kind;
  CharSourceRange Range;

  SyntaxNode(SyntaxNodeKind Kind, CharSourceRange Range)
    : Kind(Kind), Range(Range) { }

  bool isComment() const {
    return Kind == SyntaxNodeKind::CommentLine ||
           Kind == SyntaxNodeKind::CommentBlock;
  }

  bool isInnerCommentNode() const {
    return Kind == SyntaxNodeKind::CommentMarker ||
           Kind == SyntaxNodeKind::CommentURL;
  }
};

enum class SyntaxStructureKind : uint8_t {
  Class,
  Struct,
  Protocol,
  Enum,
  Extension,
  FreeFunction,
  InstanceFunction,
  StaticFunction,
  ClassFunction,
  GlobalVariable,
  InstanceVariable,
  StaticVariable,
  ClassVariable,

  ForEachStatement,
  ForStatement,
  WhileStatement,
  DoWhileStatement,
  IfStatement,
  SwitchStatement,
  CaseStatement,
  Parameter,
  BraceStatement,
  CallExpression,
  ArrayExpression,
  DictionaryExpression,
};

enum class SyntaxStructureElementKind : uint8_t {
  Id,
  Expr,
  InitExpr,
  ConditionExpr,
  Pattern,
  TypeRef,
};

struct SyntaxStructureElement {
  SyntaxStructureElementKind Kind;
  CharSourceRange Range;

  SyntaxStructureElement(SyntaxStructureElementKind Kind, CharSourceRange Range)
    : Kind(Kind), Range(Range) { }
};

struct SyntaxStructureNode {
  const Decl *Dcl = nullptr;
  SyntaxStructureKind Kind;
  DeclAttributes Attrs;
  CharSourceRange Range;
  CharSourceRange BodyRange;
  CharSourceRange NameRange;
  CharSourceRange TypeRange;
  std::vector<CharSourceRange> InheritedTypeRanges;
  std::vector<SyntaxStructureElement> Elements;

  bool isVariable() const {
    switch (Kind) {
    case SyntaxStructureKind::GlobalVariable:
    case SyntaxStructureKind::InstanceVariable:
    case SyntaxStructureKind::StaticVariable:
    case SyntaxStructureKind::ClassVariable:
    case SyntaxStructureKind::Parameter:
      return true;
    default:
      return false;
    }
  }
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

