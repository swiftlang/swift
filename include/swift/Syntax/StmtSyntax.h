//===--- StmtSyntax.h - Swift Statement Syntax Interface --------*- C++ -*-===//
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
//
// This file defines the interface for statement-specific syntax nodes,
// such as for if- and while- statements.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_STMTSYNTAX_H
#define SWIFT_SYNTAX_STMTSYNTAX_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"

namespace swift {
namespace syntax {

class StmtSyntaxData : public SyntaxData {
protected:
  StmtSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                 CursorIndex IndexInParent = 0)
    : SyntaxData(Raw, Parent, IndexInParent) {}
public:
  static bool classof(const SyntaxData *S) {
    return S->isStmt();
  }
};

/// statement -> expression ';'?
///            | declaration ';'?
///            | loop-statement ';'?
///            | branch-statement ';'?
///            | labeled-statement ';'?
///            | control-transfer-statement ';'?
///            | defer-statement ';'?
///            | do-statement ';'?
///            | compiler-control-statement ';'?
class StmtSyntax : public Syntax {
protected:
  StmtSyntax(RC<SyntaxData> Root, StmtSyntaxData *Data);
public:
  static bool classof(const Syntax *S) {
    return S->isStmt();
  }
};

#pragma mark -
#pragma mark code-block Data

class CodeBlockStmtSyntaxData final : public StmtSyntaxData {
  friend class SyntaxData;
  friend class CodeBlockStmtSyntax;
  friend struct SyntaxFactory;

  CodeBlockStmtSyntaxData(RC<RawSyntax> Raw);
  static RC<CodeBlockStmtSyntaxData> make(RC<RawSyntax> Raw);
  static RC<CodeBlockStmtSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::CodeBlockStmt;
  }
};

#pragma mark -
#pragma mark code-block API

/// code-block -> '{' statements? '}'
class CodeBlockStmtSyntax : public StmtSyntax {
  enum class Cursor {
    LeftBrace,
    Elements,
    RightBrace,
  };
  friend struct SyntaxFactory;
  friend class CodeBlockStmtSyntaxData;

  CodeBlockStmtSyntax(const RC<SyntaxData> Root, CodeBlockStmtSyntaxData *Data);

public:
  /// Returns the left brace of the code block.
  RC<TokenSyntax> getLeftBraceToken() const;

  /// Returns a new `CodeBlockSyntax` with the specified left brace token.
  CodeBlockStmtSyntax withLeftBraceToken(RC<TokenSyntax> NewLeftBrace) const;

  /// Return the n-th element in the code block.
  Syntax getElement(size_t n) const;

  /// Return a new `CodeBlockSyntax` with the added statement at the end,
  /// before the right brace token.
  CodeBlockStmtSyntax withAddedElement(Syntax AdditionalElement) const;

  /// Returns a new `CodeBlockSyntax` with `NewElements` substituted into
  /// the body.
  CodeBlockStmtSyntax
  withElements(const std::vector<Syntax> NewElements) const;

  /// Returns the right brace of the code block.
  RC<TokenSyntax> getRightBraceToken() const;

  /// Returns a new `CodeBlockSyntax` with the specified right brace token.
  CodeBlockStmtSyntax withRightBraceToken(RC<TokenSyntax> NewRightBraces) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::CodeBlockStmt;
  }
};

#pragma mark -
#pragma mark statements Data

class StmtListSyntaxData final : public StmtSyntaxData {
  friend class SyntaxData;
  friend class StmtListSyntax;
  friend class StmtListSyntaxBuilder;

  StmtListSyntaxData(RC<RawSyntax> Raw,
                     const SyntaxData *Parent = nullptr,
                     CursorIndex IndexInParent = 0);
  static RC<StmtListSyntaxData> make(RC<RawSyntax> Raw,
                                     const SyntaxData *Parent = nullptr,
                                     CursorIndex IndexInParent = 0);
  static RC<StmtListSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::StmtList;
  }
};

#pragma mark -
#pragma mark statements API

/// statements -> statement
///             | statement statements
class StmtListSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class StmtListSyntaxBuilder;
  friend class SyntaxData;

  using DataType = StmtListSyntaxData;

  StmtListSyntax(const RC<SyntaxData> Root, const StmtListSyntaxData *Data);
public:
  /// Returns a new statement list with an additional statement.
  StmtListSyntax withAddedStatement(Syntax AdditionalStatement) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::StmtList;
  }
};

#pragma mark -
#pragma mark statements Builder

class StmtListSyntaxBuilder final {
  RawSyntax::LayoutList StmtListLayout;
public:
  StmtListSyntaxBuilder &addStatement(Syntax Statement);
  StmtListSyntax build() const;
};

#pragma mark -
#pragma mark fallthrough-statement Data

class FallthroughStmtSyntaxData final : public StmtSyntaxData {
  friend class SyntaxData;
  friend class FallthroughStmtSyntax;
  friend struct SyntaxFactory;

  FallthroughStmtSyntaxData(RC<RawSyntax> Raw,
                            const SyntaxData *Parent = nullptr,
                            CursorIndex IndexInParent = 0);
  static RC<FallthroughStmtSyntaxData> make(RC<RawSyntax> Raw,
                                            const SyntaxData *Parent = nullptr,
                                            CursorIndex IndexInParent = 0);
  static RC<FallthroughStmtSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::StmtList;
  }
};

#pragma mark -
#pragma mark fallthrough-statement API

class FallthroughStmtSyntax : public StmtSyntax {
  friend struct SyntaxFactory;

  enum class Cursor : CursorIndex {
    FallthroughKeyword,
  };

  FallthroughStmtSyntax(const RC<SyntaxData> Root,
                        FallthroughStmtSyntaxData *Data);

  static FallthroughStmtSyntax make(RC<RawSyntax> Raw,
                                    const SyntaxData *Parent = nullptr,
                                    CursorIndex IndexInParent = 0);
  static FallthroughStmtSyntax makeBlank();

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FallthroughStmt;
  }
};

} // end namespace syntax
} // end namespace swift

#endif
