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
#include "swift/Syntax/UnknownSyntax.h"

using llvm::Optional;

namespace swift {
namespace syntax {

class ExprSyntax;
class ExprSyntaxData;

#pragma mark - statement Data

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

#pragma mark - statement API

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
  StmtSyntax(const RC<SyntaxData> Root, const StmtSyntaxData *Data);
public:
  static bool classof(const Syntax *S) {
    return S->isStmt();
  }
};

#pragma mark - unknown-statement Data

class UnknownStmtSyntaxData : public UnknownSyntaxData {
  UnknownStmtSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                        CursorIndex IndexInParent = 0);
public:
  static RC<UnknownStmtSyntaxData> make(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent = nullptr,
                                        CursorIndex IndexInParent = 0);

  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::UnknownStmt;
  }
};

#pragma mark - unknown-statement API

class UnknownStmtSyntax : public UnknownSyntax {
  friend class SyntaxData;
  friend class UnknownStmtSyntaxData;
  friend class LegacyASTTransformer;

  using DataType = UnknownStmtSyntaxData;

  UnknownStmtSyntax(const RC<SyntaxData> Root,
                    const UnknownStmtSyntaxData *Data);

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::UnknownStmt;
  }
};

#pragma mark -
#pragma mark code-block Data

class CodeBlockStmtSyntaxData final : public StmtSyntaxData {
  friend class SyntaxData;
  friend class CodeBlockStmtSyntax;
  friend struct SyntaxFactory;

  CodeBlockStmtSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                          CursorIndex IndexInParent = 0);
  static RC<CodeBlockStmtSyntaxData> make(RC<RawSyntax> Raw,
                                          const SyntaxData *Parent = nullptr,
                                          CursorIndex IndexInParent = 0);
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
    return SD->getKind() == SyntaxKind::FallthroughStmt;
  }
};

#pragma mark -
#pragma mark fallthrough-statement API

/// fallthrough-statement -> 'fallthrough'
class FallthroughStmtSyntax : public StmtSyntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class FallthroughStmtSyntaxData;

  using DataType = FallthroughStmtSyntaxData;

  enum class Cursor : CursorIndex {
    FallthroughKeyword,
  };

  FallthroughStmtSyntax(const RC<SyntaxData> Root,
                        const FallthroughStmtSyntaxData *Data);

  static FallthroughStmtSyntax make(RC<RawSyntax> Raw,
                                    const SyntaxData *Parent = nullptr,
                                    CursorIndex IndexInParent = 0);
  static FallthroughStmtSyntax makeBlank();

public:

  /// Get the 'fallthrough' keyword associated comprising this
  /// fallthrough statement.
  RC<TokenSyntax> getFallthroughKeyword() const;

  /// Return a new FallthroughStmtSyntax with the given fallthrough
  /// keyword.
  FallthroughStmtSyntax
  withFallthroughKeyword(RC<TokenSyntax> NewFallthroughKeyword) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FallthroughStmt;
  }
};

#pragma mark - break-statement Data

class BreakStmtSyntaxData : public StmtSyntaxData {
  friend class SyntaxData;
  friend class BreakStmtSyntax;
  friend struct SyntaxFactory;
  BreakStmtSyntaxData(RC<RawSyntax> Raw,
                            const SyntaxData *Parent = nullptr,
                            CursorIndex IndexInParent = 0);
  static RC<BreakStmtSyntaxData> make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent = nullptr,
                                      CursorIndex IndexInParent = 0);
  static RC<BreakStmtSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::BreakStmt;
  }

};

#pragma mark - break-statement API

/// break-statement -> 'break' label-name?
/// label-name -> identifier
class BreakStmtSyntax : public StmtSyntax {
  friend struct SyntaxFactory;
  friend class BreakStmtSyntaxData;
  friend class SyntaxData;

  using DataType = BreakStmtSyntaxData;

  enum class Cursor : CursorIndex {
    BreakKeyword,
    Label
  };

  BreakStmtSyntax(const RC<SyntaxData> Root,
                  BreakStmtSyntaxData *Data);

  static BreakStmtSyntax make(RC<RawSyntax> Raw,
                                    const SyntaxData *Parent = nullptr,
                                    CursorIndex IndexInParent = 0);
  static BreakStmtSyntax makeBlank();
public:

  /// Return the 'break' keyword associated with this break statement.
  RC<TokenSyntax> getBreakKeyword() const;

  /// Return a new `BreakStmtSyntax` with the given 'break' keyword.
  BreakStmtSyntax withBreakKeyword(RC<TokenSyntax> NewBreakKeyword) const;

  /// Return the destination label of this break statement. If it doesn't
  /// have one, the token is marked as missing.
  RC<TokenSyntax> getLabel() const;

  /// Return a new `BreakStmtSyntax` with the given destination label.
  BreakStmtSyntax withLabel(RC<TokenSyntax> NewLabel) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::BreakStmt;
  }
};

#pragma mark - continue-statement Data

class ContinueStmtSyntaxData : public StmtSyntaxData {
  friend class SyntaxData;
  friend class ContinueStmtSyntax;
  friend struct SyntaxFactory;
  ContinueStmtSyntaxData(RC<RawSyntax> Raw,
                      const SyntaxData *Parent = nullptr,
                      CursorIndex IndexInParent = 0);
  static RC<ContinueStmtSyntaxData> make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent = nullptr,
                                      CursorIndex IndexInParent = 0);
  static RC<ContinueStmtSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::ContinueStmt;
  }

};

#pragma mark - continue-statement API

/// continue-statement -> 'continue' label-name?
/// label-name -> identifier
class ContinueStmtSyntax : public StmtSyntax {
  friend struct SyntaxFactory;
  friend class ContinueStmtSyntaxData;
  friend class SyntaxData;

  using DataType = ContinueStmtSyntaxData;

  enum class Cursor : CursorIndex {
    ContinueKeyword,
    Label
  };

  ContinueStmtSyntax(const RC<SyntaxData> Root,
                     ContinueStmtSyntaxData *Data);

  static ContinueStmtSyntax make(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent = nullptr,
                                 CursorIndex IndexInParent = 0);
  static ContinueStmtSyntax makeBlank();
public:

  /// Return the 'continue' keyword associated with this continue statement.
  RC<TokenSyntax> getContinueKeyword() const;

  /// Return a new `ContinueStmtSyntax` with the given 'continue' keyword.
  ContinueStmtSyntax withContinueKeyword(RC<TokenSyntax> NewBreakKeyword) const;

  /// Return the destination label of this break statement. If it doesn't
  /// have one, the token is marked as continue.
  RC<TokenSyntax> getLabel() const;

  /// Return a new `ContinueStmtSyntax` with the given destination label.
  ContinueStmtSyntax withLabel(RC<TokenSyntax> NewLabel) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::ContinueStmt;
  }
};

#pragma mark - return-statement Data

class ReturnStmtSyntaxData : public StmtSyntaxData {
  friend class SyntaxData;
  friend class ReturnStmtSyntax;
  friend struct SyntaxFactory;

  RC<ExprSyntaxData> CachedExpression;

  ReturnStmtSyntaxData(RC<RawSyntax> Raw,
                       const SyntaxData *Parent = nullptr,
                       CursorIndex IndexInParent = 0);
  static RC<ReturnStmtSyntaxData> make(RC<RawSyntax> Raw,
                                       const SyntaxData *Parent = nullptr,
                                       CursorIndex IndexInParent = 0);
  static RC<ReturnStmtSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::ReturnStmt;
  }
  
};

#pragma mark - return-statement API

/// return-statement -> 'return' expression? ';'?
class ReturnStmtSyntax : public StmtSyntax {
  friend struct SyntaxFactory;
  friend class ReturnStmtSyntaxData;
  friend class SyntaxData;
  friend class Syntax;

  using DataType = ReturnStmtSyntaxData;

  enum class Cursor : CursorIndex {
    ReturnKeyword,
    Expression
  };

  ReturnStmtSyntax(const RC<SyntaxData> Root,
                   const ReturnStmtSyntaxData *Data);

  static ReturnStmtSyntax make(RC<RawSyntax> Raw,
                               const SyntaxData *Parent = nullptr,
                               CursorIndex IndexInParent = 0);
  static ReturnStmtSyntax makeBlank();
public:

  /// Return the 'return' keyword associated with this return statement.
  RC<TokenSyntax> getReturnKeyword() const;

  /// Return a new `ReturnStmtSyntax` with the given 'return' keyword.
  ReturnStmtSyntax withReturnKeyword(RC<TokenSyntax> NewReturnKeyword) const;

  /// Return the expression of this return statement.
  Optional<ExprSyntax> getExpression() const;

  /// Return a new `ReturnStmtSyntax` with the given destination label.
  ReturnStmtSyntax withExpression(ExprSyntax NewExpression) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::ReturnStmt;
  }
};

} // end namespace syntax
} // end namespace swift

#endif
