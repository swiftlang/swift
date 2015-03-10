//===- CodeCompletionCallbacks.h - Parser's interface to code completion --===//
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

#ifndef SWIFT_PARSE_CODE_COMPLETION_CALLBACKS_H
#define SWIFT_PARSE_CODE_COMPLETION_CALLBACKS_H

#include "swift/AST/ASTContext.h"
#include "swift/Parse/Parser.h"

namespace swift {

/// \brief Parser's interface to code completion.
class CodeCompletionCallbacks {
protected:
  Parser &P;
  ASTContext &Context;
  Parser::ParserPosition ExprBeginPosition;

  /// The declaration parsed during delayed parsing that was caused by code
  /// completion.  This declaration contained the code completion token.
  Decl *DelayedParsedDecl = nullptr;

  /// If code completion is done inside a controlling expression of a C-style
  /// for loop statement, this is the declaration of the iteration variable.
  Decl *CStyleForLoopIterationVariable = nullptr;

  /// True if code completion is done inside a raw value expression of an enum
  /// case.
  bool InEnumElementRawValue = false;

public:
  CodeCompletionCallbacks(Parser &P)
      : P(P), Context(P.Context) {
  }

  virtual ~CodeCompletionCallbacks() {}

  void setExprBeginning(Parser::ParserPosition PP) {
    ExprBeginPosition = PP;
  }

  void setDelayedParsedDecl(Decl *D) {
    DelayedParsedDecl = D;
  }

  class InCStyleForExprRAII {
    CodeCompletionCallbacks *Callbacks;

  public:
    InCStyleForExprRAII(CodeCompletionCallbacks *Callbacks,
                        Decl *IterationVariable)
        : Callbacks(Callbacks) {
      if (Callbacks)
        Callbacks->CStyleForLoopIterationVariable = IterationVariable;
    }

    void finished() {
      if (Callbacks)
        Callbacks->CStyleForLoopIterationVariable = nullptr;
    }

    ~InCStyleForExprRAII() {
      finished();
    }
  };

  class InEnumElementRawValueRAII {
    CodeCompletionCallbacks *Callbacks;

  public:
    InEnumElementRawValueRAII(CodeCompletionCallbacks *Callbacks)
        : Callbacks(Callbacks) {
      if (Callbacks)
        Callbacks->InEnumElementRawValue = true;
    }

    ~InEnumElementRawValueRAII() {
      if (Callbacks)
        Callbacks->InEnumElementRawValue = false;
    }
  };

  /// \brief Complete the whole expression.  This is a fallback that should
  /// produce results when more specific completion methods failed.
  virtual void completeExpr() = 0;

  /// \brief Complete expr-dot after we have consumed the dot.
  virtual void completeDotExpr(Expr *E, SourceLoc DotLoc) = 0;

  /// \brief Complete the beginning of expr-postfix -- no tokens provided
  /// by user.
  virtual void completePostfixExprBeginning() = 0;

  /// \brief Complete a given expr-postfix.
  virtual void completePostfixExpr(Expr *E) = 0;

  /// \brief Complete a given expr-postfix, given that there is a following
  /// left parenthesis.
  virtual void completePostfixExprParen(Expr *E) = 0;

  /// \brief Complete expr-super after we have consumed the 'super' keyword.
  virtual void completeExprSuper(SuperRefExpr *SRE) = 0;

  /// \brief Complete expr-super after we have consumed the 'super' keyword and
  /// a dot.
  virtual void completeExprSuperDot(SuperRefExpr *SRE) = 0;

  /// \brief Complete the beginning of type-simple -- no tokens provided
  /// by user.
  virtual void completeTypeSimpleBeginning() = 0;

  /// \brief Complete a given type-identifier after we have consumed the dot.
  virtual void completeTypeIdentifierWithDot(IdentTypeRepr *ITR) = 0;

  /// \brief Complete a given type-identifier when there is no trailing dot.
  virtual void completeTypeIdentifierWithoutDot(IdentTypeRepr *ITR) = 0;

  /// \brief Complete at the beginning of a case stmt pattern.
  virtual void completeCaseStmtBeginning() = 0;

  /// \brief Complete a case stmt pattern that starts with a dot.
  virtual void completeCaseStmtDotPrefix() = 0;

  /// Complete at the beginning of member of a nominal decl member -- no tokens
  /// provided by user.
  virtual void completeNominalMemberBeginning() = 0;

  /// Complete the keyword in attribute, for instance, @availability.
  virtual void completeDeclAttrKeyword(Decl* D, bool Sil, bool Param) = 0;

  /// Complete the parameters in attribute, for instance, version specifier for
  /// @availability.
  virtual void completeDeclAttrParam(DeclAttrKind DK, int Index) = 0;

  /// \brief Signals that the AST for the all the delayed-parsed code was
  /// constructed.  No \c complete*() callbacks will be done after this.
  virtual void doneParsing() = 0;
};

/// \brief A factory to create instances of \c CodeCompletionCallbacks.
class CodeCompletionCallbacksFactory {
public:
  virtual ~CodeCompletionCallbacksFactory() {}

  /// \brief Create an instance of \c CodeCompletionCallbacks.  The result
  /// should be deallocated with 'delete'.
  virtual CodeCompletionCallbacks *createCodeCompletionCallbacks(Parser &P) = 0;
};

} // namespace swift

#endif // LLVM_SWIFT_PARSE_CODE_COMPLETION_CALLBACKS_H

