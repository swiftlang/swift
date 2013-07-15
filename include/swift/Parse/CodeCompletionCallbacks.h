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

public:
  CodeCompletionCallbacks(Parser &P)
      : P(P), Context(P.Context) {
  }

  virtual ~CodeCompletionCallbacks() {}

  void setExprBeginning(Parser::ParserPosition PP) {
    ExprBeginPosition = PP;
  }

  /// \brief Complete the whole expression.  This is a fallback that should
  /// produce results when more specific completion methods failed.
  virtual void completeExpr() = 0;

  /// \brief Complete expr-dot after we have consumed the dot.
  virtual void completeDotExpr(Expr *E) = 0;

  /// \brief Complete expr-postfix.
  virtual void completePostfixExpr(Expr *E) = 0;

  /// \brief Complete expr-super after we have consumed the 'super' keyword.
  virtual void completeExprSuper(SuperRefExpr *SRE) = 0;

  /// \brief Complete expr-super after we have consumed the 'super' keyword and
  /// a dot.
  virtual void completeExprSuperDot(SuperRefExpr *SRE) = 0;
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

