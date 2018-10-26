//===--- CodeCompletionCallbacks.h - Parser's interface to code completion ===//
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

#ifndef SWIFT_PARSE_CODE_COMPLETION_CALLBACKS_H
#define SWIFT_PARSE_CODE_COMPLETION_CALLBACKS_H

#include "swift/AST/ASTContext.h"
#include "swift/Parse/Parser.h"

namespace swift {

enum class ObjCSelectorContext {
  /// Code completion is not performed inside #selector
  None,
  /// Code completion is performed in a method #selector
  MethodSelector,
  /// Code completion is performed inside #selector(getter:)
  GetterSelector,
  /// Code completion is performed inside #selector(setter:)
  SetterSelector
};

/// \brief Parser's interface to code completion.
class CodeCompletionCallbacks {
protected:
  Parser &P;
  ASTContext &Context;
  ParserPosition ExprBeginPosition;

  /// The declaration parsed during delayed parsing that was caused by code
  /// completion. This declaration contained the code completion token.
  Decl *ParsedDecl = nullptr;

  /// True if code completion is done inside a raw value expression of an enum
  /// case.
  bool InEnumElementRawValue = false;

  /// Whether or not the expression that is currently parsed is inside a
  /// \c #selector and if so, which kind of selector
  ObjCSelectorContext ParseExprSelectorContext = ObjCSelectorContext::None;

  /// Whether or not the expression that shall be completed is inside a
  /// \c #selector and if so, which kind of selector
  ObjCSelectorContext CompleteExprSelectorContext = ObjCSelectorContext::None;

  std::vector<Expr *> leadingSequenceExprs;

public:
  CodeCompletionCallbacks(Parser &P)
      : P(P), Context(P.Context) {
  }

  virtual ~CodeCompletionCallbacks() {}

  bool isInsideObjCSelector() const {
    return CompleteExprSelectorContext != ObjCSelectorContext::None;
  }

  void setExprBeginning(ParserPosition PP) {
    ExprBeginPosition = PP;
  }

  /// Set the decl inside which the code-completion occurred.  This is used when
  /// completing inside a parameter list or where clause where the Parser's
  /// CurDeclContext will not be where we want to perform lookup.
  void setParsedDecl(Decl *D) {
    ParsedDecl = D;
  }

  void setLeadingSequenceExprs(ArrayRef<Expr *> exprs) {
    leadingSequenceExprs.assign(exprs.begin(), exprs.end());
  }

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

  /// RAII type that temporarily sets the "in Objective-C #selector expression"
  /// flag on the code completion callbacks object.
  class InObjCSelectorExprRAII {
    CodeCompletionCallbacks *Callbacks;

  public:
    InObjCSelectorExprRAII(CodeCompletionCallbacks *Callbacks,
                           ObjCSelectorContext SelectorContext)
        : Callbacks(Callbacks) {
      if (Callbacks)
        Callbacks->ParseExprSelectorContext = SelectorContext;
    }

    ~InObjCSelectorExprRAII() {
      if (Callbacks)
        Callbacks->ParseExprSelectorContext = ObjCSelectorContext::None;
    }
  };

  /// \brief Complete the whole expression.  This is a fallback that should
  /// produce results when more specific completion methods failed.
  virtual void completeExpr() = 0;

  /// \brief Complete expr-dot after we have consumed the dot.
  virtual void completeDotExpr(Expr *E, SourceLoc DotLoc) = 0;

  /// \brief Complete the beginning of a statement or expression.
  virtual void completeStmtOrExpr() = 0;

  /// \brief Complete the beginning of expr-postfix -- no tokens provided
  /// by user.
  virtual void completePostfixExprBeginning(CodeCompletionExpr *E) = 0;

  /// \brief Complete the beginning of expr-postfix in a for-each loop sequqence
  /// -- no tokens provided by user.
  virtual void completeForEachSequenceBeginning(CodeCompletionExpr *E) = 0;

  /// \brief Complete a given expr-postfix.
  virtual void completePostfixExpr(Expr *E, bool hasSpace) = 0;

  /// \brief Complete a given expr-postfix, given that there is a following
  /// left parenthesis.
  virtual void completePostfixExprParen(Expr *E, Expr *CodeCompletionE) = 0;

  /// \brief Complete expr-super after we have consumed the 'super' keyword.
  virtual void completeExprSuper(SuperRefExpr *SRE) = 0;

  /// \brief Complete expr-super after we have consumed the 'super' keyword and
  /// a dot.
  virtual void completeExprSuperDot(SuperRefExpr *SRE, SourceLoc DotLoc) = 0;

  /// \brief Complete the argument to an Objective-C #keyPath
  /// expression.
  ///
  /// \param KPE A partial #keyPath expression that can be used to
  /// provide context. This will be \c NULL if no components of the
  /// #keyPath argument have been parsed yet.
  virtual void completeExprKeyPath(KeyPathExpr *KPE, SourceLoc DotLoc) = 0;

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
  virtual void completeNominalMemberBeginning(
      SmallVectorImpl<StringRef> &Keywords) = 0;

  /// Complete at the beginning of accessor in a accessor block.
  virtual void completeAccessorBeginning() = 0;

  /// Complete the keyword in attribute, for instance, @available.
  virtual void completeDeclAttrKeyword(Decl *D, bool Sil, bool Param) = 0;

  /// Complete the parameters in attribute, for instance, version specifier for
  /// @available.
  virtual void completeDeclAttrParam(DeclAttrKind DK, int Index) = 0;

  /// Complete within a precedence group decl or after a colon in an
  /// operator decl.
  virtual void completeInPrecedenceGroup(SyntaxKind SK) = 0;

  /// Complete the platform names inside #available statements.
  virtual void completePoundAvailablePlatform() = 0;

  /// Complete the import decl with importable modules.
  virtual void
  completeImportDecl(std::vector<std::pair<Identifier, SourceLoc>> &Path) = 0;

  /// Complete unresolved members after dot.
  virtual void completeUnresolvedMember(CodeCompletionExpr *E,
                                        SourceLoc DotLoc) = 0;

  virtual void completeAssignmentRHS(AssignExpr *E) = 0;

  virtual void completeCallArg(CodeCompletionExpr *E) = 0;

  virtual void completeReturnStmt(CodeCompletionExpr *E) = 0;

  /// Complete a yield statement.  A missing yield index means that the
  /// completion immediately follows the 'yield' keyword; it may be either
  /// an expresion or a parenthesized expression list.  A present yield
  /// index means that the completion is within the parentheses and is
  /// for a specific yield value.
  virtual void completeYieldStmt(CodeCompletionExpr *E,
                                 Optional<unsigned> yieldIndex) = 0;

  virtual void completeAfterPoundExpr(CodeCompletionExpr *E,
                                      Optional<StmtKind> ParentKind) = 0;

  virtual void completeAfterPoundDirective() = 0;

  virtual void completePlatformCondition() = 0;

  virtual void completeAfterIfStmt(bool hasElse) = 0;

  virtual void completeGenericParams(TypeLoc TL) = 0;

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
