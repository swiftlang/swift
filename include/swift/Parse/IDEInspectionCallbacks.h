//===--- IDEInspectionCallbacks.h - Parser's interface to IDE inspection --===//
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

/// Attributes that have syntax which can't be modelled using a function call.
/// This can't be \c DeclAttrKind because '@freestandig' and '@attached' have
/// the same attribute kind but take different macro roles as arguemnts.
enum class CustomSyntaxAttributeKind {
  Available,
  FreestandingMacro,
  AttachedMacro,
  StorageRestrictions
};

/// A bit of a hack. When completing inside the '@storageRestrictions'
/// attribute, we use the \c ParamIndex field to communicate where inside the
/// attribute we are performing the completion.
enum class StorageRestrictionsCompletionKind : int {
  /// We are completing directly after the '(' and require a 'initializes' or
  /// 'accesses' label.
  Label,
  /// We are completing in a context that only allows arguments (ie. accessed or
  /// initialized variables) and doesn't permit an argument label.
  Argument,
  /// Completion in a context that allows either an argument or the
  /// 'initializes' label.
  ArgumentOrInitializesLabel,
  /// Completion in a context that allows either an argument or the
  /// 'accesses' label.
  ArgumentOrAccessesLabel
};

/// Parser's interface to code completion.
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
  CodeCompletionCallbacks(Parser &P) : P(P), Context(P.Context) {}

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

  /// Set target decl for attribute if the CC token is in attribute of the decl.
  virtual void setAttrTargetDeclKind(std::optional<DeclKind> DK) {}

  /// Set that the code completion token occurred in a custom attribute. This
  /// allows us to type check the custom attribute even if it is not attached to
  /// the AST, e.g. because there is no var declaration following it.
  virtual void setCompletingInAttribute(CustomAttr *Attr){};

  /// Complete expr-dot after we have consumed the dot.
  virtual void completeDotExpr(CodeCompletionExpr *E, SourceLoc DotLoc) {};

  /// Complete the beginning of a statement or expression.
  virtual void completeStmtOrExpr(CodeCompletionExpr *E) {};

  /// Complete the beginning of expr-postfix -- no tokens provided
  /// by user.
  virtual void completePostfixExprBeginning(CodeCompletionExpr *E) {};

  /// Complete the beginning of expr-postfix in a for-each loop sequence
  /// -- no tokens provided by user.
  virtual void completeForEachSequenceBeginning(CodeCompletionExpr *E) {};

  /// Complete the \c in keyword in a for-each loop.
  virtual void completeForEachInKeyword(){};

  /// Complete a expr-postfix. The \c CodeCompletionExpr has the expression it
  /// is completing after set as its base.
  virtual void completePostfixExpr(CodeCompletionExpr *E, bool hasSpace){};

  /// Complete the argument to an Objective-C #keyPath
  /// expression.
  ///
  /// \param KPE A partial #keyPath expression that can be used to
  /// provide context. This will be \c NULL if no components of the
  /// #keyPath argument have been parsed yet.
  virtual void completeExprKeyPath(KeyPathExpr *KPE, SourceLoc DotLoc) {};

  /// Complete the beginning of the type for a parameter of a
  /// func/subscript/closure, or the type for a parameter in a function type.
  /// For the latter, we cannot know for sure whether the user is trying to
  /// write a function type, so will complete for e.g `let x: (#^COMPLETE^#`.
  virtual void completeTypePossibleFunctionParamBeginning() {}

  /// Complete the beginning of the type of result of func/var/let/subscript.
  virtual void completeTypeDeclResultBeginning() {};

  /// Same as `completeTypeSimpleOrComposition` but also allows `repeat`.
  virtual void completeTypeBeginning(){};

  /// Same as `completeTypeSimpleBeginning` but also allows `any`, `some` and
  /// `each`.
  virtual void completeTypeSimpleOrComposition(){};

  /// Complete the beginning of type-simple -- no tokens provided
  /// by user.
  virtual void completeTypeSimpleBeginning() {};

  /// Complete a given \c type-simple after we have consumed the dot.
  virtual void completeTypeSimpleWithDot(TypeRepr *TR){};

  /// Complete a given \c type-simple when there is no trailing dot.
  virtual void completeTypeSimpleWithoutDot(TypeRepr *TR){};

  /// Complete a given \c type-simple following a \c ~ prefix.
  virtual void completeTypeSimpleInverted() {};

  /// Complete the beginning of a case statement at the top of switch stmt.
  virtual void completeCaseStmtKeyword() {};

  /// Complete at the beginning of a case stmt pattern.
  virtual void completeCaseStmtBeginning(CodeCompletionExpr *E) {};

  /// Complete at the beginning of member of a nominal decl member -- no tokens
  /// provided by user.
  virtual void completeNominalMemberBeginning(
      SmallVectorImpl<StringRef> &Keywords, SourceLoc introducerLoc) {};

  /// Complete at the beginning of accessor in a accessor block.
  virtual void completeAccessorBeginning(CodeCompletionExpr *E) {};

  /// Complete the keyword in attribute, for instance, @available.
  virtual void completeDeclAttrBeginning(bool Sil, bool isIndependent) {};

  /// Complete the parameters in attribute, for instance, version specifier for
  /// @available.
  /// If `HasLabel` is `true`, then the argument already has a label specified,
  /// e.g. we're completing after `names: ` in a macro declaration.
  virtual void completeDeclAttrParam(CustomSyntaxAttributeKind DK, int Index,
                                     bool HasLabel){};

  /// Complete 'async' and 'throws' at effects specifier position.
  virtual void completeEffectsSpecifier(bool hasAsync, bool hasThrows) {};

  enum class PrecedenceGroupCompletionKind {
    Relation,
    Associativity,
    AttributeList,
    Assignment,
  };
  /// Complete within a precedence group decl or after a colon in an
  /// operator decl.
  virtual void completeInPrecedenceGroup(PrecedenceGroupCompletionKind SK) {};

  /// Complete the platform names inside #available statements.
  virtual void completePoundAvailablePlatform() {};

  /// Complete the import decl with importable modules.
  virtual void
  completeImportDecl(ImportPath::Builder &Path) {};

  /// Complete unresolved members after dot.
  virtual void completeUnresolvedMember(CodeCompletionExpr *E,
                                        SourceLoc DotLoc) {};

  virtual void completeCallArg(CodeCompletionExpr *E) {};

  virtual bool canPerformCompleteLabeledTrailingClosure() const {
    return false;
  }

  virtual void completeReturnStmt(CodeCompletionExpr *E){};

  virtual void completeThenStmt(CodeCompletionExpr *E) {};

  /// Complete a yield statement.  A missing yield index means that the
  /// completion immediately follows the 'yield' keyword; it may be either
  /// an expression or a parenthesized expression list.  A present yield
  /// index means that the completion is within the parentheses and is
  /// for a specific yield value.
  virtual void completeYieldStmt(CodeCompletionExpr *E,
                                 std::optional<unsigned> yieldIndex){};

  virtual void completeAfterPoundExpr(CodeCompletionExpr *E,
                                      std::optional<StmtKind> ParentKind){};

  virtual void completeAfterPoundDirective() {};

  virtual void completePlatformCondition() {};

  virtual void completeAfterIfStmtElse() {};

  virtual void completeGenericRequirement() {};

  virtual void completeStmtLabel(StmtKind ParentKind) {};

  virtual
  void completeForEachPatternBeginning(bool hasTry, bool hasAwait) {};

  virtual void completeTypeAttrBeginning() {};

  virtual void completeTypeAttrInheritanceBeginning() {};

  virtual void completeOptionalBinding(){};
};

class DoneParsingCallback {
public:
  virtual ~DoneParsingCallback() {}

  /// Signals that the AST for the all the delayed-parsed code was
  /// constructed.  No \c complete*() callbacks will be done after this.
  virtual void doneParsing(SourceFile *SrcFile) = 0;
};

/// A factory to create instances of \c CodeCompletionCallbacks and
/// \c DoneParsingCallback.
class IDEInspectionCallbacksFactory {
public:
  virtual ~IDEInspectionCallbacksFactory() {}

  struct Callbacks {
    std::shared_ptr<CodeCompletionCallbacks> CompletionCallbacks;
    std::shared_ptr<DoneParsingCallback> DoneParsingCallback;
  };

  /// Create an instance of \c IDEInspectionCallbacks.  The result
  /// should be deallocated with 'delete'.
  virtual Callbacks createCallbacks(Parser &P) = 0;
};

} // namespace swift

#endif // LLVM_SWIFT_PARSE_CODE_COMPLETION_CALLBACKS_H
