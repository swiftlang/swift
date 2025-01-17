//===--- SyntacticElementTarget.h - Syntactic Element Target ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the SyntacticElementTarget class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_SYNTACTIC_ELEMENT_TARGET_H
#define SWIFT_SEMA_SYNTACTIC_ELEMENT_TARGET_H

#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/TaggedUnion.h"
#include "swift/Sema/ConstraintLocator.h"
#include "swift/Sema/ContextualTypeInfo.h"

namespace swift {

namespace constraints {
/// Describes information about a for-in loop over a sequence that needs to be
/// tracked in the constraint system.
struct SequenceIterationInfo {
  /// The type of the sequence.
  Type sequenceType;

  /// The type of an element in the sequence.
  Type elementType;

  /// The type of the pattern that matches the elements.
  Type initType;

  /// Implicit `$iterator = <sequence>.makeIterator()`
  PatternBindingDecl *makeIteratorVar;

  /// Implicit `$iterator.next()` call.
  Expr *nextCall;
};

/// Describes information about a for-in loop over a pack that needs to be
/// tracked in the constraint system.
struct PackIterationInfo {
  /// The type of the pattern that matches the elements.
  Type patternType;
};

/// Describes information about a for-in loop that needs to be tracked
/// within the constraint system.
using ForEachStmtInfo = TaggedUnion<SequenceIterationInfo, PackIterationInfo>;

/// Describes the target to which a constraint system's solution can be
/// applied.
class SyntacticElementTarget {
public:
  enum class Kind {
    expression,
    closure,
    function,
    stmtCondition,
    caseLabelItem,
    patternBinding,
    uninitializedVar,

    /// The preamble of a for-in statement, including everything except the
    /// body.
    forEachPreamble,
  } kind;

private:
  union {
    struct {
      /// The expression being type-checked.
      Expr *expression;

      /// The declaration context in which the expression is being
      /// type-checked.
      DeclContext *dc;

      /// The contextual type info for the expression.
      ContextualTypeInfo contextualInfo;

      /// When initializing a pattern from the expression, this is the
      /// pattern.
      Pattern *pattern;

      /// The parent return statement if any.
      ReturnStmt *parentReturnStmt;

      struct {
        /// The variable to which property wrappers have been applied, if
        /// this is an initialization involving a property wrapper.
        VarDecl *wrappedVar;

        /// The innermost call to \c init(wrappedValue:), if this is an
        /// initialization involving a property wrapper.
        ApplyExpr *innermostWrappedValueInit;

        /// Whether this property wrapper has an initial wrapped value specified
        /// via \c = .
        bool hasInitialWrappedValue;
      } propertyWrapper;

      /// Whether the expression result will be discarded at the end.
      bool isDiscarded;

      union {
        struct {
          /// The pattern binding declaration for an initialization, if any.
          PatternBindingDecl *patternBinding;

          /// The index into the pattern binding declaration, if any.
          unsigned patternBindingIndex;
        } initialization;
      };
    } expression;

    struct {
      /// The closure expression being type-checked.
      ClosureExpr *closure;

      /// The type to which the expression should be converted.
      Type convertType;
    } closure;

    struct {
      AnyFunctionRef function;
      BraceStmt *body;
    } function;

    struct {
      StmtCondition stmtCondition;
      DeclContext *dc;
    } stmtCondition;

    struct {
      CaseLabelItem *caseLabelItem;
      DeclContext *dc;
    } caseLabelItem;

    struct {
      PatternBindingDecl *binding;
      /// Index into pattern binding declaration (if any).
      unsigned index;
      PointerUnion<VarDecl *, Pattern *> declaration;
      /// Type associated with the declaration.
      Type type;
    } uninitializedVar;

    struct {
      ForEachStmt *stmt;
      DeclContext *dc;
      Pattern *pattern;
      GenericEnvironment *packElementEnv;
      ForEachStmtInfo info;
    } forEachPreamble;

    PatternBindingDecl *patternBinding;
  };

  // If the pattern contains a single variable that has an attached
  // property wrapper, set up the initializer expression to initialize
  // the backing storage.
  void maybeApplyPropertyWrapper();

public:
  SyntacticElementTarget(Expr *expr, DeclContext *dc,
                         ContextualTypePurpose contextualPurpose,
                         Type convertType, bool isDiscarded)
      : SyntacticElementTarget(
            expr, dc, ContextualTypeInfo(convertType, contextualPurpose),
            isDiscarded) {}

  SyntacticElementTarget(Expr *expr, DeclContext *dc,
                         ContextualTypeInfo contextualInfo, bool isDiscarded);

  SyntacticElementTarget(ClosureExpr *closure, Type convertType) {
    kind = Kind::closure;
    this->closure.closure = closure;
    this->closure.convertType = convertType;
  }

  SyntacticElementTarget(AnyFunctionRef fn)
      : SyntacticElementTarget(fn, fn.getBody()) {}

  SyntacticElementTarget(StmtCondition stmtCondition, DeclContext *dc) {
    kind = Kind::stmtCondition;
    this->stmtCondition.stmtCondition = stmtCondition;
    this->stmtCondition.dc = dc;
  }

  SyntacticElementTarget(AnyFunctionRef fn, BraceStmt *body) {
    kind = Kind::function;
    function.function = fn;
    function.body = body;
  }

  SyntacticElementTarget(CaseLabelItem *caseLabelItem, DeclContext *dc) {
    kind = Kind::caseLabelItem;
    this->caseLabelItem.caseLabelItem = caseLabelItem;
    this->caseLabelItem.dc = dc;
  }

  SyntacticElementTarget(PatternBindingDecl *patternBinding) {
    kind = Kind::patternBinding;
    this->patternBinding = patternBinding;
  }

  SyntacticElementTarget(VarDecl *uninitializedWrappedVar)
      : kind(Kind::uninitializedVar) {
    if (auto *PDB = uninitializedWrappedVar->getParentPatternBinding()) {
      uninitializedVar.binding = PDB;
      uninitializedVar.index =
          PDB->getPatternEntryIndexForVarDecl(uninitializedWrappedVar);
    } else {
      uninitializedVar.binding = nullptr;
      uninitializedVar.index = 0;
    }

    uninitializedVar.declaration = uninitializedWrappedVar;
    uninitializedVar.type = Type();
  }

  SyntacticElementTarget(PatternBindingDecl *binding, unsigned index,
                         Pattern *var, Type patternTy)
      : kind(Kind::uninitializedVar) {
    uninitializedVar.binding = binding;
    uninitializedVar.index = index;
    uninitializedVar.declaration = var;
    uninitializedVar.type = patternTy;
  }

  SyntacticElementTarget(ForEachStmt *stmt, DeclContext *dc,
                         GenericEnvironment *packElementEnv)
      : kind(Kind::forEachPreamble) {
    forEachPreamble.stmt = stmt;
    forEachPreamble.dc = dc;
    forEachPreamble.packElementEnv = packElementEnv;
  }

  /// Form a target for the initialization of a pattern from an expression.
  static SyntacticElementTarget
  forInitialization(Expr *initializer, DeclContext *dc, Type patternType,
                    Pattern *pattern);

  /// Form a target for the initialization of a pattern binding entry from
  /// an expression.
  static SyntacticElementTarget
  forInitialization(Expr *initializer, Type patternType,
                    PatternBindingDecl *patternBinding,
                    unsigned patternBindingIndex);

  /// Form an expression target for a ReturnStmt.
  static SyntacticElementTarget
  forReturn(ReturnStmt *returnStmt, Type contextTy, DeclContext *dc);

  /// Form a target for the preamble of a for-in loop, excluding its where
  /// clause and body.
  static SyntacticElementTarget
  forForEachPreamble(ForEachStmt *stmt, DeclContext *dc,
                     GenericEnvironment *packElementEnv = nullptr);

  /// Form a target for a property with an attached property wrapper that is
  /// initialized out-of-line.
  static SyntacticElementTarget
  forUninitializedWrappedVar(VarDecl *wrappedVar) {
    return {wrappedVar};
  }

  static SyntacticElementTarget forUninitializedVar(PatternBindingDecl *binding,
                                                    unsigned index,
                                                    Type patternTy) {
    return {binding, index, binding->getPattern(index), patternTy};
  }

  /// Form a target for a synthesized property wrapper initializer.
  static SyntacticElementTarget
  forPropertyWrapperInitializer(VarDecl *wrappedVar, DeclContext *dc,
                                Expr *initializer);

  /// Form a target for the match expression of an ExprPattern.
  static SyntacticElementTarget forExprPattern(ExprPattern *pattern);

  /// This is useful for code completion.
  ASTNode getAsASTNode() const {
    switch (kind) {
    case Kind::expression:
      return expression.expression;

    case Kind::closure:
      return closure.closure;

    case Kind::function: {
      auto ref = *getAsFunction();
      if (auto *func = ref.getAbstractFunctionDecl())
        return func;
      return ref.getAbstractClosureExpr();
    }

    case Kind::forEachPreamble:
      return getAsForEachStmt();

    case Kind::stmtCondition:
      return ASTNode();

    case Kind::caseLabelItem:
      return *getAsCaseLabelItem();

    case Kind::patternBinding:
      return getAsPatternBinding();

    case Kind::uninitializedVar:
      return getAsUninitializedVar();
    }
  }

  Expr *getAsExpr() const {
    switch (kind) {
    case Kind::expression:
      return expression.expression;

    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::uninitializedVar:
    case Kind::forEachPreamble:
      return nullptr;
    }
    llvm_unreachable("invalid expression type");
  }

  DeclContext *getDeclContext() const {
    switch (kind) {
    case Kind::expression:
      return expression.dc;

    case Kind::closure:
      return closure.closure;

    case Kind::function:
      return function.function.getAsDeclContext();

    case Kind::stmtCondition:
      return stmtCondition.dc;

    case Kind::caseLabelItem:
      return caseLabelItem.dc;

    case Kind::patternBinding:
      return patternBinding->getDeclContext();

    case Kind::uninitializedVar: {
      if (auto *wrappedVar = uninitializedVar.declaration.dyn_cast<VarDecl *>())
        return wrappedVar->getDeclContext();

      return uninitializedVar.binding->getInitContext(uninitializedVar.index);
    }

    case Kind::forEachPreamble:
      return forEachPreamble.dc;
    }
    llvm_unreachable("invalid decl context type");
  }

  /// Get the contextual type info for an expression target.
  ContextualTypeInfo getExprContextualTypeInfo() const {
    assert(kind == Kind::expression);
    return expression.contextualInfo;
  }

  /// Get the contextual type purpose for an expression target.
  ContextualTypePurpose getExprContextualTypePurpose() const {
    return getExprContextualTypeInfo().purpose;
  }

  /// Get the contextual type for an expression target.
  Type getExprContextualType() const {
    return getExprContextualTypeLoc().getType();
  }

  /// Get the contextual type for an expression target.
  TypeLoc getExprContextualTypeLoc() const {
    // For an @autoclosure parameter, the conversion type is
    // the result of the function type.
    auto typeLoc = getExprContextualTypeInfo().typeLoc;
    if (FunctionType *autoclosureParamType = getAsAutoclosureParamType())
      return TypeLoc(typeLoc.getTypeRepr(), autoclosureParamType->getResult());

    return typeLoc;
  }

  /// Retrieve the type to which an expression should be converted, or
  /// a NULL type if no conversion constraint should be generated.
  Type getExprConversionType() const {
    if (contextualTypeIsOnlyAHint())
      return Type();
    return getExprContextualType();
  }

  /// Retrieve the conversion type locator for the expression, or \c nullptr
  /// if it has not been set.
  ConstraintLocator *getExprConvertTypeLocator() const {
    return getExprContextualTypeInfo().locator;
  }

  /// Returns the autoclosure parameter type, or \c nullptr if the
  /// expression has a different kind of context.
  FunctionType *getAsAutoclosureParamType() const {
    if (getExprContextualTypePurpose() == CTP_AutoclosureDefaultParameter)
      return getExprContextualTypeInfo().getType()->castTo<FunctionType>();

    return nullptr;
  }

  void setExprConversionType(Type type) {
    assert(kind == Kind::expression);
    expression.contextualInfo.typeLoc = TypeLoc::withoutLoc(type);
  }

  void setExprConversionTypeLoc(TypeLoc type) {
    assert(kind == Kind::expression);
    expression.contextualInfo.typeLoc = type;
  }

  void setExprContextualTypePurpose(ContextualTypePurpose ctp) {
    assert(kind == Kind::expression);
    expression.contextualInfo.purpose = ctp;
  }

  /// Whether this target is for an initialization expression and pattern.
  bool isForInitialization() const {
    return kind == Kind::expression &&
           getExprContextualTypePurpose() == CTP_Initialization;
  }

  /// For a pattern initialization target, retrieve the pattern.
  Pattern *getInitializationPattern() const {
    if (kind == Kind::uninitializedVar)
      return uninitializedVar.declaration.get<Pattern *>();

    assert(isForInitialization());
    return expression.pattern;
  }

  ExprPattern *getExprPattern() const {
    assert(kind == Kind::expression);
    assert(getExprContextualTypePurpose() == CTP_ExprPattern);
    return cast<ExprPattern>(expression.pattern);
  }

  ReturnStmt *getParentReturnStmt() const {
    assert(kind == Kind::expression);
    return expression.parentReturnStmt;
  }

  Type getClosureContextualType() const {
    assert(kind == Kind::closure);
    return closure.convertType;
  }

  /// For a pattern initialization target, retrieve the contextual pattern.
  ContextualPattern getContextualPattern() const;

  /// Whether this target is for a for-in preamble, excluding the body.
  bool isForEachPreamble() const { return kind == Kind::forEachPreamble; }

  /// Whether this is an initialization for an Optional.Some pattern.
  bool isOptionalSomePatternInit() const {
    return isForInitialization() &&
           dyn_cast_or_null<OptionalSomePattern>(expression.pattern) &&
           !expression.pattern->isImplicit();
  }

  /// Check whether this is an initialization for `async let` pattern.
  bool isAsyncLetInitializer() const {
    if (!isForInitialization())
      return false;

    if (auto *PBD = getInitializationPatternBindingDecl())
      return PBD->isAsyncLet();
    return false;
  }

  /// Whether or not an opaque value placeholder should be injected into the
  /// first \c wrappedValue argument of an apply expression so the initializer
  /// expression can be turned into a property wrapper generator function.
  bool shouldInjectWrappedValuePlaceholder(ApplyExpr *apply) const {
    if (!isForInitialization())
      return false;

    auto *wrappedVar = expression.propertyWrapper.wrappedVar;
    if (!apply || !wrappedVar)
      return false;

    // Don't create property wrapper generator functions for static variables
    // and local variables with initializers.
    bool hasInit = expression.propertyWrapper.hasInitialWrappedValue;
    if (wrappedVar->isStatic() ||
        (hasInit && wrappedVar->getDeclContext()->isLocalContext()))
      return false;

    return expression.propertyWrapper.innermostWrappedValueInit == apply;
  }

  /// Whether this target is for initialization of a property wrapper
  /// with an initial wrapped value specified via \c = .
  bool propertyWrapperHasInitialWrappedValue() const {
    return (kind == Kind::expression &&
            expression.propertyWrapper.hasInitialWrappedValue);
  }

  /// Retrieve the wrapped variable when initializing a pattern with a
  /// property wrapper.
  VarDecl *getInitializationWrappedVar() const {
    assert(isForInitialization());
    return expression.propertyWrapper.wrappedVar;
  }

  PatternBindingDecl *getInitializationPatternBindingDecl() const {
    assert(isForInitialization());
    return expression.initialization.patternBinding;
  }

  unsigned getInitializationPatternBindingIndex() const {
    assert(isForInitialization());
    return expression.initialization.patternBindingIndex;
  }

  GenericEnvironment *getPackElementEnv() const {
    assert(isForEachPreamble());
    return forEachPreamble.packElementEnv;
  }

  const ForEachStmtInfo &getForEachStmtInfo() const {
    assert(isForEachPreamble());
    return forEachPreamble.info;
  }

  ForEachStmtInfo &getForEachStmtInfo() {
    assert(isForEachPreamble());
    return forEachPreamble.info;
  }

  /// Whether this context infers an opaque return type.
  bool infersOpaqueReturnType() const;

  /// Whether the contextual type is only a hint, rather than a type
  bool contextualTypeIsOnlyAHint() const;

  bool isDiscardedExpr() const {
    assert(kind == Kind::expression);
    return expression.isDiscarded;
  }

  void setExpr(Expr *expr) {
    assert(kind == Kind::expression);
    expression.expression = expr;
  }

  Pattern *getPattern() const {
    if (auto *pattern = getAsUninitializedVar())
      return pattern;

    if (isForInitialization())
      return getInitializationPattern();

    if (kind == Kind::forEachPreamble)
      return forEachPreamble.pattern;

    return nullptr;
  }

  void setPattern(Pattern *pattern) {
    if (kind == Kind::uninitializedVar) {
      assert(uninitializedVar.declaration.is<Pattern *>());
      uninitializedVar.declaration = pattern;
      return;
    }

    if (kind == Kind::forEachPreamble) {
      forEachPreamble.pattern = pattern;
      return;
    }

    switch (getExprContextualTypePurpose()) {
    case CTP_Initialization:
    case CTP_ForEachStmt:
    case CTP_ForEachSequence:
    case CTP_ExprPattern:
      break;
    default:
      assert(false && "Unexpected contextual type purpose");
      break;
    }
    expression.pattern = pattern;
  }

  std::optional<AnyFunctionRef> getAsFunction() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::uninitializedVar:
    case Kind::forEachPreamble:
      return std::nullopt;

    case Kind::function:
      return function.function;
    }
    llvm_unreachable("invalid function kind");
  }

  std::optional<StmtCondition> getAsStmtCondition() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::uninitializedVar:
    case Kind::forEachPreamble:
      return std::nullopt;

    case Kind::stmtCondition:
      return stmtCondition.stmtCondition;
    }
    llvm_unreachable("invalid statement kind");
  }

  std::optional<CaseLabelItem *> getAsCaseLabelItem() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::patternBinding:
    case Kind::uninitializedVar:
    case Kind::forEachPreamble:
      return std::nullopt;

    case Kind::caseLabelItem:
      return caseLabelItem.caseLabelItem;
    }
    llvm_unreachable("invalid case label type");
  }

  PatternBindingDecl *getAsPatternBinding() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::uninitializedVar:
    case Kind::forEachPreamble:
      return nullptr;

    case Kind::patternBinding:
      return patternBinding;
    }
    llvm_unreachable("invalid case label type");
  }

  VarDecl *getAsUninitializedWrappedVar() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::forEachPreamble:
      return nullptr;

    case Kind::uninitializedVar:
      return uninitializedVar.declaration.dyn_cast<VarDecl *>();
    }
    llvm_unreachable("invalid case label type");
  }

  Pattern *getAsUninitializedVar() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::forEachPreamble:
      return nullptr;

    case Kind::uninitializedVar:
      return uninitializedVar.declaration.dyn_cast<Pattern *>();
    }
    llvm_unreachable("invalid case label type");
  }

  ForEachStmt *getAsForEachStmt() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::uninitializedVar:
      return nullptr;

    case Kind::forEachPreamble:
      return forEachPreamble.stmt;
    }
    llvm_unreachable("invalid case label type");
  }

  Type getTypeOfUninitializedVar() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::forEachPreamble:
      return nullptr;

    case Kind::uninitializedVar:
      return uninitializedVar.type;
    }
    llvm_unreachable("invalid case label type");
  }

  PatternBindingDecl *getPatternBindingOfUninitializedVar() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::forEachPreamble:
      return nullptr;

    case Kind::uninitializedVar:
      return uninitializedVar.binding;
    }
    llvm_unreachable("invalid case label type");
  }

  unsigned getIndexOfUninitializedVar() const {
    switch (kind) {
    case Kind::expression:
    case Kind::closure:
    case Kind::function:
    case Kind::stmtCondition:
    case Kind::caseLabelItem:
    case Kind::patternBinding:
    case Kind::forEachPreamble:
      return 0;

    case Kind::uninitializedVar:
      return uninitializedVar.index;
    }
    llvm_unreachable("invalid case label type");
  }

  BraceStmt *getFunctionBody() const {
    assert(kind == Kind::function);
    return function.body;
  }

  void setFunctionBody(BraceStmt *stmt) {
    assert(kind == Kind::function);
    function.body = stmt;
  }

  /// Retrieve the source range of the target.
  SourceRange getSourceRange() const {
    switch (kind) {
    case Kind::expression: {
      auto range = expression.expression->getSourceRange();

      // For an initialization, include the pattern in the range too.
      if (isForInitialization()) {
        if (auto *pattern = getInitializationPattern()) {
          if (auto patternRange = pattern->getSourceRange()) {
            if (range.isInvalid()) {
              range = patternRange;
            } else {
              range.widen(patternRange);
            }
          }
        }
      }
      return range;
    }
    case Kind::closure:
      return closure.closure->getSourceRange();

    case Kind::function:
      return function.body->getSourceRange();

    case Kind::stmtCondition:
      return SourceRange(stmtCondition.stmtCondition.front().getStartLoc(),
                         stmtCondition.stmtCondition.back().getEndLoc());

    case Kind::caseLabelItem:
      return caseLabelItem.caseLabelItem->getSourceRange();

    case Kind::patternBinding:
      return patternBinding->getSourceRange();

    case Kind::uninitializedVar: {
      if (auto *wrappedVar =
              uninitializedVar.declaration.dyn_cast<VarDecl *>()) {
        return wrappedVar->getSourceRange();
      }
      return uninitializedVar.declaration.get<Pattern *>()->getSourceRange();
    }

    // For-in preamble target doesn't cover the body.
    case Kind::forEachPreamble:
      auto *stmt = forEachPreamble.stmt;
      SourceLoc startLoc = stmt->getForLoc();
      SourceLoc endLoc = stmt->getParsedSequence()->getEndLoc();

      if (auto *whereExpr = stmt->getWhere()) {
        endLoc = whereExpr->getEndLoc();
      }

      return {startLoc, endLoc};
    }
    llvm_unreachable("invalid target type");
  }

  /// Retrieve the source location for the target.
  SourceLoc getLoc() const {
    switch (kind) {
    case Kind::expression:
      return expression.expression->getLoc();

    case Kind::closure:
      return closure.closure->getLoc();

    case Kind::function:
      return function.function.getLoc();

    case Kind::stmtCondition:
      return stmtCondition.stmtCondition.front().getStartLoc();

    case Kind::caseLabelItem:
      return caseLabelItem.caseLabelItem->getStartLoc();

    case Kind::patternBinding:
      return patternBinding->getLoc();

    case Kind::uninitializedVar: {
      if (auto *wrappedVar =
              uninitializedVar.declaration.dyn_cast<VarDecl *>()) {
        return wrappedVar->getLoc();
      }
      return uninitializedVar.declaration.get<Pattern *>()->getLoc();
    }

    case Kind::forEachPreamble:
      return forEachPreamble.stmt->getStartLoc();
    }
    llvm_unreachable("invalid target type");
  }

  /// Marks the target as invalid, filling in ErrorTypes for the AST it
  /// represents.
  void markInvalid() const;

  /// Walk the contents of the application target.
  std::optional<SyntacticElementTarget> walk(ASTWalker &walker) const;
};

} // namespace constraints
} // namespace swift

#endif /* SWIFT_SEMA_SYNTACTIC_ELEMENT_TARGET_H */
