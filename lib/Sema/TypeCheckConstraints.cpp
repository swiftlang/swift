//===--- TypeCheckConstraints.cpp - Constraint-based Type Checking --------===//
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
//
// This file provides high-level entry points that use constraint
// systems for type checking, as well as a few miscellaneous helper
// functions that support the constraint system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <iterator>
#include <map>
#include <memory>
#include <utility>
#include <tuple>

using namespace swift;
using namespace constraints;

//===--------------------------------------------------------------------===//
// Type variable implementation.
//===--------------------------------------------------------------------===//
#pragma mark Type variable implementation

void TypeVariableType::Implementation::print(llvm::raw_ostream &OS) {
  getTypeVariable()->print(OS, PrintOptions());
}

SavedTypeVariableBinding::SavedTypeVariableBinding(TypeVariableType *typeVar)
  : TypeVar(typeVar), ParentOrFixed(typeVar->getImpl().ParentOrFixed) { }

void SavedTypeVariableBinding::restore() {
  TypeVar->getImpl().ParentOrFixed = ParentOrFixed;
}

ArchetypeType *TypeVariableType::Implementation::getArchetype() const {
  // Check whether we have a path that terminates at an archetype locator.
  if (!locator || locator->getPath().empty() ||
      locator->getPath().back().getKind() != ConstraintLocator::Archetype)
    return nullptr;

  // Retrieve the archetype.
  return locator->getPath().back().getArchetype();
}

// Only allow allocation of resolved overload set list items using the
// allocator in ASTContext.
void *ResolvedOverloadSetListItem::operator new(size_t bytes,
                                                ConstraintSystem &cs,
                                                unsigned alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

Type constraints::adjustLValueForReference(Type type, bool isAssignment,
                                           ASTContext &context) {
  LValueType::Qual quals = LValueType::Qual::Implicit;
  if (auto lv = type->getAs<LValueType>()) {
    // FIXME: The introduction of 'non-heap' here is an artifact of the type
    // checker's inability to model the address-of operator that carries the
    // heap bit from its input to its output while removing the 'implicit' bit.
    // When we actually apply the inferred types in a constraint system to a
    // concrete expression, the 'implicit' bits will be dropped and the
    // appropriate 'heap' bits will be re-introduced.
    return LValueType::get(lv->getObjectType(),
                           quals | lv->getQualifiers(),
                           context);
  }

  // For an assignment operator, the first parameter is an implicit inout.
  if (isAssignment) {
    if (auto funcTy = type->getAs<FunctionType>()) {
      Type inputTy;
      if (auto inputTupleTy = funcTy->getInput()->getAs<TupleType>()) {
        if (inputTupleTy->getFields().size() > 0) {
          auto &firstParam = inputTupleTy->getFields()[0];
          auto firstParamTy
            = adjustLValueForReference(firstParam.getType(), false, context);
          SmallVector<TupleTypeElt, 2> elements;
          elements.push_back(firstParam.getWithType(firstParamTy));
          elements.append(inputTupleTy->getFields().begin() + 1,
                          inputTupleTy->getFields().end());
          inputTy = TupleType::get(elements, context);
        } else {
          inputTy = funcTy->getInput();
        }
      } else {
        inputTy = adjustLValueForReference(funcTy->getInput(), false, context);
      }

      return FunctionType::get(inputTy, funcTy->getResult(),
                               funcTy->getExtInfo(),
                               context);
    }
  }

  return type;
}

bool constraints::computeTupleShuffle(TupleType *fromTuple, TupleType *toTuple,
                                      SmallVectorImpl<int> &sources,
                                      SmallVectorImpl<unsigned> &variadicArgs,
                                      bool sourceLabelsAreMandatory) {
  const int unassigned = -3;
  
  SmallVector<bool, 4> consumed(fromTuple->getFields().size(), false);
  sources.clear();
  variadicArgs.clear();
  sources.assign(toTuple->getFields().size(), unassigned);

  // Match up any named elements.
  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    const auto &toElt = toTuple->getFields()[i];

    // Skip unnamed elements.
    if (toElt.getName().empty())
      continue;

    // Find the corresponding named element.
    int matched = -1;
    {
      int index = 0;
      for (auto field : fromTuple->getFields()) {
        if (field.getName() == toElt.getName() && !consumed[index]) {
          matched = index;
          break;
        }
        ++index;
      }
    }
    if (matched == -1)
      continue;

    // Record this match.
    sources[i] = matched;
    consumed[matched] = true;
  }  

  // Resolve any unmatched elements.
  unsigned fromNext = 0, fromLast = fromTuple->getFields().size();
  auto skipToNextAvailableInput = [&] {
    while (fromNext != fromLast && consumed[fromNext])
      ++fromNext;
  };
  skipToNextAvailableInput();

  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    // Check whether we already found a value for this element.
    if (sources[i] != unassigned)
      continue;

    const auto &elt2 = toTuple->getFields()[i];

    // Variadic tuple elements match the rest of the input elements.
    if (elt2.isVararg()) {
      // Collect the remaining (unnamed) inputs.
      while (fromNext != fromLast) {
        // Labeled elements can't be adopted into varargs even if
        // they're non-mandatory.  There isn't a really strong reason
        // for this, though.
        if (fromTuple->getFields()[fromNext].hasName()) {
          return true;
        }

        variadicArgs.push_back(fromNext);
        consumed[fromNext] = true;
        skipToNextAvailableInput();
      }
      sources[i] = TupleShuffleExpr::FirstVariadic;
      break;
    }

    // If there aren't any more inputs, we can use a default argument.
    if (fromNext == fromLast) {
      if (elt2.hasInit()) {
        sources[i] = TupleShuffleExpr::DefaultInitialize;
        continue;
      }

      return true;
    }

    // Otherwise, assign this input to the next output element.

    // Complain if the input element is named and either the label is
    // mandatory or we're trying to match it with something with a
    // different label.
    if (fromTuple->getFields()[fromNext].hasName() &&
        (sourceLabelsAreMandatory || elt2.hasName())) {
      return true;
    }

    sources[i] = fromNext;
    consumed[fromNext] = true;
    skipToNextAvailableInput();
  }

  // Complain if we didn't reach the end of the inputs.
  if (fromNext != fromLast) {
    return true;
  }

  // If we got here, we should have claimed all the arguments.
  assert(std::find(consumed.begin(), consumed.end(), false) == consumed.end());
  return false;
}

Expr *ConstraintLocatorBuilder::trySimplifyToExpr() const {
  SmallVector<LocatorPathElt, 4> pathBuffer;
  Expr *anchor = getLocatorParts(pathBuffer);
  ArrayRef<LocatorPathElt> path = pathBuffer;

  Expr *targetAnchor;
  SmallVector<LocatorPathElt, 4> targetPathBuffer;
  SourceRange range1, range2;

  simplifyLocator(anchor, path, targetAnchor, targetPathBuffer, range1, range2);
  return (path.empty() ? anchor : nullptr);
}

bool constraints::hasMandatoryTupleLabels(Expr *e) {
  return isa<TupleExpr>(e->getSemanticsProvidingExpr());
}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//

static unsigned getNumArgs(ValueDecl *value) {
  if (!isa<FuncDecl>(value)) return ~0U;

  AnyFunctionType *fnTy = value->getType()->castTo<AnyFunctionType>();
  if (value->getDeclContext()->isTypeContext())
    fnTy = fnTy->getResult()->castTo<AnyFunctionType>();
  Type argTy = fnTy->getInput();
  if (auto tuple = argTy->getAs<TupleType>()) {
    return tuple->getFields().size();
  } else {
    return 1;
  }
}

static bool matchesDeclRefKind(ValueDecl *value, DeclRefKind refKind) {
  if (value->getType()->is<ErrorType>())
    return true;

  switch (refKind) {
      // An ordinary reference doesn't ignore anything.
    case DeclRefKind::Ordinary:
      return true;

      // A binary-operator reference only honors FuncDecls with a certain type.
    case DeclRefKind::BinaryOperator:
      return (getNumArgs(value) == 2);

    case DeclRefKind::PrefixOperator:
      return (!value->getAttrs().isPostfix() && getNumArgs(value) == 1);

    case DeclRefKind::PostfixOperator:
      return (value->getAttrs().isPostfix() && getNumArgs(value) == 1);
  }
  llvm_unreachable("bad declaration reference kind");
}

/// BindName - Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression.  Context is the DeclContext used
/// for the lookup.
static Expr *BindName(UnresolvedDeclRefExpr *UDRE, DeclContext *Context,
                      TypeChecker &TC) {
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  Identifier Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();

  // Perform standard value name lookup.
  UnqualifiedLookup Lookup(Name, Context, &TC, UDRE->getLoc());

  if (!Lookup.isSuccess()) {
    TC.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return new (TC.Context) ErrorExpr(Loc);
  }

  // FIXME: Need to refactor the way we build an AST node from a lookup result!

  if (Lookup.Results.size() == 1 &&
      Lookup.Results[0].Kind == UnqualifiedLookupResult::ModuleName) {
    ModuleType *MT = ModuleType::get(Lookup.Results[0].getNamedModule());
    return new (TC.Context) ModuleExpr(Loc, MT);
  }

  bool AllDeclRefs = true;
  SmallVector<ValueDecl*, 4> ResultValues;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
      case UnqualifiedLookupResult::MemberProperty:
      case UnqualifiedLookupResult::MemberFunction:
      case UnqualifiedLookupResult::MetatypeMember:
      case UnqualifiedLookupResult::ExistentialMember:
      case UnqualifiedLookupResult::ArchetypeMember:
      case UnqualifiedLookupResult::MetaArchetypeMember:
      case UnqualifiedLookupResult::ModuleName:
        // Types are never referenced with an implicit 'self'.
        if (!isa<TypeDecl>(Result.getValueDecl())) {
          AllDeclRefs = false;
          break;
        }

        SWIFT_FALLTHROUGH;

      case UnqualifiedLookupResult::ModuleMember:
      case UnqualifiedLookupResult::LocalDecl: {
        ValueDecl *D = Result.getValueDecl();
        if (matchesDeclRefKind(D, UDRE->getRefKind()))
          ResultValues.push_back(D);
        break;
      }
    }
  }
  if (AllDeclRefs) {
    // Diagnose uses of operators that found no matching candidates.
    if (ResultValues.empty()) {
      assert(UDRE->getRefKind() != DeclRefKind::Ordinary);
      TC.diagnose(Loc, diag::use_nonmatching_operator, Name,
                  UDRE->getRefKind() == DeclRefKind::BinaryOperator ? 0 :
                  UDRE->getRefKind() == DeclRefKind::PrefixOperator ? 1 : 2);
      return new (TC.Context) ErrorExpr(Loc);
    }

    return TC.buildRefExpr(ResultValues, Loc, UDRE->isImplicit(),
                           UDRE->isSpecialized());
  }

  ResultValues.clear();
  bool AllMemberRefs = true;
  ValueDecl *Base = 0;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
      case UnqualifiedLookupResult::MemberProperty:
      case UnqualifiedLookupResult::MemberFunction:
      case UnqualifiedLookupResult::MetatypeMember:
      case UnqualifiedLookupResult::ExistentialMember:
        ResultValues.push_back(Result.getValueDecl());
        if (Base && Result.getBaseDecl() != Base) {
          AllMemberRefs = false;
          break;
        }
        Base = Result.getBaseDecl();
        break;
      case UnqualifiedLookupResult::ModuleMember:
      case UnqualifiedLookupResult::LocalDecl:
      case UnqualifiedLookupResult::ModuleName:
        AllMemberRefs = false;
        break;
      case UnqualifiedLookupResult::MetaArchetypeMember:
      case UnqualifiedLookupResult::ArchetypeMember:
        // FIXME: We need to extend OverloadedMemberRefExpr to deal with this.
        llvm_unreachable("Archetype members in overloaded member references");
        break;
    }
  }

  if (AllMemberRefs) {
    Expr *BaseExpr;
    if (auto NTD = dyn_cast<NominalTypeDecl>(Base)) {
      Type BaseTy = MetaTypeType::get(NTD->getDeclaredTypeInContext(),
                                      TC.Context);
      BaseExpr = new (TC.Context) MetatypeExpr(nullptr, Loc, BaseTy);
    } else {
      BaseExpr = new (TC.Context) DeclRefExpr(Base, Loc, /*implicit=*/true);
    }
    return new (TC.Context) UnresolvedDotExpr(BaseExpr, SourceLoc(), Name, Loc,
                                              UDRE->isImplicit());
  }
  
  llvm_unreachable("Can't represent lookup result");
}

namespace {
  class PreCheckExpression : public ASTWalker {
    TypeChecker &TC;
    DeclContext *DC;
    bool RequiresAnotherPass = false;

  public:
    PreCheckExpression(TypeChecker &tc, DeclContext *dc) : TC(tc), DC(dc) { }

    /// Determine whether pre-check requires another pass.
    bool requiresAnotherPass() const { return RequiresAnotherPass; }

    // Reset internal state for another pass.
    void reset() {
      RequiresAnotherPass = false;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        // Validate the parameters.
        if (TC.typeCheckPattern(closure->getParams(), DC, true)) {
          expr->setType(ErrorType::get(TC.Context));
          return { false, expr };
        }

        // Validate the result type, if present.
        if (closure->hasExplicitResultType() &&
            TC.validateType(closure->getExplicitResultTypeLoc(), DC)) {
          expr->setType(ErrorType::get(TC.Context));
          return { false, expr };
        }
        
        return { closure->hasSingleExpressionBody(), expr };
      }

      if (auto unresolved = dyn_cast<UnresolvedDeclRefExpr>(expr)) {
        return { true, BindName(unresolved, DC, TC) };
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      // Fold sequence expressions.
      if (auto seqExpr = dyn_cast<SequenceExpr>(expr)) {
        return TC.foldSequence(seqExpr, DC);
      }

      // Type check the type in an array new expression.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        // FIXME: Check that the element type has a default constructor.
        
        if (TC.validateType(newArray->getElementTypeLoc(), DC,
                            /*allowUnboundGenerics=*/true))
          return nullptr;

        // Check array bounds. They are subproblems that don't interact with
        // the surrounding expression context.
        for (unsigned i = newArray->getBounds().size(); i != 1; --i) {
          auto &bound = newArray->getBounds()[i-1];
          if (!bound.Value)
            continue;

          // All inner bounds must be constant.
          if (TC.typeCheckArrayBound(bound.Value, /*requireConstant=*/true, DC))
            return nullptr;
        }

        // The outermost bound does not need to be constant.
        if (TC.typeCheckArrayBound(newArray->getBounds()[0].Value,
                                   /*requireConstant=*/false, DC))
          return nullptr;

        return expr;
      }

      // Type check the type parameters in an UnresolvedSpecializeExpr.
      if (auto us = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
        for (TypeLoc &type : us->getUnresolvedParams()) {
          if (TC.validateType(type, DC)) {
            TC.diagnose(us->getLAngleLoc(),
                        diag::while_parsing_as_left_angle_bracket);
            return nullptr;
          }
        }
        return expr;
      }

      // For a coercion "x as T", check the cast first.
      if (auto cast = dyn_cast<ConditionalCheckedCastExpr>(expr)) {
        // If there is no subexpression, the sequence hasn't been folded yet.
        // We'll require another pass.
        if (!cast->getSubExpr()) {
          RequiresAnotherPass = true;
          return cast;
        }

        // Validate the type.
        if (TC.validateType(cast->getCastTypeLoc(), DC,
                            /*allowUnboundGenerics=*/true))
          return nullptr;

        return checkAsCastExpr(cast);
      }

      // For a dynamic type check "x is T", check it first.
      if (auto isa = dyn_cast<IsaExpr>(expr)) {
        // If there is no subexpression, the sequence hasn't been folded yet.
        // We'll require another pass.
        if (!isa->getSubExpr()) {
          RequiresAnotherPass = true;
          return isa;
        }

        // Validate the type.
        if (TC.validateType(isa->getCastTypeLoc(), DC,
                            /*allowUnboundGenerics=*/true))
          return nullptr;

        CheckedCastKind castKind = checkCheckedCastExpr(isa);
        switch (castKind) {
          // Invalid type check.
        case CheckedCastKind::Unresolved:
          return nullptr;
          // Check is trivially true.
        case CheckedCastKind::Coercion:
          TC.diagnose(isa->getLoc(), diag::isa_is_always_true,
                      isa->getSubExpr()->getType(),
                      isa->getCastTypeLoc().getType());
          break;

          // Valid checks.
        case CheckedCastKind::Downcast:
        case CheckedCastKind::SuperToArchetype:
        case CheckedCastKind::ArchetypeToArchetype:
        case CheckedCastKind::ArchetypeToConcrete:
        case CheckedCastKind::ExistentialToArchetype:
        case CheckedCastKind::ExistentialToConcrete:
        case CheckedCastKind::ConcreteToArchetype:
        case CheckedCastKind::ConcreteToUnrelatedExistential:
          isa->setCastKind(castKind);
          break;
        }
        return isa;
      }

      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      // Never walk into statements.
      return { false, stmt };
    }

  private:
    /// Expression type-checking listener for checking a cast.
    class CastCheckListener : public ExprTypeCheckListener {
      Type &ToType;

    public:
      explicit CastCheckListener(Type &toType) : ToType(toType) { }

      virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
        // Open up the type we're casting to.
        ToType = cs.openType(ToType);

        // Either convert the expression to the given type or perform a
        // checked cast to the given type.
        auto fromType = expr->getType();
        auto locator = cs.getConstraintLocator(expr, { });
        Constraint *constraints[2] = {
          Constraint::create(cs, ConstraintKind::Conversion, fromType, ToType,
                             Identifier(), locator),
          Constraint::create(cs, ConstraintKind::CheckedCast, fromType, ToType,
                             Identifier(), locator),
        };
        cs.addConstraint(Constraint::createDisjunction(cs, constraints,
                                                       locator));

        return false;
      }

      void solvedConstraints(Solution &solution) {
        // Simplify the type we're converting to.
        ConstraintSystem &cs = solution.getConstraintSystem();
        ToType = solution.simplifyType(cs.getTypeChecker(), ToType);
      }
    };

    /// Type-check a checked cast expression.
    CheckedCastKind checkCheckedCastExpr(CheckedCastExpr *expr) {
      // Simplify the type we're converting to.
      Type toType = expr->getCastTypeLoc().getType();

      // Type-check the subexpression.
      CastCheckListener listener(toType);
      Expr *sub = expr->getSubExpr();
      if (TC.typeCheckExpression(sub, DC, Type(), /*discardedExpr=*/false,
                                 FreeTypeVariableBinding::Disallow,
                                 &listener)) {
        return CheckedCastKind::Unresolved;
      }

      sub = TC.coerceToRValue(sub);
      if (!sub) {
        return CheckedCastKind::Unresolved;
      }
      expr->setSubExpr(sub);

      Type fromType = sub->getType();
      expr->getCastTypeLoc().setType(toType);
      return TC.typeCheckCheckedCast(fromType, toType, DC,
                                     expr->getLoc(),
                                     sub->getSourceRange(),
                                     expr->getCastTypeLoc().getSourceRange(),
                                     [&](Type commonTy) -> bool {
                                       return TC.convertToType(sub, commonTy,
                                                               DC);
                                     });
    }

    Expr *checkAsCastExpr(CheckedCastExpr *expr) {
      CheckedCastKind castKind = checkCheckedCastExpr(expr);
      Type toType = expr->getCastTypeLoc().getType();
      switch (castKind) {
        /// Invalid cast.
      case CheckedCastKind::Unresolved:
        return nullptr;
        /// Cast trivially succeeds. Emit a fixit and reduce to a coercion.
      case CheckedCastKind::Coercion: {
        // This is a coercion. Convert the subexpression.
        Expr *sub = expr->getSubExpr();
        bool failed = TC.convertToType(sub, toType, DC);
        (void)failed;
        assert(!failed && "Not convertible?");

        // Transmute the checked cast into a coercion expression.
        Expr *result = new (TC.Context) CoerceExpr(sub, expr->getLoc(),
                                                   expr->getCastTypeLoc());

        // The result type is the type we're converting to.
        result->setType(toType);
        return result;
      }

      // Valid casts.
      case CheckedCastKind::Downcast:
      case CheckedCastKind::SuperToArchetype:
      case CheckedCastKind::ArchetypeToArchetype:
      case CheckedCastKind::ArchetypeToConcrete:
      case CheckedCastKind::ExistentialToArchetype:
      case CheckedCastKind::ExistentialToConcrete:
      case CheckedCastKind::ConcreteToArchetype:
      case CheckedCastKind::ConcreteToUnrelatedExistential:
        expr->setCastKind(castKind);
        break;
      }
      return expr;
    }
  };
}

/// \brief Clean up the given ill-formed expression, removing any references
/// to type variables and setting error types on erroneous expression nodes.
static Expr *cleanupIllFormedExpression(ASTContext &context,
                                        ConstraintSystem *cs, Expr *expr) {
  class CleanupIllFormedExpression : public ASTWalker {
    ASTContext &context;
    ConstraintSystem *cs;

  public:
    CleanupIllFormedExpression(ASTContext &context, ConstraintSystem *cs)
      : context(context), cs(cs) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        SmallVector<VarDecl *, 6> Params;
        closure->getParams()->collectVariables(Params);
        for (auto VD : Params) {
          if (VD->hasType()) {
            Type T = VD->getType();
            if (cs)
              T = cs->simplifyType(T);
            if (T->hasTypeVariable()) {
              T = ErrorType::get(context);
              VD->setInvalid();
            }
            VD->overwriteType(T);
          } else {
            VD->setType(ErrorType::get(context));
            VD->setInvalid();
          }
        }
        if (!closure->hasSingleExpressionBody()) {
          return { false, walkToExprPost(expr) };
        }

        return { true, expr };
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      Type type;
      if (expr->getType()) {
        type = expr->getType();
        if (cs)
          type = cs->simplifyType(type);
      }

      if (!type || type->hasTypeVariable())
        expr->setType(ErrorType::get(context));
      else
        expr->setType(type);
      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      // Never walk into statements.
      return { false, stmt };
    }
  };

  if (!expr)
    return expr;
  
  return expr->walk(CleanupIllFormedExpression(context, cs));
}

namespace {
  /// \brief RAII object that cleans up the given expression if not explicitly
  /// disabled.
  class CleanupIllFormedExpressionRAII {
    ConstraintSystem &cs;
    Expr **expr;

  public:
    CleanupIllFormedExpressionRAII(ConstraintSystem &cs, Expr *&expr)
      : cs(cs), expr(&expr) { }

    ~CleanupIllFormedExpressionRAII() {
      if (expr) {
        *expr = cleanupIllFormedExpression(cs.getASTContext(), &cs, *expr);
      }
    }

    /// \brief Disable the cleanup of this expression; it doesn't need it.
    void disable() {
      expr = nullptr;
    }
  };
}

/// Pre-check the expression, validating any types that occur in the
/// expression and folding sequence expressions.
static bool preCheckExpression(TypeChecker &tc, Expr *&expr, DeclContext *dc) {
  PreCheckExpression preCheck(tc, dc);
  do {
    // Perform the pre-check.
    preCheck.reset();
    if (auto result = expr->walk(preCheck)) {
      expr = result;
      continue;
    }

    // Pre-check failed. Clean up and return.
    expr = cleanupIllFormedExpression(dc->getASTContext(), nullptr, expr);
    return true;
  } while (preCheck.requiresAnotherPass());

  return false;
}

ExprTypeCheckListener::~ExprTypeCheckListener() { }

bool ExprTypeCheckListener::builtConstraints(ConstraintSystem &cs, Expr *expr) {
  return false;
}


void ExprTypeCheckListener::solvedConstraints(Solution &solution) { }

Expr *ExprTypeCheckListener::appliedSolution(Solution &solution, Expr *expr) {
  return expr;
}

#pragma mark High-level entry points
bool TypeChecker::typeCheckExpression(
       Expr *&expr, DeclContext *dc,
       Type convertType,
       bool discardedExpr,
       FreeTypeVariableBinding allowFreeTypeVariables,
       ExprTypeCheckListener *listener) {
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  if (preCheckExpression(*this, expr, dc))
    return true;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraints(expr))
    expr = generatedExpr;
  else {
    return true;
  }

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType,
                     cs.getConstraintLocator(expr, { }));
  }

  // Notify the listener that we've built the constraint system.
  if (listener && listener->builtConstraints(cs, expr)) {
    return true;
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable, allowFreeTypeVariables)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      performExprDiagnostics(*this, expr);
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    performExprDiagnostics(*this, expr);
    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Notify the listener that we have a solution.
  if (listener) {
    listener->solvedConstraints(solution);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolution(solution, expr);
  if (!result) {
    performExprDiagnostics(*this, expr);
    // Failure already diagnosed, above, as part of applying the solution.
   return true;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution.coerceToType(result, convertType,
                                   cs.getConstraintLocator(expr, { }));
    if (!result) {
      performExprDiagnostics(*this, expr);
      return true;
    }
  } else if (auto lvalueType = result->getType()->getAs<LValueType>()) {
    if (!lvalueType->getQualifiers().isImplicit()) {
      // We explicitly took a reference to the result, but didn't use it.
      // Complain and emit a Fix-It to zap the '&'.
      auto addressOf = cast<AddressOfExpr>(result->getSemanticsProvidingExpr());
      diagnose(addressOf->getLoc(), diag::reference_non_inout,
               lvalueType->getObjectType())
        .highlight(addressOf->getSubExpr()->getSourceRange())
        .fixItRemove(SourceRange(addressOf->getLoc()));

      // Strip the address-of expression.
      result = addressOf->getSubExpr();
      lvalueType = result->getType()->getAs<LValueType>();
    } 

    if (lvalueType && !discardedExpr) {
      // We referenced an lvalue. Load it.
      assert(lvalueType->getQualifiers().isImplicit() &&
             "Explicit lvalue diagnosed above");
      result = new (Context) LoadExpr(result, lvalueType->getObjectType());
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  // If there's a listener, notify it that we've applied the solution.
  if (listener) {
    result = listener->appliedSolution(solution, result);
    if (!result) {
      performExprDiagnostics(*this, expr);
      return true;
    }
  }

  performExprDiagnostics(*this, result);

  expr = result;
  cleanup.disable();
  return false;
}

bool TypeChecker::typeCheckExpressionShallow(Expr *&expr, DeclContext *dc,
                                             Type convertType) {
  PrettyStackTraceExpr stackTrace(Context, "shallow type-checking", expr);

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraintsShallow(expr))
    expr = generatedExpr;
  else
    return true;

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType,
                     cs.getConstraintLocator(expr, { }));
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolutionShallow(solution, expr);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
    return true;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution.coerceToType(result, convertType,
                                   cs.getConstraintLocator(expr, { }));
    if (!result) {
      return true;
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  expr = result;
  cleanup.disable();
  return false;
}

bool TypeChecker::typeCheckBinding(PatternBindingDecl *binding) {
  /// Type checking listener for pattern binding initializers.
  class BindingListener : public ExprTypeCheckListener {
    /// The pattern binding declaration whose initializer we're checking.
    PatternBindingDecl *Binding;

    /// The locator we're using.
    ConstraintLocator *Locator;

    /// The type of the pattern.
    Type PatternType;

  public:
    explicit BindingListener(PatternBindingDecl *binding) : Binding(binding) { }

    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // Save the locator we're using for the expression.
      Locator = cs.getConstraintLocator(expr, { });

      // Collect constraints from the pattern.
      auto pattern = Binding->getPattern();
      PatternType = cs.generateConstraints(pattern, Locator);
      if (!PatternType)
        return true;

      // Add a conversion constraint between the types.
      cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                       PatternType, Locator);
      return false;
    }

    virtual Expr *appliedSolution(Solution &solution, Expr *expr) {
      // Figure out what type the constraints decided on.
      auto &tc = solution.getConstraintSystem().getTypeChecker();
      PatternType = solution.simplifyType(tc, PatternType);

      // Convert the initializer to the type of the pattern.
      expr = solution.coerceToType(expr, PatternType, Locator);
      if (!expr) {
        return nullptr;
      }

      // Force the initializer to be materializable.
      // FIXME: work this into the constraint system
      expr = tc.coerceToMaterializable(expr);

      // Apply the solution to the pattern as well.
      Pattern *pattern = Binding->getPattern();
      if (tc.coerceToType(pattern, Binding->getDeclContext(),
                          expr->getType(), /*allowOverride=*/true)) {
        return nullptr;
      }
      Binding->setPattern(pattern);
      Binding->setInit(expr, /*checked=*/true);
      return expr;
    }
  };

  BindingListener listener(binding);
  Expr *init = binding->getInit();
  assert(init && "type-checking an uninitialized binding?");
  return typeCheckExpression(init, binding->getDeclContext(), Type(),
                             /*discardedExpr=*/false,
                             FreeTypeVariableBinding::Disallow,
                             &listener);
}

/// \brief Compute the rvalue type of the given expression, which is the
/// destination of an assignment statement.
Type ConstraintSystem::computeAssignDestType(Expr *dest, SourceLoc equalLoc) {
  if (TupleExpr *TE = dyn_cast<TupleExpr>(dest)) {
    auto &ctx = getASTContext();
    SmallVector<TupleTypeElt, 4> destTupleTypes;
    for (unsigned i = 0; i != TE->getNumElements(); ++i) {
      Expr *subExpr = TE->getElement(i);
      Type elemTy = computeAssignDestType(subExpr, equalLoc);
      if (!elemTy)
        return Type();
      destTupleTypes.push_back(TupleTypeElt(elemTy, TE->getElementName(i)));
    }

    return TupleType::get(destTupleTypes, ctx);
  }

  Type destTy = simplifyType(dest->getType());
  if (LValueType *destLV = destTy->getAs<LValueType>()) {
    // If the destination is a settable lvalue, we're good; get its object type.
    if (!destLV->isSettable()) {
      // FIXME: error message refers to "variable or subscript" instead of
      // saying which one it is.
      getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_settable)
        .highlight(dest->getSourceRange());
      return Type();
    }
    destTy = destLV->getObjectType();
  } else if (auto typeVar = dyn_cast<TypeVariableType>(destTy.getPointer())) {
    // The destination is a type variable. This type variable must be an
    // lvalue type, which we enforce via a subtyping relationship with
    // [inout(implicit, settable)] T, where T is a fresh type variable that
    // will be the object type of this particular expression type.
    auto objectTv = createTypeVariable(
                      getConstraintLocator(dest,
                                           ConstraintLocator::AssignDest),
                      TVO_CanBindToLValue);
    auto refTv = LValueType::get(objectTv,
                                 LValueType::Qual::Implicit,
                                 getASTContext());
    addConstraint(ConstraintKind::Subtype, typeVar, refTv);
    destTy = objectTv;
  } else {
    if (!destTy->is<ErrorType>())
      getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_lvalue)
        .highlight(dest->getSourceRange());

    return Type();
  }
  
  return destTy;
}

bool TypeChecker::typeCheckCondition(Expr *&expr, DeclContext *dc) {
  /// Expression type checking listener for conditions.
  class ConditionListener : public ExprTypeCheckListener {
    Expr *OrigExpr;

  public:
    // Add the appropriate LogicValue constraint.
    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // Save the original expression.
      OrigExpr = expr;

      // If the expression has type Builtin.Int1 (or an l-value with that
      // object type), go ahead and special-case that.  This doesn't need
      // to be deeply principled because builtin types are not user-facing.
      auto rvalueType = expr->getType()->getRValueType();
      if (rvalueType->isBuiltinIntegerType(1)) {
        cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                         rvalueType);
        return false;
      }

      // Otherwise, the result must be a LogicValue.
      auto &tc = cs.getTypeChecker();
      auto logicValueProto = tc.getProtocol(expr->getLoc(),
                                            KnownProtocolKind::LogicValue);
      if (!logicValueProto) {
        return true;
      }

      cs.addConstraint(ConstraintKind::ConformsTo, expr->getType(),
                       logicValueProto->getDeclaredType(),
                       cs.getConstraintLocator(OrigExpr, { }));
      return false;
    }

    // Convert the result to a logic value.
    virtual Expr *appliedSolution(constraints::Solution &solution,
                                  Expr *expr) {
      auto &cs = solution.getConstraintSystem();
      return solution.convertToLogicValue(expr,
                                          cs.getConstraintLocator(OrigExpr,
                                                                  { }));
    }
  };

  ConditionListener listener;
  return typeCheckExpression(expr, dc, Type(), /*discardedExpr=*/false,
                             FreeTypeVariableBinding::Disallow,
                             &listener);
}

bool TypeChecker::typeCheckArrayBound(Expr *&expr, bool constantRequired,
                                      DeclContext *dc) {
  PrettyStackTraceExpr stackTrace(Context, "type-checking array bound", expr);

  // If it's an integer literal expression, just convert the type directly.
  if (auto lit = dyn_cast<IntegerLiteralExpr>(
                   expr->getSemanticsProvidingExpr())) {
    // FIXME: the choice of 64-bit is rather arbitrary.
    expr->setType(BuiltinIntegerType::get(64, Context));

    // Constant array bounds must be non-zero.
    if (constantRequired) {
      uint64_t size = lit->getValue().getZExtValue();
      if (size == 0) {
        diagnose(lit->getLoc(), diag::new_array_bound_zero)
        .highlight(lit->getSourceRange());
        return nullptr;
      }
    }

    return false;
  }

  // Otherwise, if a constant expression is required, fail.
  if (constantRequired) {
    diagnose(expr->getLoc(), diag::non_constant_array)
      .highlight(expr->getSourceRange());
    return true;
  }

  /// Expression type checking listener for array bounds.
  class ArrayBoundListener : public ExprTypeCheckListener {
    Expr *OrigExpr;

  public:
    // Add the appropriate ArrayBound constraint.
    // Add the appropriate LogicValue constraint.
    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // Save the original expression.
      OrigExpr = expr;

      // The result must be an ArrayBound.
      auto &tc = cs.getTypeChecker();
      auto arrayBoundProto = tc.getProtocol(expr->getLoc(),
                                            KnownProtocolKind::ArrayBound);
      if (!arrayBoundProto) {
        return true;
      }

      cs.addConstraint(ConstraintKind::ConformsTo, expr->getType(),
                       arrayBoundProto->getDeclaredType(),
                       cs.getConstraintLocator(OrigExpr, { }));
      return false;
    }

    // Convert the result to an array bound.
    virtual Expr *appliedSolution(constraints::Solution &solution,
                                  Expr *expr) {
      auto &cs = solution.getConstraintSystem();
      return solution.convertToArrayBound(expr,
                                         cs.getConstraintLocator(OrigExpr,
                                                                 { }));
    }
  };

  ArrayBoundListener listener;
  return typeCheckExpression(expr, dc, Type(), /*discardedExpr=*/false,
                             FreeTypeVariableBinding::Disallow, &listener);
}

/// Find the '~=` operator that can compare an expression inside a pattern to a
/// value of a given type.
bool TypeChecker::typeCheckExprPattern(ExprPattern *EP, DeclContext *DC,
                                       Type rhsType) {
  PrettyStackTracePattern stackTrace(Context, "type-checking", EP);

  // Create a variable to stand in for the RHS value.
  auto *matchVar = new (Context) VarDecl(/*static*/ false,
                                         EP->getLoc(),
                                         Context.getIdentifier("$match"),
                                         rhsType,
                                         DC);
  EP->setMatchVar(matchVar);
  
  // Find '~=' operators for the match.
  // First try the current context.
  auto matchOperator = Context.getIdentifier("~=");
  UnqualifiedLookup matchLookup(matchOperator, DC, this);
  // If that doesn't work, fall back to the stdlib. Some contexts (viz.
  // Clang modules) don't normally see the stdlib.
  // FIXME: There might be better ways to do this.
  if (!matchLookup.isSuccess()) {
    matchLookup = UnqualifiedLookup(matchOperator,
                                    Context.getStdlibModule(),
                                    this);
  }
  
  if (!matchLookup.isSuccess()) {
    diagnose(EP->getLoc(), diag::no_match_operator);
    return true;
  }
  
  SmallVector<ValueDecl*, 4> choices;
  for (auto &result : matchLookup.Results) {
    if (!result.hasValueDecl())
      continue;
    choices.push_back(result.getValueDecl());
  }
  
  if (choices.empty()) {
    diagnose(EP->getLoc(), diag::no_match_operator);
    return true;
  }
  
  // Build the 'expr ~= var' expression.
  auto *matchOp = buildRefExpr(choices, EP->getLoc(), /*Implicit=*/true);
  auto *matchVarRef = new (Context) DeclRefExpr(matchVar,
                                                EP->getLoc(),
                                                /*Implicit=*/true);
  
  Expr *matchArgElts[] = {EP->getSubExpr(), matchVarRef};
  auto *matchArgs
    = new (Context) TupleExpr(EP->getSubExpr()->getSourceRange().Start,
                    Context.AllocateCopy(MutableArrayRef<Expr*>(matchArgElts)),
                    nullptr,
                    EP->getSubExpr()->getSourceRange().End,
                    false, /*Implicit=*/true);
  
  Expr *matchCall = new (Context) BinaryExpr(matchOp, matchArgs,
                                             /*Implicit=*/true);
  
  // Check the expression as a condition.
  if (typeCheckCondition(matchCall, DC))
    return true;

  // Save the type-checked expression in the pattern.
  EP->setMatchExpr(matchCall);
  // Set the type on the pattern.
  EP->setType(rhsType);
  return false;
}

bool TypeChecker::isTrivialSubtypeOf(Type type1, Type type2, DeclContext *dc) {
  // FIXME: Egregious hack due to checkClassOverrides being awful.
  if (type1->is<PolymorphicFunctionType>() ||
      type2->is<PolymorphicFunctionType>())
    return false;

  ConstraintSystem cs(*this, dc);
  cs.addConstraint(ConstraintKind::TrivialSubtype, type1, type2);
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isSubtypeOf(Type type1, Type type2, DeclContext *dc) {
  ConstraintSystem cs(*this, dc);
  cs.addConstraint(ConstraintKind::Subtype, type1, type2);
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isConvertibleTo(Type type1, Type type2, DeclContext *dc) {
  ConstraintSystem cs(*this, dc);
  cs.addConstraint(ConstraintKind::Conversion, type1, type2);
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isSubstitutableFor(Type type, ArchetypeType *archetype,
                                     DeclContext *dc) {
  ConstraintSystem cs(*this, dc);

  // Add all of the requirements of the archetype to the given type.
  // FIXME: Short-circuit if any of the constraints fails.
  if (archetype->requiresClass() && !type->mayHaveSuperclass())
    return false;

  if (auto superclass = archetype->getSuperclass()) {
    cs.addConstraint(ConstraintKind::TrivialSubtype, type, superclass);
  }
  for (auto proto : archetype->getConformsTo()) {
    cs.addConstraint(ConstraintKind::ConformsTo, type,
                     proto->getDeclaredType());
  }

  // Solve the system.
  SmallVector<Solution, 1> solution;
  return !cs.solve(solution);
}

Expr *TypeChecker::coerceToRValue(Expr *expr) {
  // If we already have an rvalue, we're done.
  auto lvalueTy = expr->getType()->getAs<LValueType>();
  if (!lvalueTy)
    return expr;

  // Can't load from an explicit lvalue.
  if (auto addrOf = dyn_cast<AddressOfExpr>(expr->getSemanticsProvidingExpr())){
    diagnose(expr->getLoc(), diag::load_of_explicit_lvalue,
             lvalueTy->getObjectType())
      .fixItRemove(SourceRange(expr->getLoc()));
    return coerceToRValue(addrOf->getSubExpr());
  }

  // Load the lvalue.
  return new (Context) LoadExpr(expr, lvalueTy->getObjectType());
}

Expr *TypeChecker::coerceToMaterializable(Expr *expr) {
  // Load lvalues.
  if (auto lvalue = expr->getType()->getAs<LValueType>()) {
    return new (Context) LoadExpr(expr, lvalue->getObjectType());
  }

  // Walk into parenthesized expressions to update the subexpression.
  if (auto paren = dyn_cast<ParenExpr>(expr)) {
    auto sub = coerceToMaterializable(paren->getSubExpr());
    paren->setSubExpr(sub);
    paren->setType(sub->getType());
    return paren;
  }

  // Walk into tuples to update the subexpressions.
  if (auto tuple = dyn_cast<TupleExpr>(expr)) {
    bool anyChanged = false;
    for (auto &elt : tuple->getElements()) {
      // Materialize the element.
      auto oldType = elt->getType();
      elt = coerceToMaterializable(elt);

      // If the type changed at all, make a note of it.
      if (elt->getType().getPointer() != oldType.getPointer()) {
        anyChanged = true;
      }
    }

    // If any of the types changed, rebuild the tuple type.
    if (anyChanged) {
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(tuple->getElements().size());
      for (unsigned i = 0, n = tuple->getNumElements(); i != n; ++i) {
        Type type = tuple->getElement(i)->getType();
        Identifier name = tuple->getElementName(i);
        elements.push_back(TupleTypeElt(type, name));
      }
      tuple->setType(TupleType::get(elements, Context));
    }

    return tuple;
  }

  // Nothing to do.
  return expr;
}

bool TypeChecker::convertToType(Expr *&expr, Type type, DeclContext *dc) {
  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  cs.addConstraint(ConstraintKind::Conversion, expr->getType(), type,
                   cs.getConstraintLocator(expr, { }));

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Perform the conversion.
  Expr *result = solution.coerceToType(expr, type,
                                       cs.getConstraintLocator(expr, { }));
  if (!result) {
    return true;
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  expr = result;
  cleanup.disable();
  return false;
}

//===--------------------------------------------------------------------===//
// Debugging
//===--------------------------------------------------------------------===//
#pragma mark Debugging

void Solution::dump(SourceManager *sm) const {
  dump(sm, llvm::errs());
}

void Solution::dump(SourceManager *sm, raw_ostream &out) const {
  out << "Fixed score: " << FixedScore << "\n";

  out << "Type variables:\n";
  for (auto binding : typeBindings) {
    out.indent(2);
    binding.first->getImpl().print(out);
    out << " as ";
    binding.second.print(out);
    out << "\n";
  }

  out << "\n";
  out << "Overload choices:\n";
  for (auto ovl : overloadChoices) {
    out.indent(2);
    if (ovl.first)
      ovl.first->dump(sm, out);
    out << " with ";

    auto choice = ovl.second.choice;
    switch (choice.getKind()) {
    case OverloadChoiceKind::Decl:
    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::TypeDecl:
      if (choice.getBaseType())
        out << choice.getBaseType()->getString() << ".";
        
      out << choice.getDecl()->getName().str() << ": "
        << ovl.second.openedType->getString() << "\n";
      break;

    case OverloadChoiceKind::BaseType:
      out << "base type " << choice.getBaseType()->getString() << "\n";
      break;

    case OverloadChoiceKind::TupleIndex:
      out << "tuple " << choice.getBaseType()->getString() << " index "
        << choice.getTupleIndex() << "\n";
      break;
    }
    out << "\n";
  }
}

void ConstraintSystem::dump() {
  dump(llvm::errs());
}

void ConstraintSystem::dump(raw_ostream &out) {
  out << "Score: " << CurrentScore << "\n";
  out << "Type Variables:\n";
  for (auto tv : TypeVariables) {
    out.indent(2);
    tv->getImpl().print(out);
    if (tv->getImpl().canBindToLValue())
      out << " [lvalue allowed]";
    auto rep = getRepresentative(tv);
    if (rep == tv) {
      if (auto fixed = getFixedType(tv)) {
        out << " as ";
        fixed->print(out);
      }
    } else {
      out << " equivalent to ";
      rep->print(out);
    }
    out << "\n";
  }

  out << "\nActive Constraints:\n";
  for (auto &constraint : ActiveConstraints) {
    out.indent(2);
    constraint.print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  out << "\nInactive Constraints:\n";
  for (auto &constraint : InactiveConstraints) {
    out.indent(2);
    constraint.print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  if (solverState && !solverState->retiredConstraints.empty()) {
    out << "\nRetired Constraints:\n";
    for (auto &constraint : solverState->retiredConstraints) {
      out.indent(2);
      constraint.print(out, &getTypeChecker().Context.SourceMgr);
      out << "\n";
    }
  }

  if (resolvedOverloadSets) {
    out << "Resolved overloads:\n";

    // Otherwise, report the resolved overloads.
    for (auto resolved = resolvedOverloadSets;
         resolved; resolved = resolved->Previous) {
      auto &choice = resolved->Choice;
      out << "  selected overload set choice ";
      switch (choice.getKind()) {
        case OverloadChoiceKind::Decl:
        case OverloadChoiceKind::DeclViaDynamic:
        case OverloadChoiceKind::TypeDecl:
          if (choice.getBaseType())
            out << choice.getBaseType()->getString() << ".";
          out << choice.getDecl()->getName().str() << ": "
            << resolved->BoundType->getString() << " == "
            << resolved->ImpliedType->getString() << "\n";
          break;

        case OverloadChoiceKind::BaseType:
          out << "base type " << choice.getBaseType()->getString() << "\n";
          break;

        case OverloadChoiceKind::TupleIndex:
          out << "tuple " << choice.getBaseType()->getString() << " index "
              << choice.getTupleIndex() << "\n";
          break;
      }
    }
    out << "\n";
  }

  if (failedConstraint) {
    out << "\nFailed constraint:\n";
    out.indent(2);
    failedConstraint->print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }
}

/// Determine the semantics of a checked cast operation.
CheckedCastKind TypeChecker::typeCheckCheckedCast(Type fromType,
                                 Type toType,
                                 DeclContext *dc,
                                 SourceLoc diagLoc,
                                 SourceRange diagFromRange,
                                 SourceRange diagToRange,
                                 std::function<bool (Type)> convertToType) {
  Type origFromType = fromType;
  bool toArchetype = toType->is<ArchetypeType>();
  bool fromArchetype = fromType->is<ArchetypeType>();
  SmallVector<ProtocolDecl*, 2> toProtocols;
  bool toExistential = toType->isExistentialType(toProtocols);
  SmallVector<ProtocolDecl*, 2> fromProtocols;
  bool fromExistential = fromType->isExistentialType(fromProtocols);
  
  // If the from/to types are equivalent or implicitly convertible,
  // this is a coercion.
  if (fromType->isEqual(toType) || isConvertibleTo(fromType, toType, dc)) {
    return CheckedCastKind::Coercion;
  }
  
  // We can only downcast to an existential if the destination protocols are
  // objc and the source type is an objc class or an existential bounded by objc
  // protocols.
  if (toExistential) {
    if (fromExistential) {
      for (auto fromProtocol : fromProtocols) {
        if (!fromProtocol->isObjC())
          goto unsupported_existential_cast;
      }
    } else {
      auto fromClass = fromType->getClassOrBoundGenericClass();
      if (!fromClass || !fromClass->isObjC())
        goto unsupported_existential_cast;
    }

    for (auto toProtocol : toProtocols) {
      if (!toProtocol->isObjC())
        goto unsupported_existential_cast;
    }
    
    return CheckedCastKind::ConcreteToUnrelatedExistential;
    
  unsupported_existential_cast:
    diagnose(diagLoc, diag::downcast_to_non_objc_existential,
             origFromType, toType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
    return CheckedCastKind::Unresolved;
  }
  
  // A downcast can:
  //   - convert an archetype to a (different) archetype type.
  if (fromArchetype && toArchetype) {
    return CheckedCastKind::ArchetypeToArchetype;
  }

  //   - convert from an existential to an archetype or conforming concrete
  //     type.
  if (fromExistential) {
    if (toArchetype) {
      return CheckedCastKind::ExistentialToArchetype;
    } else if (isConvertibleTo(toType, fromType, dc)) {
      return CheckedCastKind::ExistentialToConcrete;
    } else {
      diagnose(diagLoc,
               diag::downcast_from_existential_to_unrelated,
               origFromType, toType)
        .highlight(diagFromRange)
        .highlight(diagToRange);
      return CheckedCastKind::Unresolved;
    }
  }
  
  //   - convert an archetype to a concrete type fulfilling its constraints.
  if (fromArchetype) {
    if (!isSubstitutableFor(toType, fromType->castTo<ArchetypeType>(), dc)) {
      diagnose(diagLoc,
               diag::downcast_from_archetype_to_unrelated,
               origFromType, toType)
        .highlight(diagFromRange)
        .highlight(diagToRange);
      return CheckedCastKind::Unresolved;
    }
    return CheckedCastKind::ArchetypeToConcrete;
  }
  
  if (toArchetype) {
    //   - convert from a superclass to an archetype.
    if (auto toSuperType = toType->castTo<ArchetypeType>()->getSuperclass()) {
      // Coerce to the supertype of the archetype.
      if (convertToType(toSuperType))
        return CheckedCastKind::Unresolved;
      
      return CheckedCastKind::SuperToArchetype;
    }
    
    //  - convert a concrete type to an archetype for which it fulfills
    //    constraints.
    if (isSubstitutableFor(fromType, toType->castTo<ArchetypeType>(), dc)) {
      return CheckedCastKind::ConcreteToArchetype;
    }
    
    diagnose(diagLoc,
             diag::downcast_from_concrete_to_unrelated_archetype,
             origFromType, toType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
    return CheckedCastKind::Unresolved;
  }

  // The remaining case is a class downcast.

  assert(!fromArchetype && "archetypes should have been handled above");
  assert(!toArchetype && "archetypes should have been handled above");
  assert(!fromExistential && "existentials should have been handled above");
  assert(!toExistential && "existentials should have been handled above");

  // The destination type must be a subtype of the source type.
  if (!isSubtypeOf(toType, fromType, dc)) {
    diagnose(diagLoc, diag::downcast_to_unrelated, origFromType, toType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
    return CheckedCastKind::Unresolved;
  }

  return CheckedCastKind::Downcast;
}
