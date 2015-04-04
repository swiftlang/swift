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
#include "swift/Parse/Lexer.h"
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

TypeBase *TypeVariableType::getBaseBeingSubstituted() {
  auto impl = this->getImpl();
  auto archetype = impl.getArchetype();
  
  if (archetype)
    return archetype;

  if (auto locator = impl.getLocator())
    if (auto anchor = locator->getAnchor())
      if (auto anchorType = anchor->getType())
        if (!(anchorType->getAs<TypeVariableType>() ||
              anchorType->getAs<AnyFunctionType>()))
          return anchorType.getPointer();
  
  if (auto proto = impl.literalConformanceProto) {
    return proto->getType()->
           getAs<MetatypeType>()->
           getInstanceType().getPointer();
  }

  return this;
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

bool constraints::computeTupleShuffle(TupleType *fromTuple, TupleType *toTuple,
                                      SmallVectorImpl<int> &sources,
                                      SmallVectorImpl<unsigned> &variadicArgs) {
  const int unassigned = -3;
  
  SmallVector<bool, 4> consumed(fromTuple->getNumElements(), false);
  sources.clear();
  variadicArgs.clear();
  sources.assign(toTuple->getNumElements(), unassigned);

  // Match up any named elements.
  for (unsigned i = 0, n = toTuple->getNumElements(); i != n; ++i) {
    const auto &toElt = toTuple->getElement(i);

    // Skip unnamed elements.
    if (!toElt.hasName())
      continue;

    // Find the corresponding named element.
    int matched = -1;
    {
      int index = 0;
      for (auto field : fromTuple->getElements()) {
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
  unsigned fromNext = 0, fromLast = fromTuple->getNumElements();
  auto skipToNextAvailableInput = [&] {
    while (fromNext != fromLast && consumed[fromNext])
      ++fromNext;
  };
  skipToNextAvailableInput();

  for (unsigned i = 0, n = toTuple->getNumElements(); i != n; ++i) {
    // Check whether we already found a value for this element.
    if (sources[i] != unassigned)
      continue;

    const auto &elt2 = toTuple->getElement(i);

    // Variadic tuple elements match the rest of the input elements.
    if (elt2.isVararg()) {
      // Collect the remaining (unnamed) inputs.
      while (fromNext != fromLast) {
        // Labeled elements can't be adopted into varargs even if
        // they're non-mandatory.  There isn't a really strong reason
        // for this, though.
        if (fromTuple->getElement(fromNext).hasName()) {
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

    // Fail if the input element is named and we're trying to match it with
    // something with a different label.
    if (fromTuple->getElement(fromNext).hasName() && elt2.hasName())
      return true;

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

bool constraints::hasTrailingClosure(const ConstraintLocatorBuilder &locator) {
  if (Expr *e = locator.trySimplifyToExpr()) {
    if (ParenExpr *parenExpr = dyn_cast<ParenExpr>(e)) {
      return parenExpr->hasTrailingClosure();
    } else if (TupleExpr *tupleExpr = dyn_cast<TupleExpr>(e)) {
      return tupleExpr->hasTrailingClosure();
    }
  }
  return false;
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
    return tuple->getNumElements();
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
    return (!value->getAttrs().hasAttribute<PostfixAttr>() &&
            getNumArgs(value) == 1);

  case DeclRefKind::PostfixOperator:
    return (value->getAttrs().hasAttribute<PostfixAttr>() &&
            getNumArgs(value) == 1);
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
  UnqualifiedLookup Lookup(Name, Context, &TC,
                           /*NonCascading=*/isa<AbstractFunctionDecl>(Context),
                           UDRE->getLoc());

  if (!Lookup.isSuccess()) {
    TC.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return new (TC.Context) ErrorExpr(Loc);
  }

  // FIXME: Need to refactor the way we build an AST node from a lookup result!

  if (!Lookup.Results.empty() &&
      Lookup.Results[0].Kind == UnqualifiedLookupResult::ModuleName) {
    assert(Lookup.Results.size() == 1 && "module names should be unique");
    ModuleType *MT = ModuleType::get(Lookup.Results[0].getNamedModule());
    return new (TC.Context) ModuleExpr(Loc, MT);
  }

  bool AllDeclRefs = true;
  SmallVector<ValueDecl*, 4> ResultValues;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
      case UnqualifiedLookupResult::ModuleName:
        llvm_unreachable("handled above");

      case UnqualifiedLookupResult::MemberProperty:
      case UnqualifiedLookupResult::MemberFunction:
      case UnqualifiedLookupResult::MetatypeMember:
      case UnqualifiedLookupResult::ExistentialMember:
      case UnqualifiedLookupResult::ArchetypeMember:
      case UnqualifiedLookupResult::MetaArchetypeMember:
        // Types are never referenced with an implicit 'self'.
        if (!isa<TypeDecl>(Result.getValueDecl())) {
          AllDeclRefs = false;
          break;
        }

        SWIFT_FALLTHROUGH;

      case UnqualifiedLookupResult::ModuleMember:
      case UnqualifiedLookupResult::LocalDecl: {
        ValueDecl *D = Result.getValueDecl();
        if (!D->hasType()) {
          assert(D->getDeclContext()->isLocalContext());
          if (!D->isInvalid()) {
            TC.diagnose(Loc, diag::use_local_before_declaration, Name);
            TC.diagnose(D->getLoc(), diag::decl_declared_here, Name);
          }
          return new (TC.Context) ErrorExpr(Loc);
        }
        if (matchesDeclRefKind(D, UDRE->getRefKind()))
          ResultValues.push_back(D);
        break;
      }
    }
  }

  // If we have an unambiguous reference to a type decl, form a TypeExpr.  This
  // doesn't handle specialized decls since they are processed when the
  // UnresolvedSpecializeExpr is seen.
  if (!UDRE->isSpecialized() &&
      ResultValues.size() == 1 && UDRE->getRefKind() == DeclRefKind::Ordinary &&
      isa<TypeDecl>(ResultValues[0])) {
    return TypeExpr::createForDecl(Loc, cast<TypeDecl>(ResultValues[0]));
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

    // For operators, sort the results so that non-generic operations come
    // first.
    // Note: this is part of a performance hack to prefer non-generic operators
    // to generic operators, because the former is far more efficient to check.
    if (UDRE->getRefKind() != DeclRefKind::Ordinary) {
      std::stable_sort(ResultValues.begin(), ResultValues.end(),
                       [&](ValueDecl *x, ValueDecl *y) -> bool {
        auto xGeneric = x->getInterfaceType()->getAs<GenericFunctionType>();
        auto yGeneric = y->getInterfaceType()->getAs<GenericFunctionType>();
        if (static_cast<bool>(xGeneric) != static_cast<bool>(yGeneric)) {
          return xGeneric? false : true;
        }

        if (!xGeneric)
          return false;

        unsigned xDepth = xGeneric->getGenericParams().back()->getDepth();
        unsigned yDepth = yGeneric->getGenericParams().back()->getDepth();
        return xDepth < yDepth;
      });
    }

    return TC.buildRefExpr(ResultValues, Context, Loc, UDRE->isImplicit(),
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
      BaseExpr = TypeExpr::createForDecl(Loc, NTD);
    } else {
      BaseExpr = new (TC.Context) DeclRefExpr(Base, Loc, /*implicit=*/true);
    }
    return new (TC.Context) UnresolvedDotExpr(BaseExpr, SourceLoc(), Name, Loc,
                                              UDRE->isImplicit());
  }
  
  // FIXME: If we reach this point, the program we're being handed is likely
  // very broken, but it's still conceivable that this may happen due to
  // invalid shadowed declarations.
  // llvm_unreachable("Can't represent lookup result");
  return new (TC.Context) ErrorExpr(Loc);
}

namespace {
  class PreCheckExpression : public ASTWalker {
    TypeChecker &TC;
    DeclContext *DC;
  public:
    PreCheckExpression(TypeChecker &tc, DeclContext *dc) : TC(tc), DC(dc) { }

    bool walkToClosureExprPre(ClosureExpr *expr);

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For capture lists, we typecheck the decls they contain.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        // Validate the capture list.
        for (auto capture : captureList->getCaptureList()) {
          TC.typeCheckDecl(capture.Init, true);
          TC.typeCheckDecl(capture.Init, false);
          TC.typeCheckDecl(capture.Var, true);
          TC.typeCheckDecl(capture.Var, false);
        }
        return { true, expr };
      }

      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<ClosureExpr>(expr))
        return { walkToClosureExprPre(closure), expr };

      if (auto unresolved = dyn_cast<UnresolvedDeclRefExpr>(expr)) {
        TC.checkForForbiddenPrefix(unresolved);
        return { true, BindName(unresolved, DC, TC) };
      }

      if (auto PlaceholderE = dyn_cast<EditorPlaceholderExpr>(expr)) {
        if (!PlaceholderE->getTypeLoc().isNull()) {
          if (!TC.validateType(PlaceholderE->getTypeLoc(), DC))
            expr->setType(PlaceholderE->getTypeLoc().getType());
        }
        return { true, expr };
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      // Fold sequence expressions.
      if (auto seqExpr = dyn_cast<SequenceExpr>(expr))
        return TC.foldSequence(seqExpr, DC);

      // Type check the type parameters in an UnresolvedSpecializeExpr.
      if (auto us = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
        for (TypeLoc &type : us->getUnresolvedParams()) {
          if (TC.validateType(type, DC)) {
            TC.diagnose(us->getLAngleLoc(),
                        diag::while_parsing_as_left_angle_bracket);
            return nullptr;
          }
        }
        
        // If this is a reference type a specialized type, form a TypeExpr.
        if (auto *dre = dyn_cast<DeclRefExpr>(us->getSubExpr())) {
          if (auto *TD = dyn_cast<TypeDecl>(dre->getDecl())) {
            SmallVector<TypeRepr*, 4> TypeReprs;
            for (auto elt : us->getUnresolvedParams())
              TypeReprs.push_back(elt.getTypeRepr());
            auto angles = SourceRange(us->getLAngleLoc(), us->getRAngleLoc());
            return TypeExpr::createForSpecializedDecl(dre->getLoc(),
                                                      TD,
                                            TC.Context.AllocateCopy(TypeReprs),
                                                      angles);
          }
        }
        
        return expr;
      }
      
      // If we're about to step out of a ClosureExpr, restore the DeclContext.
      if (auto *ce = dyn_cast<ClosureExpr>(expr)) {
        assert(DC == ce && "DeclContext imbalance");
        DC = ce->getParent();
      }

      // If this is a sugared type that needs to be folded into a single
      // TypeExpr, do it.
      if (auto *simplified = simplifyTypeExpr(expr))
        return simplified;

      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      // Never walk into statements.
      return { false, stmt };
    }
    
    /// Simplify expressions which are type sugar productions that got parsed
    /// as expressions due to the parser not knowing which identifiers are
    /// type names.
    TypeExpr *simplifyTypeExpr(Expr *E);
  };
}

/// Perform prechecking of a ClosureExpr before we dive into it.  This returns
/// true for single-expression closures, where we want the body to be considered
/// part of this larger expression.
bool PreCheckExpression::walkToClosureExprPre(ClosureExpr *closure) {
  // Validate the parameters.
  TypeResolutionOptions options;
  options |= TR_AllowUnspecifiedTypes;
  options |= TR_AllowUnboundGenerics;
  options |= TR_ImmediateFunctionInput;
  options |= TR_InExpression;
  bool hadParameterError = false;
  if (TC.typeCheckPattern(closure->getParams(), DC, options)) {
    closure->setType(ErrorType::get(TC.Context));

    // If we encounter an error validating the parameter list, don't bail.
    // Instead, go on to validate any potential result type, and bail
    // afterwards.  This allows for better diagnostics, and keeps the
    // closure expression type well-formed.
    hadParameterError = true;
  }

  // Validate the result type, if present.
  if (closure->hasExplicitResultType() &&
      TC.validateType(closure->getExplicitResultTypeLoc(), DC,
                      TR_InExpression)) {
    closure->setType(ErrorType::get(TC.Context));
    return false;
  }

  if (hadParameterError)
    return false;

  // If the closure has a multi-statement body, we don't walk into it
  // here.
  if (!closure->hasSingleExpressionBody())
    return false;

  // Update the current DeclContext to be the closure we're about to
  // recurse into.
  assert(DC == closure->getParent() && "Decl context isn't correct");
  DC = closure;
  return true;
}

/// Simplify expressions which are type sugar productions that got parsed
/// as expressions due to the parser not knowing which identifiers are
/// type names.
TypeExpr *PreCheckExpression::simplifyTypeExpr(Expr *E) {
  // Fold T[] into an array, it isn't a subscript on a metatype.
  if (auto *SE = dyn_cast<SubscriptExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(SE->getBase());
    if (!TyExpr) return nullptr;
    
    // We don't fold subscripts with indexes, just an empty subscript.
    TupleExpr *Indexes = dyn_cast<TupleExpr>(SE->getIndex());
    if (!Indexes || Indexes->getNumElements() != 0)
      return nullptr;

    auto *InnerTypeRepr = TyExpr->getTypeRepr();
    assert(!TyExpr->isImplicit() && InnerTypeRepr &&
           "SubscriptExpr doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");

    auto *NewTypeRepr =
      new (TC.Context) ArrayTypeRepr(InnerTypeRepr, nullptr,
                                     Indexes->getSourceRange(),
                                     /*OldSyntax=*/true);

    TC.diagnose(Indexes->getStartLoc(), diag::new_array_syntax)
      .fixItInsert(SE->getStartLoc(), "[")
      .fixItRemove(Indexes->getStartLoc());

    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  
  // Fold 'T.Type' or 'T.Protocol' into a metatype when T is a TypeExpr.
  if (auto *MRE = dyn_cast<UnresolvedDotExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(MRE->getBase());
    if (!TyExpr) return nullptr;
    
    auto *InnerTypeRepr = TyExpr->getTypeRepr();
    assert(!TyExpr->isImplicit() && InnerTypeRepr &&
           "This doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");

    if (MRE->getName() == TC.Context.Id_Protocol) {
      auto *NewTypeRepr =
        new (TC.Context) ProtocolTypeRepr(InnerTypeRepr, MRE->getNameLoc());
      return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
    }
    
    if (MRE->getName() == TC.Context.Id_Type) {
      auto *NewTypeRepr =
        new (TC.Context) MetatypeTypeRepr(InnerTypeRepr, MRE->getNameLoc());
      return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
    }
  }

  // Fold T? into an optional type when T is a TypeExpr.
  if (isa<OptionalEvaluationExpr>(E) || isa<BindOptionalExpr>(E)) {
    TypeExpr *TyExpr;
    SourceLoc QuestionLoc;
    if (auto *OOE = dyn_cast<OptionalEvaluationExpr>(E)) {
      TyExpr = dyn_cast<TypeExpr>(OOE->getSubExpr());
      QuestionLoc = OOE->getLoc();
    } else {
      TyExpr = dyn_cast<TypeExpr>(cast<BindOptionalExpr>(E)->getSubExpr());
      QuestionLoc = cast<BindOptionalExpr>(E)->getQuestionLoc();
    }
    if (!TyExpr) return nullptr;

    auto *InnerTypeRepr = TyExpr->getTypeRepr();
    assert(!TyExpr->isImplicit() && InnerTypeRepr &&
           "This doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");
    
    // The optional evaluation is passed through.
    if (isa<OptionalEvaluationExpr>(E))
      return TyExpr;

    auto *NewTypeRepr =
      new (TC.Context) OptionalTypeRepr(InnerTypeRepr, QuestionLoc);
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  
  // Fold (T) into a type T with parens around it.
  if (auto *PE = dyn_cast<ParenExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(PE->getSubExpr());
    if (!TyExpr) return nullptr;
    
    TypeRepr *InnerTypeRepr[] = { TyExpr->getTypeRepr() };
    assert(!TyExpr->isImplicit() && InnerTypeRepr[0] &&
           "SubscriptExpr doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");
    
    auto *NewTypeRepr =
     new (TC.Context) TupleTypeRepr(TC.Context.AllocateCopy(InnerTypeRepr),
                                    PE->getSourceRange(), SourceLoc());
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  
  // Fold a tuple expr like (T1,T2) into a tuple type (T1,T2).
  if (auto *TE = dyn_cast<TupleExpr>(E)) {
    if (TE->hasTrailingClosure() ||
        // FIXME: Handle tuple element names too.
        TE->hasElementNames() ||
        // FIXME: Decide what to do about ().  It could be a type or an expr.
        TE->getNumElements() == 0)
      return nullptr;

    SmallVector<TypeRepr *, 4> Elts;
    for (auto Elt : TE->getElements()) {
      auto *eltTE = dyn_cast<TypeExpr>(Elt);
      if (!eltTE) return nullptr;
      assert(eltTE->getTypeRepr() && !eltTE->isImplicit() &&
             "This doesn't work on implicit TypeExpr's, the "
             "TypeExpr should have been built correctly in the first place");

      Elts.push_back(eltTE->getTypeRepr());
    }
    auto *NewTypeRepr =
      new (TC.Context) TupleTypeRepr(TC.Context.AllocateCopy(Elts),
                                     TE->getSourceRange(), SourceLoc());
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  

  // Fold [T] into an array type.
  if (auto *AE = dyn_cast<ArrayExpr>(E)) {
    if (AE->getElements().size() != 1)
      return nullptr;

    TypeExpr *TyExpr = dyn_cast<TypeExpr>(AE->getElement(0));
    if (!TyExpr)
      return nullptr;

    auto *NewTypeRepr =
      new (TC.Context) ArrayTypeRepr(TyExpr->getTypeRepr(), nullptr,
                                     SourceRange(AE->getLBracketLoc(),
                                                 AE->getRBracketLoc()),
                                     /*OldSyntax=*/false);
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));

  }

  // Fold [K : V] into a dictionary type.
  if (auto *DE = dyn_cast<DictionaryExpr>(E)) {
    if (DE->getElements().size() != 1)
      return nullptr;

    TypeRepr *keyTypeRepr, *valueTypeRepr;
    
    if (auto EltTuple = dyn_cast<TupleExpr>(DE->getElement(0))) {
      TypeExpr *KeyTyExpr = dyn_cast<TypeExpr>(EltTuple->getElement(0));
      if (!KeyTyExpr)
        return nullptr;

      TypeExpr *ValueTyExpr = dyn_cast<TypeExpr>(EltTuple->getElement(1));
      if (!ValueTyExpr)
        return nullptr;
     
      keyTypeRepr = KeyTyExpr->getTypeRepr();
      valueTypeRepr = ValueTyExpr->getTypeRepr();
    } else {
      auto *TE = dyn_cast<TypeExpr>(DE->getElement(0));
      if (!TE) return nullptr;
      
      auto *TRE = dyn_cast_or_null<TupleTypeRepr>(TE->getTypeRepr());
      if (!TRE || TRE->getEllipsisLoc().isValid()) return nullptr;
      while (TRE->isParenType()) {
        TRE = dyn_cast_or_null<TupleTypeRepr>(TRE->getElement(0));
        if (!TRE || TRE->getEllipsisLoc().isValid()) return nullptr;
      }

      assert(TRE->getElements().size() == 2);
      keyTypeRepr = TRE->getElement(0);
      valueTypeRepr = TRE->getElement(1);
    }

    auto *NewTypeRepr =
      new (TC.Context) DictionaryTypeRepr(keyTypeRepr, valueTypeRepr,
                                          /*FIXME:colonLoc=*/SourceLoc(),
                                          SourceRange(DE->getLBracketLoc(),
                                                      DE->getRBracketLoc()));
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }

  return nullptr;
}


/// \brief Clean up the given ill-formed expression, removing any references
/// to type variables and setting error types on erroneous expression nodes.
static Expr *cleanupIllFormedExpression(ASTContext &context,
                                        ConstraintSystem *cs, Expr *expr) {
  class CleanupIllFormedExpression : public ASTWalker {
    ASTContext &context;

  public:
    CleanupIllFormedExpression(ASTContext &context, ConstraintSystem *cs)
      : context(context) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        closure->getParams()->forEachVariable([&](VarDecl *VD) {
          if (VD->hasType()) {
            Type T = VD->getType();
            if (T->hasTypeVariable()) {
              T = ErrorType::get(context);
              VD->setInvalid();
            }
            VD->overwriteType(T);
          } else {
            VD->setType(ErrorType::get(context));
            VD->setInvalid();
          }
        });
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      Type type;
      if (expr->getType()) {
        type = expr->getType();
      }

      if (!type || type->hasTypeVariable())
        expr->setType(ErrorType::get(context));
      else
        expr->setType(type);
      return expr;
    }

    bool walkToDeclPre(Decl *D) override {
      // Ensure that declarations inside the closure are marked as invalid.
      // Alternatively, we could type check them anyway, even if inferring the
      // type of the closure failed.
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
        AFD->overwriteType(ErrorType::get(context));
      }
      return true;
    }
  };

  if (!expr)
    return expr;
  
  return expr->walk(CleanupIllFormedExpression(context, cs));
}

CleanupIllFormedExpressionRAII::~CleanupIllFormedExpressionRAII() {
  if (expr) {
    *expr = cleanupIllFormedExpression(cs.getASTContext(), &cs, *expr);
  }
}

/// Pre-check the expression, validating any types that occur in the
/// expression and folding sequence expressions.
static bool preCheckExpression(TypeChecker &tc, Expr *&expr, DeclContext *dc) {
  PreCheckExpression preCheck(tc, dc);
  // Perform the pre-check.
  if (auto result = expr->walk(preCheck)) {
    expr = result;
    return false;
  }

  // Pre-check failed. Clean up and return.
  expr = cleanupIllFormedExpression(dc->getASTContext(), nullptr, expr);
  return true;
}

ExprTypeCheckListener::~ExprTypeCheckListener() { }

bool ExprTypeCheckListener::builtConstraints(ConstraintSystem &cs, Expr *expr) {
  return false;
}

void ExprTypeCheckListener::solvedConstraints(Solution &solution) { }

Expr *ExprTypeCheckListener::appliedSolution(Solution &solution, Expr *expr) {
  return expr;
}

bool ExprTypeCheckListener::suppressDiagnostics() const {
  return false;
}

static void diagnoseExpr(TypeChecker &TC, const Expr *E,
                         const DeclContext *DC,
                         const ExprTypeCheckListener *listener) {
  if (listener && listener->suppressDiagnostics())
    return;
  performExprDiagnostics(TC, E, DC);
}

#pragma mark High-level entry points
bool TypeChecker::typeCheckExpression(
       Expr *&expr, DeclContext *dc,
       Type convertType,
       Type contextualType,
       bool discardedExpr,
       FreeTypeVariableBinding allowFreeTypeVariables,
       ExprTypeCheckListener *listener) {
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  if (preCheckExpression(*this, expr, dc))
    return true;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc, ConstraintSystemFlags::AllowFixes);
  
  if (!contextualType.isNull()) {
    cs.setContextualType(expr, &contextualType);
  }
  
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
                     cs.getConstraintLocator(expr));
    cs.setConversionType(expr, convertType.getPointer());
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
    if (listener && listener->suppressDiagnostics())
      return true;

    // Try to provide a decent diagnostic.
    if (cs.salvage(viable, expr)) {
      diagnoseExpr(*this, expr, dc, listener);
      return true;
    }

    // The system was salvaged; continue on as if nothing happened.
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
  auto result = cs.applySolution(solution, expr,
                                 listener && listener->suppressDiagnostics());
  if (!result) {
    diagnoseExpr(*this, expr, dc, listener);
    // Failure already diagnosed, above, as part of applying the solution.
    return true;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution.coerceToType(result, convertType,
                                   cs.getConstraintLocator(expr));
    if (!result) {
      diagnoseExpr(*this, expr, dc, listener);
      return true;
    }
  } else if (auto *ioTy = result->getType()->getAs<InOutType>()) {
    // We explicitly took a reference to the result, but didn't use it.
    // Complain and emit a Fix-It to zap the '&'.
    auto addressOf = cast<InOutExpr>(result->getSemanticsProvidingExpr());
    diagnose(addressOf->getLoc(), diag::reference_non_inout,
             ioTy->getObjectType())
      .highlight(addressOf->getSubExpr()->getSourceRange())
      .fixItRemove(SourceRange(addressOf->getLoc()));

    // Strip the address-of expression.
    result = addressOf->getSubExpr();
  } else if (result->getType()->isLValueType() && !discardedExpr) {
    // We referenced an lvalue. Load it.
    result = solution.coerceToType(result, result->getType()->getRValueType(),
                                   cs.getConstraintLocator(expr));
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
      diagnoseExpr(*this, expr, dc, listener);
      return true;
    }
  }

  diagnoseExpr(*this, result, dc, listener);

  expr = result;
  cleanup.disable();
  return false;
}

/// Private class to "cleanse" an expression tree of types. This is done in the
/// case of a typecheck failure, where we may want to re-typecheck partially-
/// typechecked subexpressions in a context-free manner.
class TypeNullifier : public ASTWalker {
public:
  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    
    // Preserve module expr type data to prevent further lookups.
    if (isa<ModuleExpr>(expr))
      return { false, expr };
    
    expr->setType(nullptr);
    
    if (auto cast = dyn_cast<ExplicitCastExpr>(expr)) {
      if (cast->getCastTypeLoc().getTypeRepr())
        cast->getCastTypeLoc().setType(nullptr);
    } else if (auto typeExpr = dyn_cast<TypeExpr>(expr)) {
      if (typeExpr->getTypeLoc().getTypeRepr())
        typeExpr->getTypeLoc().setType(nullptr);
    } else if (auto unresolvedSpec = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
      for (auto &arg : unresolvedSpec->getUnresolvedParams()) {
        if (arg.getTypeRepr())
          arg.setType(nullptr);
      }
    }

    return { true, expr };
  }

  std::pair<bool, Pattern*> walkToPatternPre(Pattern *pattern) override {
    pattern->setType(nullptr);
    return { true, pattern };
  }
};

void TypeChecker::eraseTypeData(Expr *&expr) {
  assert(expr);
  
  expr->walk(TypeNullifier());
}

bool TypeChecker::typeCheckExpressionShallow(Expr *&expr, DeclContext *dc,
                                             Type convertType) {
  PrettyStackTraceExpr stackTrace(Context, "shallow type-checking", expr);

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc, ConstraintSystemFlags::AllowFixes);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraintsShallow(expr))
    expr = generatedExpr;
  else
    return true;

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType,
                     cs.getConstraintLocator(expr));
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
  if (cs.solve(viable) && cs.salvage(viable, expr)) {
    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolutionShallow(solution, expr, 
                                        /*suppressDiagnostics=*/false);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
    return true;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution.coerceToType(result, convertType,
                                   cs.getConstraintLocator(expr));
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

bool TypeChecker::typeCheckBinding(PatternBindingDecl *binding,
                                   unsigned PatternEntry) {

  /// Type checking listener for pattern binding initializers.
  class BindingListener : public ExprTypeCheckListener {
    /// The pattern binding declaration whose initializer we're checking.
    PatternBindingDecl *Binding;
    
    /// The entry in the PatternList that we are checking.
    unsigned PatternEntry;

    /// The locator we're using.
    ConstraintLocator *Locator;

    /// The type of the initializer.
    Type InitType;
    
  public:
    explicit BindingListener(PatternBindingDecl *binding, unsigned entry)
      : Binding(binding), PatternEntry(entry) { }

    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // Save the locator we're using for the expression.
      Locator = cs.getConstraintLocator(expr);

      // Collect constraints from the pattern.
      auto pattern = Binding->getPattern(PatternEntry);
      InitType = cs.generateConstraints(pattern, Locator);
      if (!InitType)
        return true;

      // Add a conversion constraint between the types.
      cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                       InitType, Locator);

      // The expression has been pre-checked; save it in case we fail later.
      Binding->setInit(PatternEntry, expr);

      return false;
    }

    virtual Expr *appliedSolution(Solution &solution, Expr *expr) {
      // Figure out what type the constraints decided on.
      auto &tc = solution.getConstraintSystem().getTypeChecker();
      InitType = solution.simplifyType(tc, InitType);

      // Convert the initializer to the type of the pattern.
      expr = solution.coerceToType(expr, InitType, Locator,
                                   false
                                   /*ignoreTopLevelInjection=
                                     Binding->isConditional()*/);
      if (!expr) {
        return nullptr;
      }

      // Force the initializer to be materializable.
      // FIXME: work this into the constraint system
      expr = tc.coerceToMaterializable(expr);

      // Apply the solution to the pattern as well.
      Pattern *pattern = Binding->getPattern(PatternEntry);
      Type patternType = expr->getType();
      patternType = patternType->getWithoutDefaultArgs(tc.Context);

      TypeResolutionOptions options;
      options |= TR_OverrideType;
      options |= TR_InExpression;
      if (tc.coercePatternToType(pattern, Binding->getDeclContext(),
                                 patternType, options)) {
        return nullptr;
      }
      Binding->setPattern(PatternEntry, pattern);
      Binding->setInit(PatternEntry, expr);
      Binding->setInitializerChecked();
      return expr;
    }
  };

  BindingListener listener(binding, PatternEntry);
  Expr *init = binding->getInit(PatternEntry);
  assert(init && "type-checking an uninitialized binding?");

  // Enter an initializer context if necessary.
  DeclContext *DC = binding->getDeclContext();
  PatternBindingInitializer *initContext = nullptr;
  bool initContextIsNew = false;
  if (!DC->isLocalContext()) {
    // Check for an existing context created by the parser.
    initContext = cast_or_null<PatternBindingInitializer>(
                                       init->findExistingInitializerContext());

    // If we didn't find one, create it.
    if (!initContext) {
      initContext = Context.createPatternBindingContext(DC);
      initContext->setBinding(binding);
      initContextIsNew = true;
    }
    DC = initContext;
  }
  
  auto pattern = binding->getPattern(PatternEntry);
  auto contextualType = pattern->hasType() ?
                            pattern->getType() :
                            Type();

  // Type-check the initializer.
  bool hadError = typeCheckExpression(init, DC, Type(),
                                      contextualType, /*discardedExpr=*/false,
                                      FreeTypeVariableBinding::Disallow,
                                      &listener);

  // If we entered an initializer context, contextualize any
  // auto-closures we might have created.
  if (initContext) {
    bool hasClosures =
      (!hadError && contextualizeInitializer(initContext, init));

    // If we created a fresh context and didn't make any autoclosures,
    // destroy the initializer context.
    if (!hasClosures && initContextIsNew) {
      Context.destroyPatternBindingContext(initContext);
    }
  }

  return hadError;
}

bool TypeChecker::typeCheckForEachBinding(DeclContext *dc, ForEachStmt *stmt) {
  /// Type checking listener for for-each binding.
  class BindingListener : public ExprTypeCheckListener {
    /// The for-each statement.
    ForEachStmt *Stmt;

    /// The locator we're using.
    ConstraintLocator *Locator;

    /// The type of the initializer.
    Type InitType;

  public:
    explicit BindingListener(ForEachStmt *stmt) : Stmt(stmt) { }

    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // Save the locator we're using for the expression.
      Locator = cs.getConstraintLocator(expr);

      // The expression type must conform to the Sequence.
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *sequenceProto
        = tc.getProtocol(Stmt->getForLoc(), KnownProtocolKind::SequenceType);
      if (!sequenceProto) {
        return true;
      }

      cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                       sequenceProto->getDeclaredType(), Locator);

      // Collect constraints from the element pattern.
      auto pattern = Stmt->getPattern();
      InitType = cs.generateConstraints(pattern, Locator);
      if (!InitType)
        return true;
      
      // Manually search for the generator witness. If no generator/element pair
      // exists, solve for them.
      Type generatorType;
      Type elementType;
      
      auto member = cs.TC.lookupMemberType(expr->getType()->getRValueType(),
                                           tc.Context.Id_Generator,
                                           cs.DC,
                                           isa<AbstractFunctionDecl>(cs.DC));
      
      if (member) {
        generatorType = member.front().second;
        
        member = cs.TC.lookupMemberType(generatorType,
                                        tc.Context.Id_Element,
                                        cs.DC,
                                        isa<AbstractFunctionDecl>(cs.DC));
        
        if (member)
          elementType = member.front().second;
      }
      
      if (elementType.isNull()) {
      
        // Determine the generator type of the sequence.
        generatorType = cs.createTypeVariable(Locator, /*options=*/0);
        cs.addConstraint(
          Constraint::create(cs, ConstraintKind::TypeMember,
                             expr->getType(), generatorType,
                             tc.Context.Id_Generator,
                             cs.getConstraintLocator(
                               Locator,
                               ConstraintLocator::SequenceGeneratorType)));

        // Determine the element type of the generator.
        // FIXME: Should look up the type witness.
        elementType = cs.createTypeVariable(Locator, /*options=*/0);
        cs.addConstraint(Constraint::create(
                           cs, ConstraintKind::TypeMember,
                           generatorType, elementType,
                           tc.Context.Id_Element,
                           cs.getConstraintLocator(
                             Locator,
                             ConstraintLocator::GeneratorElementType)));
      }
      

      // Add a conversion constraint between the element type of the sequence
      // and the type of the element pattern.
      cs.addConstraint(ConstraintKind::Conversion, elementType, InitType,
                       Locator);
      
      Stmt->setSequence(expr);
      return false;
    }

    virtual Expr *appliedSolution(Solution &solution, Expr *expr) {
      // Figure out what type the constraints decided on.
      auto &cs = solution.getConstraintSystem();
      auto &tc = cs.getTypeChecker();
      InitType = solution.simplifyType(tc, InitType);

      // Force the sequence to be materializable.
      // FIXME: work this into the constraint system
      expr = tc.coerceToMaterializable(expr);

      // Apply the solution to the iteration pattern as well.
      Pattern *pattern = Stmt->getPattern();
      TypeResolutionOptions options;
      options |= TR_OverrideType;
      options |= TR_EnumerationVariable;
      options |= TR_InExpression;
      if (tc.coercePatternToType(pattern, cs.DC, InitType, options)) {
        return nullptr;
      }

      Stmt->setPattern(pattern);
      Stmt->setSequence(expr);
      return expr;
    }
  };

  BindingListener listener(stmt);
  Expr *seq = stmt->getSequence();
  assert(seq && "type-checking an uninitialized for-each statement?");

  // Type-check the for-each loop sequence and element pattern.
  if (typeCheckExpression(seq, dc, Type(), Type(), /*discardedExpr=*/false,
                          FreeTypeVariableBinding::Disallow, &listener))
    return true;

  return false;
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
  if (destTy->is<ErrorType>())
    return Type();

  if (LValueType *destLV = destTy->getAs<LValueType>()) {
    destTy = destLV->getObjectType();
  } else if (auto typeVar = dyn_cast<TypeVariableType>(destTy.getPointer())) {
    // The destination is a type variable. This type variable must be an
    // lvalue type, which we enforce via an equality relationship with
    // @lvalue T, where T is a fresh type variable that
    // will be the object type of this particular expression type.
    auto objectTv = createTypeVariable(
                      getConstraintLocator(dest,
                                           ConstraintLocator::AssignDest),
                      /*options=*/0);
    auto refTv = LValueType::get(objectTv);
    addConstraint(ConstraintKind::Bind,
                  typeVar,
                  refTv,
                  this->getConstraintLocator(dest));
    destTy = objectTv;
  } else {
    // Give a more specific diagnostic depending on what we're assigning to.
    if (auto *DRE = dyn_cast<DeclRefExpr>(dest))
      if (auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
        Diag<Identifier> d;
        if (VD->isImplicit() && VD->getName() == getASTContext().Id_self)
          d = diag::assignment_to_self;
        else if (VD->isLet())
          d = diag::assignment_lhs_is_let;
        else if (VD->hasAccessorFunctions() && !VD->getSetter())
          d = diag::assignment_get_only_property;
        else
          d = diag::assignment_lhs_is_vardecl;
        getTypeChecker().diagnose(equalLoc, d, VD->getName())
        .highlight(dest->getSourceRange());
        return Type();
      }

    // Provide specific diagnostics for unresolved type expr.
    if (auto *UDE = dyn_cast<UnresolvedDotExpr>(dest))
      if (auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase()))
        if (auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
          getTypeChecker().diagnose(equalLoc, diag::assignment_unresolved_expr,
                                     UDE->getName(), VD->getName())
          .highlight(dest->getSourceRange());
          return Type();
        }

    // Otherwise, emit an unhelpful message.
    getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_lvalue)
      .highlight(dest->getSourceRange());

    return Type();
  }
  
  return destTy;
}

bool TypeChecker::typeCheckCondition(Expr *&expr, DeclContext *dc) {
  /// Expression type checking listener for conditions.
  class ConditionListener : public ExprTypeCheckListener {
    Expr *OrigExpr = nullptr;

  public:
    // Add the appropriate BooleanType constraint.
    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // Save the original expression.
      OrigExpr = expr;

      // If the expression has type Builtin.Int1 (or an l-value with that
      // object type), go ahead and special-case that.  This doesn't need
      // to be deeply principled because builtin types are not user-facing.
      auto rvalueType = expr->getType()->getRValueType();
      if (rvalueType->isBuiltinIntegerType(1)) {
        cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                         rvalueType,
                         cs.getConstraintLocator(expr));
        return false;
      }

      // Otherwise, the result must be a BooleanType.
      auto &tc = cs.getTypeChecker();
      auto logicValueProto = tc.getProtocol(expr->getLoc(),
                                            KnownProtocolKind::BooleanType);
      if (!logicValueProto) {
        return true;
      }

      auto logicValueType = logicValueProto->getDeclaredType();
      
      // Relate this expr to the the BooleanType in the cache, but don't
      // create a new conformance. This will help us produce a better
      // diagnostic, if need be.
      auto innerExpr = expr;
      while (auto parenExpr = dyn_cast<ParenExpr>(innerExpr))
        innerExpr = parenExpr->getSubExpr();
      
      if (dyn_cast<LiteralExpr>(innerExpr))
        cs.setConversionType(expr, logicValueType);
      
      cs.addConstraint(ConstraintKind::ConformsTo, expr->getType(),
                       logicValueType,
                       cs.getConstraintLocator(OrigExpr));
      return false;
    }

    // Convert the result to a Builtin.i1.
    virtual Expr *appliedSolution(constraints::Solution &solution,
                                  Expr *expr) {
      auto &cs = solution.getConstraintSystem();
      return solution.convertBooleanTypeToBuiltinI1(expr,
                                            cs.getConstraintLocator(OrigExpr));
    }
    
  };

  ConditionListener listener;
  return typeCheckExpression(expr, dc, Type(), Type(), /*discardedExpr=*/false,
                             FreeTypeVariableBinding::Disallow,
                             &listener);
}

bool TypeChecker::typeCheckCondition(StmtCondition &cond, DeclContext *dc) {
  bool hadError = false;
  for (auto &elt : cond) {
    if (auto E = elt.getCondition()) {
      hadError |= typeCheckCondition(E, dc);
      elt.setCondition(E);
    } else {
      auto PBD = elt.getBinding();
      assert(PBD && "Unknown kind of condition");
      typeCheckDecl(PBD, false);
      hadError |= PBD->isInvalid();
    }
  }
  return false;
}

/// Find the '~=` operator that can compare an expression inside a pattern to a
/// value of a given type.
bool TypeChecker::typeCheckExprPattern(ExprPattern *EP, DeclContext *DC,
                                       Type rhsType) {
  PrettyStackTracePattern stackTrace(Context, "type-checking", EP);

  // Create a 'let' binding to stand in for the RHS value.
  auto *matchVar = new (Context) VarDecl(/*static*/ false, /*IsLet*/true,
                                         EP->getLoc(),
                                         Context.getIdentifier("$match"),
                                         rhsType,
                                         DC);
  EP->setMatchVar(matchVar);
  matchVar->setHasNonPatternBindingInit();
  
  // Find '~=' operators for the match.
  UnqualifiedLookup matchLookup(Context.Id_MatchOperator, DC, this,
                                /*NonCascading=*/true);

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
  auto *matchOp = buildRefExpr(choices, DC, EP->getLoc(), /*Implicit=*/true);
  auto *matchVarRef = new (Context) DeclRefExpr(matchVar,
                                                EP->getLoc(),
                                                /*Implicit=*/true);
  
  Expr *matchArgElts[] = {EP->getSubExpr(), matchVarRef};
  auto *matchArgs
    = TupleExpr::create(Context, EP->getSubExpr()->getSourceRange().Start,
                        matchArgElts, { }, { },
                        EP->getSubExpr()->getSourceRange().End,
                        /*hasTrailingClosure=*/false, /*Implicit=*/true);
  
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

bool TypeChecker::typesSatisfyConstraint(Type type1, Type type2,
                                         ConstraintKind kind, DeclContext *dc) {
  ConstraintSystem cs(*this, dc, ConstraintSystemOptions());
  cs.addConstraint(kind, type1, type2, cs.getConstraintLocator(nullptr));
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isSubtypeOf(Type type1, Type type2, DeclContext *dc) {
  return typesSatisfyConstraint(type1, type2, ConstraintKind::Subtype, dc);
}

bool TypeChecker::isConvertibleTo(Type type1, Type type2, DeclContext *dc) {
  return typesSatisfyConstraint(type1, type2, ConstraintKind::Conversion, dc);
}

bool TypeChecker::isExplicitlyConvertibleTo(Type type1, Type type2,
                                            DeclContext *dc) {
  return typesSatisfyConstraint(type1, type2,
                                ConstraintKind::ExplicitConversion, dc);
}

bool TypeChecker::checkedCastMaySucceed(Type t1, Type t2, DeclContext *dc) {
  auto kind = typeCheckCheckedCast(t1, t2, dc,
                                   SourceLoc(), SourceRange(), SourceRange(),
                                   /*convertToType=*/ nullptr,
                                   /*suppressDiagnostics=*/ true);
  return (kind != CheckedCastKind::Unresolved);
}

bool TypeChecker::isSubstitutableFor(Type type, ArchetypeType *archetype,
                                     DeclContext *dc) {
  ConstraintSystem cs(*this, dc, ConstraintSystemOptions());
  auto locator = cs.getConstraintLocator(nullptr);

  // Add all of the requirements of the archetype to the given type.
  // FIXME: Short-circuit if any of the constraints fails.
  if (archetype->requiresClass() && !type->mayHaveSuperclass())
    return false;

  if (auto superclass = archetype->getSuperclass()) {
    cs.addConstraint(ConstraintKind::Subtype, type, superclass, locator);
  }
  for (auto proto : archetype->getConformsTo()) {
    cs.addConstraint(ConstraintKind::ConformsTo, type,
                     proto->getDeclaredType(), locator);
  }

  // Solve the system.
  SmallVector<Solution, 1> solution;
  return !cs.solve(solution);
}

Expr *TypeChecker::coerceToRValue(Expr *expr) {
  // Can't load from an inout value.
  if (auto *iot = expr->getType()->getAs<InOutType>()) {
    // Emit a fixit if we can find the & expression that turned this into an
    // inout.
    if (auto addrOf =
        dyn_cast<InOutExpr>(expr->getSemanticsProvidingExpr())) {
      diagnose(expr->getLoc(), diag::load_of_explicit_lvalue,
               iot->getObjectType())
      .fixItRemove(SourceRange(addrOf->getLoc()));
      return coerceToRValue(addrOf->getSubExpr());
    } else {
      diagnose(expr->getLoc(), diag::load_of_explicit_lvalue,
               iot->getObjectType());
      return expr;
    }
  }

  // If we already have an rvalue, we're done, otherwise emit a load.
  if (auto lvalueTy = expr->getType()->getAs<LValueType>())
    return new (Context) LoadExpr(expr, lvalueTy->getObjectType());

  return expr;
}

Expr *TypeChecker::coerceToMaterializable(Expr *expr) {
  // If the type is already materializable, then we're already done.
  if (expr->getType()->isMaterializable())
    return expr;
  
  // Load lvalues.
  if (auto lvalue = expr->getType()->getAs<LValueType>())
    return new (Context) LoadExpr(expr, lvalue->getObjectType());

  // Walk into parenthesized expressions to update the subexpression.
  if (auto paren = dyn_cast<IdentityExpr>(expr)) {
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
  // TODO: need to add kind arg?
  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc, ConstraintSystemFlags::AllowFixes);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  cs.addConstraint(ConstraintKind::ExplicitConversion, expr->getType(), type,
                   cs.getConstraintLocator(expr));

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable) && cs.salvage(viable, expr)) {
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
                                       cs.getConstraintLocator(expr));
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
    case OverloadChoiceKind::DeclViaBridge:
    case OverloadChoiceKind::DeclViaUnwrappedOptional:
      choice.getDecl()->dumpRef(out);
      out << " as ";
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

  out << "\n";
  out << "Constraint restrictions:\n";
  for (auto &restriction : ConstraintRestrictions) {
    out.indent(2) << restriction.first.first
                  << " to " << restriction.first.second
                  << " is " << getName(restriction.second) << "\n";
  }

  out << "\nDisjunction choices:\n";
  for (auto &choice : DisjunctionChoices) {
    out.indent(2);
    choice.first->dump(sm, out);
    out << " is #" << choice.second << "\n";
  }

  if (!OpenedTypes.empty()) {
    out << "\nOpened types:\n";
    for (const auto &opened : OpenedTypes) {
      out.indent(2);
      opened.first->dump(sm, out);
      out << " opens ";
      interleave(opened.second.begin(), opened.second.end(),
                 [&](OpenedType opened) {
                   opened.first.print(out);
                   out << " -> ";
                   opened.second->print(out);
                 },
                 [&]() {
                   out << ", ";
                 });
      out << "\n";
    }
  }

  if (!OpenedExistentialTypes.empty()) {
    out << "\nOpened existential types:\n";
    for (const auto &openedExistential : OpenedExistentialTypes) {
      out.indent(2);
      openedExistential.first->dump(sm, out);
      out << " opens to " << openedExistential.second->getString();
      out << "\n";
    }
  }

  if (!Fixes.empty()) {
    out << "\nFixes:\n";
    for (auto &fix : Fixes) {
      out.indent(2);
      fix.first.print(out, &getConstraintSystem());
      out << " @ ";
      fix.second->dump(sm, out);
      out << "\n";
    }
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
      case OverloadChoiceKind::DeclViaBridge:
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
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

  if (!DisjunctionChoices.empty()) {
    out << "\nDisjunction choices:\n";
    for (auto &choice : DisjunctionChoices) {
      out.indent(2);
      choice.first->dump(&getTypeChecker().Context.SourceMgr, out);
      out << " is #" << choice.second << "\n";
    }
  }

  if (!OpenedTypes.empty()) {
    out << "\nOpened types:\n";
    for (const auto &opened : OpenedTypes) {
      out.indent(2);
      opened.first->dump(&getTypeChecker().Context.SourceMgr, out);
      out << " opens ";
      interleave(opened.second.begin(), opened.second.end(),
                 [&](OpenedType opened) {
                   opened.first.print(out);
                   out << " -> ";
                   opened.second->print(out);
                 },
                 [&]() {
                   out << ", ";
                 });
      out << "\n";
    }
  }

  if (!OpenedExistentialTypes.empty()) {
    out << "\nOpened existential types:\n";
    for (const auto &openedExistential : OpenedExistentialTypes) {
      out.indent(2);
      openedExistential.first->dump(&getTypeChecker().Context.SourceMgr, out);
      out << " opens to " << openedExistential.second->getString();
      out << "\n";
    }
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
                                 std::function<bool (Type)> convertToType,
                                 bool suppressDiagnostics) {
  // If the from/to types are equivalent or implicitly convertible,
  // this is a coercion.
  if (fromType->isEqual(toType) || isConvertibleTo(fromType, toType, dc)) {
    return CheckedCastKind::Coercion;
  }

  Type origFromType = fromType;
  Type origToType = toType;

  // Strip optional wrappers off of the destination type in sync with
  // stripping them off the origin type.
  while (auto toValueType = toType->getAnyOptionalObjectType()) {
    // Complain if we're trying to increase optionality, e.g.
    // casting an NSObject? to an NSString??.  That's not a subtype
    // relationship.
    auto fromValueType = fromType->getAnyOptionalObjectType();
    if (!fromValueType) {
      if (!suppressDiagnostics) {
        diagnose(diagLoc, diag::downcast_to_more_optional,
                 origFromType, origToType)
          .highlight(diagFromRange)
          .highlight(diagToRange);
      }
      return CheckedCastKind::Unresolved;
    }

    toType = toValueType;
    fromType = fromValueType;
  }
  
  // On the other hand, casts can decrease optionality monadically.
  unsigned extraFromOptionals = 0;
  while (auto fromValueType = fromType->getAnyOptionalObjectType()) {
    fromType = fromValueType;
    ++extraFromOptionals;
  }

  // If the unwrapped from/to types are equivalent, this isn't a real
  // downcast. Complain.
  if (fromType->isEqual(toType)) {
    assert(extraFromOptionals > 0 && "No extra 'from' optionals?");
    
    // FIXME: Add a Fix-It, when the caller provides us with enough information.
    if (!suppressDiagnostics) {
      diagnose(diagLoc, diag::downcast_same_type,
               origFromType, origToType, std::string(extraFromOptionals, '!'))
        .highlight(diagFromRange)
        .highlight(diagToRange);
    }
    return CheckedCastKind::Unresolved;
  }

  // Strip metatypes. If we can cast two types, we can cast their metatypes.
  bool metatypeCast = false;
  while (auto toMetatype = toType->getAs<MetatypeType>()) {
    auto fromMetatype = fromType->getAs<MetatypeType>();
    if (!fromMetatype)
      break;
    
    metatypeCast = true;
    toType = toMetatype->getInstanceType();
    fromType = fromMetatype->getInstanceType();
  }
  
  // Strip an inner layer of potentially existential metatype.
  bool toExistentialMetatype = false;
  bool fromExistentialMetatype = false;
  if (auto toMetatype = toType->getAs<AnyMetatypeType>()) {
    toExistentialMetatype = toMetatype->is<ExistentialMetatypeType>();
    if (auto fromMetatype = fromType->getAs<AnyMetatypeType>()) {
      fromExistentialMetatype = fromMetatype->is<ExistentialMetatypeType>();
      toType = toMetatype->getInstanceType();
      fromType = fromMetatype->getInstanceType();
    }
  }

  bool toArchetype = toType->is<ArchetypeType>();
  bool fromArchetype = fromType->is<ArchetypeType>();
  SmallVector<ProtocolDecl*, 2> toProtocols;
  bool toExistential = toType->isExistentialType(toProtocols);
  SmallVector<ProtocolDecl*, 2> fromProtocols;
  bool fromExistential = fromType->isExistentialType(fromProtocols);
  
  // If we're doing a metatype cast, it can only be existential if we're
  // casting to/from the existential metatype. 'T.self as P.Protocol'
  // can only succeed if T is exactly the type P, so is a concrete cast,
  // whereas 'T.self as P.Type' succeeds for types conforming to the protocol
  // P, and is an existential cast.
  if (metatypeCast) {
    toExistential &= toExistentialMetatype;
    fromExistential &= fromExistentialMetatype;
  }
  
  // Casts to or from generic types can't be statically constrained in most
  // cases, because there may be protocol conformances we don't statically
  // know about.
  //
  // TODO: There are cases we could statically warn about, such as casting
  // from a non-class to a class-constrained archetype or existential.
  if (toExistential || fromExistential || fromArchetype || toArchetype)
    return CheckedCastKind::ValueCast;

  // Reality check casts between concrete types.

  ConstraintSystem cs(*this, dc, ConstraintSystemOptions());
  
  if (cs.isArrayType(toType) && cs.isArrayType(fromType)) {
    return CheckedCastKind::ArrayDowncast;
  }

  if (auto toDict = cs.isDictionaryType(toType)) {
    if (auto fromDict = cs.isDictionaryType(fromType)) {
      if (toDict->first->isBridgeableObjectType() &&
          toDict->second->isBridgeableObjectType() &&
          fromDict->first->isBridgeableObjectType() &&
          fromDict->second->isBridgeableObjectType())
        return CheckedCastKind::DictionaryDowncast;
      
      return CheckedCastKind::DictionaryDowncastBridged;
    }
  }

  if (cs.isSetType(toType) && cs.isSetType(fromType)) {
    auto toBaseType = cs.getBaseTypeForSetType(toType.getPointer());
    auto fromBaseType = cs.getBaseTypeForSetType(fromType.getPointer());
    if (toBaseType->isBridgeableObjectType() &&
        fromBaseType->isBridgeableObjectType()) {
      return CheckedCastKind::SetDowncast;
    }
    return CheckedCastKind::SetDowncastBridged;
  }

  // If the destination type is a subtype of the source type, we have
  // a downcast.
  if (isSubtypeOf(toType, fromType, dc)) {
    return CheckedCastKind::ValueCast;
  }

  // If we can bridge through an Objective-C class, do so.
  if (Type objCClass = getDynamicBridgedThroughObjCClass(dc, true, fromType,
                                                         toType)){
    if (isSubtypeOf(objCClass, fromType, dc))
      return CheckedCastKind::BridgeFromObjectiveC;
  }

  // The runtime doesn't support casts to CF types and always lets them succeed.
  // This "always fails" diagnosis makes no sense when paired with the CF
  // one.
  auto clas = toType->getClassOrBoundGenericClass();
  if (!clas || !clas->isForeign()) {
    if (suppressDiagnostics) {
      return CheckedCastKind::Unresolved;
    }
    diagnose(diagLoc, diag::downcast_to_unrelated, origFromType, origToType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
  }
  return CheckedCastKind::ValueCast;
}

/// If the expression is a an implicit call to _forceBridgeFromObjectiveC or
/// _conditionallyBridgeFromObjectiveC, returns the argument of that call.
static Expr *lookThroughBridgeFromObjCCall(ASTContext &ctx, Expr *expr) {
  auto call = dyn_cast<CallExpr>(expr);
  if (!call || !call->isImplicit())
    return nullptr;

  auto callee = call->getCalledValue();
  if (!callee)
    return nullptr;

  if (callee == ctx.getForceBridgeFromObjectiveC(nullptr) ||
      callee == ctx.getConditionallyBridgeFromObjectiveC(nullptr))
    return cast<TupleExpr>(call->getArg())->getElement(0);

  return nullptr;
}

/// If the expression has the effect of a forced downcast, find the
/// underlying forced downcast expression.
ForcedCheckedCastExpr *swift::findForcedDowncast(ASTContext &ctx, Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
  
  // Simple case: forced checked cast.
  if (auto forced = dyn_cast<ForcedCheckedCastExpr>(expr)) {
    return forced;
  }

  // If we have an implicit force, look through it.
  if (auto forced = dyn_cast<ForceValueExpr>(expr)) {
    if (forced->isImplicit()) {
      expr = forced->getSubExpr();
    }
  }

  // Skip through optional evaluations and binds.
  auto skipOptionalEvalAndBinds = [](Expr *expr) -> Expr* {
    do {
      if (!expr->isImplicit())
        break;

      if (auto optionalEval = dyn_cast<OptionalEvaluationExpr>(expr)) {
        expr = optionalEval->getSubExpr();
        continue;
      }

      if (auto bindOptional = dyn_cast<BindOptionalExpr>(expr)) {
        expr = bindOptional->getSubExpr();
        continue;
      }
      
      break;
    } while (true);

    return expr;
  };

  auto sub = skipOptionalEvalAndBinds(expr);
  
  // If we have an explicit cast, we're done.
  if (auto *FCE = dyn_cast<ForcedCheckedCastExpr>(sub))
    return FCE;

  // Otherwise, try to look through an implicit _forceBridgeFromObjectiveC() call.
  if (auto arg = lookThroughBridgeFromObjCCall(ctx, sub)) {
    sub = skipOptionalEvalAndBinds(arg);
    if (auto *FCE = dyn_cast<ForcedCheckedCastExpr>(sub))
      return FCE;
  }

  return nullptr;
}
