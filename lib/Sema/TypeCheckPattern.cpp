//===--- TypeCheckPattern.cpp - Type Checking for Patterns ----------------===//
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
// This file implements semantic analysis for patterns, analysing a
// pattern tree in both bottom-up and top-down ways.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/NameLookup.h"
#include <utility>
using namespace swift;

namespace {

/// Resolves an expression consisting of nested member references ending in a
/// decl reference to a type, returning true on success or false on failure.
class ResolveTypeReference : public ASTVisitor<ResolveTypeReference,
                                               /*ExprRetTy=*/ bool>
{
public:
  TypeChecker &TC;
  SmallVectorImpl<IdentTypeRepr::Component> &components;
  llvm::PointerUnion<Module*, TypeDecl*> curScope;
  Type curType;
  DeclContext *rootDC;

  ResolveTypeReference(TypeChecker &TC,
                       SmallVectorImpl<IdentTypeRepr::Component> &components,
                       DeclContext *rootDC)
    : TC(TC), components(components), rootDC(rootDC) {}
  
  Type doIt(Expr *e) {
    if (!visit(e))
      return Type();
    // If we succeeded, store the resolved type in the final component of the
    // identifier.
    if (curType)
      components.back().setValue(curType);
    
    return curType;
  }
  
  bool visitExpr(Expr *e) {
    return false;
  }
  
  bool visitDeclRefExpr(DeclRefExpr *dre) {
    assert(components.empty() && "decl ref should be root element of expr");
    assert(!curScope && "decl ref should be root element of expr");
    
    // Get the declared type.
    auto *td = dyn_cast<TypeDecl>(dre->getDecl());
    if (!td)
      return false;
    
    curScope = td;
    curType = td->getDeclaredType();
    
    // Track the AST location of the component.
    components.push_back({dre->getLoc(), dre->getDecl()->getName(), {}, nullptr});
    components.back().setValue(curScope.get<TypeDecl*>());
    return true;
  }
  
  bool visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *udre) {
    assert(components.empty() && "decl ref should be root element of expr");
    assert(!curScope && "decl ref should be root element of expr");
    
    // Look up the type.
    UnqualifiedLookup lookup(udre->getName(), rootDC);
    if (!lookup.isSuccess())
      return false;
    
    // Prefer a type result if we find one, but accept a module too.
    for (auto &result : lookup.Results) {
      switch (result.Kind) {
      case UnqualifiedLookupResult::ModuleName:
        // If we see two modules, bail on ambiguity.
        if (curScope.is<Module*>())
          return false;
        // If we already have a type, favor it.
        if (curScope.is<TypeDecl*>())
          continue;

        curScope = result.getNamedModule();
        break;
      
      // Ignore kinds that never refer to types.
      case UnqualifiedLookupResult::MemberProperty:
      case UnqualifiedLookupResult::MemberFunction:
        continue;
          
      // See whether we resolved a type.
      case UnqualifiedLookupResult::ModuleMember:
      case UnqualifiedLookupResult::LocalDecl:
      case UnqualifiedLookupResult::MetatypeMember:
      case UnqualifiedLookupResult::ExistentialMember:
      case UnqualifiedLookupResult::ArchetypeMember:
      case UnqualifiedLookupResult::MetaArchetypeMember: {
        auto *td = dyn_cast<TypeDecl>(result.getValueDecl());
        if (!td)
          continue;
        // If we already have a type, bail on ambiguity.
        if (curScope.is<TypeDecl*>())
          continue;
        
        curScope = td;
        curType = td->getDeclaredType();
        break;
      }
      }
    }
    
    assert(curScope && "shouldn't have got past the loop without a curType");
    
    // Track the AST location of the component.
    components.push_back({udre->getLoc(), udre->getName(), {}, nullptr});
    if (curScope.is<TypeDecl*>())
      components.back().setValue(curScope.get<TypeDecl*>());
    else
      components.back().setValue(curScope.get<Module*>());
    return true;
  }
  
  bool visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    if (!visit(ude->getBase()))
      return false;
    
    assert(!components.empty() && "no components before dot expr?!");
    assert(curScope && "no base context?!");
    
    // Do qualified lookup into the current scope.
    LookupTypeResult lookup;
    if (TypeDecl *td = curScope.dyn_cast<TypeDecl*>()) {
      // Only nominal types have members.
      auto *ntd = dyn_cast<NominalTypeDecl>(td);
      if (!ntd)
        return false;
      
      // Find a type member by the given name.
      lookup = TC.lookupMemberType(td->getDeclaredType(), ude->getName());
    } else if (Module *m = curScope.dyn_cast<Module*>()) {
      // Look into the module.
      lookup = TC.lookupMemberType(ModuleType::get(m), ude->getName());
    } else
      llvm_unreachable("invalid curType");
    
    // Make sure the lookup succeeded.
    if (!lookup || lookup.isAmbiguous())
      return false;
    
    curScope = lookup[0].first;
    curType = lookup[0].second;
    
    // Track the AST location of the new component.
    components.push_back({ude->getLoc(), ude->getName(), {}, nullptr});
    components.back().setValue(curScope.get<TypeDecl*>());
    return true;
  }
  
  bool visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *use) {
    if (!visit(use->getSubExpr()))
      return false;
    
    assert(!components.empty() && "no components before generic args?!");
    assert(curScope && "no base context?!");
    
    // Generic parameters don't affect lookup (yet; GADTs or
    // constrained extensions would change this), but apply the parameters for
    // validation and source tracking purposes.
    
    curType = TC.applyGenericArguments(curType, use->getLoc(),
                                       use->getUnresolvedParams());
    if (!curType)
      return false;
    
    // Track the AST location of the generic arguments.
    SmallVector<TypeRepr*, 4> argTypeReprs;
    for (auto &arg : use->getUnresolvedParams())
      argTypeReprs.push_back(arg.getTypeRepr());
    
    auto origComponent = components.back();
    components.back() = {origComponent.getIdLoc(),
                         origComponent.getIdentifier(),
                         TC.Context.AllocateCopy(argTypeReprs),
                         nullptr};
    components.back().setValue(origComponent.getBoundDecl());
    return true;
  }
};
  
/// Find a union element in a union type.
UnionElementDecl *
lookupUnionMemberElement(TypeChecker &TC, Type ty, Identifier name) {
  // The type must be a union.
  UnionDecl *oof = ty->getUnionOrBoundGenericUnion();
  if (!oof)
    return nullptr;
  
  // Look up the case inside the union.
  LookupResult foundElements = TC.lookupMember(ty, name,
                                               /*allowDynamicLookup=*/false);
  if (!foundElements)
    return nullptr;
  
  // See if there is any union element in there.
  UnionElementDecl *foundElement = nullptr;
  for (ValueDecl *e : foundElements) {
    auto *oe = dyn_cast<UnionElementDecl>(e);
    if (!oe)
      continue;
    // Ambiguities should be ruled out by parsing.
    assert(!foundElement && "ambiguity in union case name lookup?!");
    foundElement = oe;
  }
  
  return foundElement;
}
  
/// Resolve a chain of unresolved dot expressions 'T.U<V>.W' to a union element
/// reference, returning a TypeLoc over the type reference and the referenced
/// UnionElementDecl if successful, or returning null on failure.
static std::pair<TypeLoc, UnionElementDecl*>
lookupUnionElementReference(TypeChecker &TC,
                            UnresolvedDotExpr *refExpr,
                            DeclContext *DC) {
  // The left side of the dot needs to be a type; do type lookup on it.
  SmallVector<IdentTypeRepr::Component, 4> components;
  
  Type ty = ResolveTypeReference(TC, components, DC).doIt(refExpr->getBase());
  if (!ty)
    return {{}, nullptr};
  
  // Look up the case inside the union.
  UnionElementDecl *foundElement
    = lookupUnionMemberElement(TC, ty, refExpr->getName());
  if (!foundElement)
    return {{}, nullptr};
  
  // Build a TypeLoc to preserve AST location info for the reference chain.
  TypeRepr *repr
    = new (TC.Context) IdentTypeRepr(TC.Context.AllocateCopy(components));
  TypeLoc loc(repr);
  loc.setType(ty);
  return {loc, foundElement};
}

class ResolvePattern : public ASTVisitor<ResolvePattern,
                                         /*ExprRetTy=*/Pattern*,
                                         /*StmtRetTy=*/void,
                                         /*DeclRetTy=*/void,
                                         /*PatternRetTy=*/Pattern*>
{
public:
  TypeChecker &TC;
  DeclContext *DC;
  
  ResolvePattern(TypeChecker &TC, DeclContext *DC) : TC(TC), DC(DC) {}
  
  // Handle productions that are always leaf patterns or are already resolved.
#define ALWAYS_RESOLVED_PATTERN(Id) \
  Pattern *visit##Id##Pattern(Id##Pattern *P) { return P; }
  ALWAYS_RESOLVED_PATTERN(Named)
  ALWAYS_RESOLVED_PATTERN(Any)
  ALWAYS_RESOLVED_PATTERN(Isa)
  ALWAYS_RESOLVED_PATTERN(Paren)
  ALWAYS_RESOLVED_PATTERN(Tuple)
  ALWAYS_RESOLVED_PATTERN(NominalType)
  ALWAYS_RESOLVED_PATTERN(UnionElement)
  ALWAYS_RESOLVED_PATTERN(Typed)
#undef ALWAYS_RESOLVED_PATTERN

  Pattern *visitVarPattern(VarPattern *P) {
    Pattern *newSub = visit(P->getSubPattern());
    P->setSubPattern(newSub);
    return P;
  }
  
  Pattern *visitExprPattern(ExprPattern *P) {
    if (P->isResolved())
      return P;
    
    // Try to convert to a pattern.
    Pattern *exprAsPattern = visit(P->getSubExpr());
    // If we failed, keep the ExprPattern as is.
    if (!exprAsPattern) {
      P->setResolved(true);
      return P;
    }
    return exprAsPattern;
  }
  
  // Convert a subexpression to a pattern if possible, or wrap it in an
  // ExprPattern.
  Pattern *getSubExprPattern(Expr *E) {
    Pattern *p = visit(E);
    if (!p)
      return new (TC.Context) ExprPattern(E, nullptr, nullptr);
    return p;
  }
  
  // Most exprs remain exprs and should be wrapped in ExprPatterns.
  Pattern *visitExpr(Expr *E) {
    return nullptr;
  }
  
  // Unwrap UnresolvedPatternExprs.
  Pattern *visitUnresolvedPatternExpr(UnresolvedPatternExpr *E) {
    return visit(E->getSubPattern());
  }
  
  // Convert a paren expr to a pattern if it contains a pattern.
  Pattern *visitParenExpr(ParenExpr *E) {
    if (Pattern *subPattern = visit(E->getSubExpr()))
      return new (TC.Context) ParenPattern(E->getLParenLoc(), subPattern,
                                           E->getRParenLoc());
    return nullptr;
  }
  
  // Convert all tuples to patterns.
  Pattern *visitTupleExpr(TupleExpr *E) {
    // Construct a TuplePattern.
    // FIXME: Carry over field labels.
    SmallVector<TuplePatternElt, 4> patternElts;
    
    for (auto *subExpr : E->getElements()) {
      Pattern *pattern = getSubExprPattern(subExpr);
      patternElts.push_back(TuplePatternElt(pattern));
    }
    
    return TuplePattern::create(TC.Context, E->getLoc(),
                                patternElts, E->getRParenLoc());
  }
  
  // Unresolved member syntax '.Element' forms a UnionElement pattern. The
  // element will be resolved when we type-check the pattern.
  Pattern *visitUnresolvedMemberExpr(UnresolvedMemberExpr *ume) {
    return new (TC.Context) UnionElementPattern(TypeLoc(), ume->getDotLoc(),
                                                ume->getNameLoc(),
                                                ume->getName(),
                                                nullptr, nullptr);
  }
  
  // Member syntax 'T.Element' forms a pattern if 'T' is a union and the
  // member name is a member of the union.
  Pattern *visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    TypeLoc referencedType;
    UnionElementDecl *referencedDecl;
    std::tie(referencedType, referencedDecl)
      = lookupUnionElementReference(TC, ude, DC);
    
    if (!referencedDecl)
      return nullptr;
    
    return new (TC.Context) UnionElementPattern(referencedType,
                                                ude->getDotLoc(),
                                                ude->getNameLoc(),
                                                ude->getName(),
                                                referencedDecl,
                                                nullptr);
    
    return nullptr;
  }
  
  // Call syntax 'T.Element(x...)' or '.Element(x...)' forms a pattern if
  // the callee references a union element. The arguments then form a tuple
  // pattern matching the element's data.
  Pattern *visitCallExpr(CallExpr *ce) {
    // '.Element(x...)' is always treated as a UnionElementPattern.
    if (auto *ume = dyn_cast<UnresolvedMemberExpr>(ce->getFn())) {
      auto *subPattern = getSubExprPattern(ce->getArg());
      return new (TC.Context) UnionElementPattern(TypeLoc(), ume->getDotLoc(),
                                         ume->getNameLoc(),
                                         ume->getName(),
                                         nullptr,
                                         subPattern);
    }
    
    // 'T.Element(x...)' is treated as a UnionElementPattern if the dotted path
    // references a union element.
    
    if (auto *ude = dyn_cast<UnresolvedDotExpr>(ce->getFn())) {
      TypeLoc referencedType;
      UnionElementDecl *referencedDecl;
      std::tie(referencedType, referencedDecl)
        = lookupUnionElementReference(TC, ude, DC);
      
      if (!referencedDecl)
        return nullptr;
      
      if (auto oofElt = dyn_cast<UnionElementDecl>(referencedDecl)) {
        auto *subPattern = getSubExprPattern(ce->getArg());
        return new (TC.Context) UnionElementPattern(referencedType,
                                                    ude->getDotLoc(),
                                                    ude->getNameLoc(),
                                                    ude->getName(),
                                                    oofElt,
                                                    subPattern);
      }
    }

    return nullptr;
  }
};

} // end anonymous namespace

/// Perform top-down syntactic disambiguation of a pattern. Where ambiguous
/// expr/pattern productions occur (tuples, function calls, etc.), favor the
/// pattern interpretation if it forms a valid pattern; otherwise, leave it as
/// an expression.
Pattern *TypeChecker::resolvePattern(Pattern *P, DeclContext *DC) {
  return ResolvePattern(*this, DC).visit(P);
}

static bool validateTypedPattern(TypeChecker &TC, TypedPattern *TP,
                                 bool isVararg) {
  if (TP->hasType())
    return TP->getType()->is<ErrorType>();

  bool hadError = false;
  TypeLoc &TL = TP->getTypeLoc();
  if (TC.validateType(TL))
    hadError = true;
  Type Ty = TL.getType();

  if (isVararg && !hadError) {
    // FIXME: Use ellipsis loc for diagnostic.
    Ty = TC.getArraySliceType(TP->getLoc(), Ty);
    if (Ty.isNull())
      hadError = true;
  }

  if (hadError) {
    TP->setType(ErrorType::get(TC.Context));
  } else {
    TP->setType(Ty);
  }
  return hadError;
}

/// Perform bottom-up type-checking on a pattern.  If this returns
/// false, the type of the pattern will have been set.  If allowUnknownTypes is
/// true, then this accepts "any" and "named" patterns, setting their type to
/// UnresolvedType.
bool TypeChecker::typeCheckPattern(Pattern *P, DeclContext *dc,
                                   bool allowUnknownTypes,
                                   bool isVararg) {
  switch (P->getKind()) {
  // Type-check paren patterns by checking the sub-pattern and
  // propagating that type out.
  case PatternKind::Paren: {
    Pattern *SP = cast<ParenPattern>(P)->getSubPattern();
    if (typeCheckPattern(SP, dc, allowUnknownTypes)) {
      P->setType(ErrorType::get(Context));
      return true;
    }
    if (SP->hasType())
      P->setType(SP->getType());
    return false;
  }

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    TypedPattern *TP = cast<TypedPattern>(P);
    bool hadError = validateTypedPattern(*this, TP, isVararg);
    hadError |= coerceToType(TP->getSubPattern(), dc, P->getType());
    return hadError;
  }

  // A wildcard or name pattern cannot appear by itself in a context
  // which requires an explicit type.
  case PatternKind::Any:
  case PatternKind::Named:
    // If we're type checking this pattern in a context that can provide type
    // information, then the lack of type information is not an error.
    if (allowUnknownTypes) {
      return false;
    }
      
    diagnose(P->getLoc(), diag::cannot_infer_type_for_pattern);
    P->setType(ErrorType::get(Context));
    if (auto named = dyn_cast<NamedPattern>(P)) {
      if (auto var = named->getDecl())
        var->setType(ErrorType::get(Context));
    }
    return true;

  // A tuple pattern propagates its tuple-ness out.
  case PatternKind::Tuple: {
    auto tuplePat = cast<TuplePattern>(P);
    bool hadError = false;
    SmallVector<TupleTypeElt, 8> typeElts;

    bool missingType = false;
    for (unsigned i = 0, e = tuplePat->getFields().size(); i != e; ++i) {
      TuplePatternElt &elt = tuplePat->getFields()[i];
      Pattern *pattern = elt.getPattern();
      bool isVararg = tuplePat->hasVararg() && i == e-1;
      if (typeCheckPattern(pattern, dc, allowUnknownTypes, isVararg)) {
        hadError = true;
        continue;
      }
      if (!pattern->hasType()) {
        missingType = true;
        continue;
      }

      typeElts.push_back(TupleTypeElt(pattern->getType(),
                                      pattern->getBoundName(),
                                      elt.getDefaultArgKind(),
                                      isVararg));
    }

    if (hadError) {
      P->setType(ErrorType::get(Context));
      return true;
    }
    if (!missingType && !allowUnknownTypes)
      P->setType(TupleType::get(typeElts, Context));
    return false;
  }

#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("bottom-up type checking of refutable patterns "
                     "not implemented");
  }
  llvm_unreachable("bad pattern kind!");
}

/// Perform top-down type coercion on the given pattern.
bool TypeChecker::coerceToType(Pattern *P, DeclContext *dc, Type type,
                               bool isVararg) {
  switch (P->getKind()) {
  // For parens and vars, just set the type annotation and propagate inwards.
  case PatternKind::Paren:
    P->setType(type);
    return coerceToType(cast<ParenPattern>(P)->getSubPattern(), dc, type);
  case PatternKind::Var:
    P->setType(type);
    return coerceToType(cast<VarPattern>(P)->getSubPattern(), dc, type);

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    TypedPattern *TP = cast<TypedPattern>(P);
    bool hadError = validateTypedPattern(*this, TP, isVararg);
    if (!hadError) {
      if (!type->isEqual(TP->getType()) && !type->is<ErrorType>()) {
        // Complain if the types don't match exactly.
        // TODO: allow implicit conversions?
        diagnose(P->getLoc(), diag::pattern_type_mismatch_context, type);
        hadError = true;
      }
    }

    hadError |= coerceToType(TP->getSubPattern(), dc, TP->getType());
    return hadError;
  }

  // For wildcard and name patterns, just set the type.
  case PatternKind::Named: {
    NamedPattern *NP = cast<NamedPattern>(P);
    NP->getDecl()->overwriteType(type);

    P->setType(type);
    return false;
  }
  case PatternKind::Any:
    P->setType(type);
    return false;

  // We can match a tuple pattern with a tuple type.
  // TODO: permit implicit conversions?
  case PatternKind::Tuple: {
    TuplePattern *TP = cast<TuplePattern>(P);
    bool hadError = false;

    if (type->is<ErrorType>())
      hadError = true;

    // The context type must be a tuple.
    TupleType *tupleTy = type->getAs<TupleType>();
    if (!tupleTy && !hadError) {
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_in_non_tuple_context,
               type);
      hadError = true;
    }

    // The number of elements must match exactly.
    // TODO: incomplete tuple patterns, with some syntax.
    if (!hadError && tupleTy->getFields().size() != TP->getNumFields()) {
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_length_mismatch, type);
      hadError = true;
    }

    // Coerce each tuple element to the respective type.
    // TODO: detect and diagnose shuffling
    // TODO: permit shuffling
    P->setType(type);

    for (unsigned i = 0, e = TP->getNumFields(); i != e; ++i) {
      TuplePatternElt &elt = TP->getFields()[i];
      Pattern *pattern = elt.getPattern();
      bool isVararg = TP->hasVararg() && i == e-1;

      Type CoercionType;
      if (hadError)
        CoercionType = ErrorType::get(Context);
      else
        CoercionType = tupleTy->getFields()[i].getType();

      hadError |= coerceToType(pattern, dc, CoercionType, isVararg);

      // Type-check the initialization expression.
      if (ExprHandle *initHandle = elt.getInit()) {
        Expr *init = initHandle->getExpr();
        if (initHandle->alreadyChecked()) {
          // Nothing to do
        } else if (typeCheckExpression(init, dc, CoercionType,
                                       /*discardedExpr=*/false)) {
          initHandle->setExpr(initHandle->getExpr(), true);
        } else {
          initHandle->setExpr(init, true);
        }
      }
    }

    return hadError;
  }

  // Coerce expressions by finding a '~=' operator that can compare the
  // expression to a value of the coerced type.
  case PatternKind::Expr: {
    assert(cast<ExprPattern>(P)->isResolved()
           && "coercing unresolved expr pattern!");
    return typeCheckExprPattern(cast<ExprPattern>(P), dc, type);
  }
      
  // Coerce an 'is' pattern by determining the cast kind.
  case PatternKind::Isa: {
    auto IP = cast<IsaPattern>(P);

    // Type-check the type parameter.
    if (validateType(IP->getCastTypeLoc()))
      return nullptr;
    
    CheckedCastKind castKind
      = typeCheckCheckedCast(type, IP->getCastTypeLoc().getType(),
                             IP->getLoc(),
                             IP->getLoc(),IP->getCastTypeLoc().getSourceRange(),
                             [](Type) { return false; });
    switch (castKind) {
    case CheckedCastKind::Unresolved:
      return nullptr;
    case CheckedCastKind::InvalidCoercible:
      diagnose(IP->getLoc(), diag::isa_is_always_true,
               type,
               IP->getCastTypeLoc().getType());
      return nullptr;
    // Valid checks.
    case CheckedCastKind::Downcast:
    case CheckedCastKind::SuperToArchetype:
    case CheckedCastKind::ArchetypeToArchetype:
    case CheckedCastKind::ArchetypeToConcrete:
    case CheckedCastKind::ExistentialToArchetype:
    case CheckedCastKind::ExistentialToConcrete:
      IP->setCastKind(castKind);
      break;
    }
    
    IP->setType(type);
    return false;
  }
      
  case PatternKind::UnionElement: {
    auto *OP = cast<UnionElementPattern>(P);
    
    // If the element decl was not resolved (because it was spelled without a
    // type as `.Foo`), resolve it now that we have a type.
    if (!OP->getElementDecl()) {
      UnionElementDecl *element
        = lookupUnionMemberElement(*this, type, OP->getName());
      if (!element) {
        diagnose(OP->getLoc(), diag::union_element_pattern_member_not_found,
                 OP->getName().str(), type);
        return true;
      }
      OP->setElementDecl(element);
    }
    
    // If there is a subpattern, push the union element type down onto it.
    if (OP->hasSubPattern()) {
      UnionElementDecl *elt = OP->getElementDecl();
      Type elementType;
      if (elt->hasArgumentType())
        elementType = type->getTypeOfMember(elt->getModuleContext(),
                                            elt, this,
                                            elt->getArgumentType());
      else
        elementType = TupleType::getEmpty(Context);
      if (coerceToType(OP->getSubPattern(), dc, elementType))
        return true;
    }
    OP->setType(type);
    return false;
  }
      
  case PatternKind::NominalType:
    // TODO: Check the type against the coerced type, then coerce the
    // subpatterns to the field types.
    llvm_unreachable("not implemented");
  }
  llvm_unreachable("bad pattern kind!");
}
