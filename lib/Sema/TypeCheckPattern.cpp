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
#include "GenericTypeResolver.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/NameLookup.h"
#include "swift/Parse/Lexer.h"
#include <utility>
using namespace swift;

/// Find an unqualified enum element.
static EnumElementDecl *
lookupUnqualifiedEnumMemberElement(TypeChecker &TC, DeclContext *DC,
                                   Identifier name) {
  UnqualifiedLookup lookup(name, DC, &TC, /*non-cascading=*/true,
                           SourceLoc(), /*typeLookup*/false);
  
  if (!lookup.isSuccess())
    return nullptr;

  // See if there is any enum element in there.
  EnumElementDecl *foundElement = nullptr;
  for (auto result : lookup.Results) {
    if (!result.hasValueDecl())
      continue;
    auto *oe = dyn_cast<EnumElementDecl>(result.getValueDecl());
    if (!oe)
      continue;
    // Ambiguities should be ruled out by parsing.
    assert(!foundElement && "ambiguity in enum case name lookup?!");
    foundElement = oe;
  }
  return foundElement;
}

/// Find an enum element in an enum type.
static EnumElementDecl *
lookupEnumMemberElement(TypeChecker &TC, DeclContext *DC, Type ty,
                        Identifier name) {
  // Look up the case inside the enum.
  // FIXME: We should be able to tell if this is a private lookup.
  LookupResult foundElements = TC.lookupMember(ty, name, DC,
                                               /*isKnownPrivate=*/false,
                                               /*allowDynamicLookup=*/false);
  if (!foundElements)
    return nullptr;
  
  // See if there is any enum element in there.
  EnumElementDecl *foundElement = nullptr;
  for (ValueDecl *e : foundElements) {
    auto *oe = dyn_cast<EnumElementDecl>(e);
    if (!oe)
      continue;
    // Ambiguities should be ruled out by parsing.
    assert(!foundElement && "ambiguity in enum case name lookup?!");
    foundElement = oe;
  }
  
  return foundElement;
}

namespace {
// 'T(x...)' is treated as a NominalTypePattern if 'T' references a type
// by name, or an EnumElementPattern if 'T' references an enum element.
// Build up an IdentTypeRepr and see what it resolves to.
struct ExprToIdentTypeRepr : public ASTVisitor<ExprToIdentTypeRepr, bool>
{
  SmallVectorImpl<ComponentIdentTypeRepr *> &components;
  ASTContext &C;

  ExprToIdentTypeRepr(decltype(components) &components, ASTContext &C)
    : components(components), C(C) {}
  
  bool visitExpr(Expr *e) {
    return false;
  }

  bool visitDeclRefExpr(DeclRefExpr *dre) {
    assert(components.empty() && "decl ref should be root element of expr");
    
    // Get the declared type.
    if (auto *td = dyn_cast<TypeDecl>(dre->getDecl())) {
      components.push_back(
        new (C) SimpleIdentTypeRepr(dre->getLoc(), dre->getDecl()->getName()));
      components.back()->setValue(td);
      return true;
    }
    return false;
  }
  
  bool visitModuleExpr(ModuleExpr *me) {
    assert(components.empty() && "decl ref should be root element of expr");
    
    // Add the declared module.
    auto module = me->getType()->getAs<ModuleType>()->getModule();
    components.push_back(
      new (C) SimpleIdentTypeRepr(me->getLoc(), module->Name));
    components.back()->setValue(module);
    return true;
  }
  
  bool visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *udre) {
    assert(components.empty() && "decl ref should be root element of expr");
    // Track the AST location of the component.
    components.push_back(
      new (C) SimpleIdentTypeRepr(udre->getLoc(), udre->getName()));
    return true;
  }
  
  bool visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    if (!visit(ude->getBase()))
      return false;
    
    assert(!components.empty() && "no components before dot expr?!");

    // Track the AST location of the new component.
    components.push_back(
      new (C) SimpleIdentTypeRepr(ude->getLoc(), ude->getName()));
    return true;
  }
  
  bool visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *use) {
    if (!visit(use->getSubExpr()))
      return false;
    
    assert(!components.empty() && "no components before generic args?!");
    
    // Track the AST location of the generic arguments.
    SmallVector<TypeRepr*, 4> argTypeReprs;
    for (auto &arg : use->getUnresolvedParams())
      argTypeReprs.push_back(arg.getTypeRepr());
    auto origComponent = components.back();
    components.back() = new (C) GenericIdentTypeRepr(origComponent->getIdLoc(),
      origComponent->getIdentifier(),
      C.AllocateCopy(argTypeReprs),
      SourceRange(use->getLAngleLoc(), use->getRAngleLoc()));

    return true;
  }
};
  
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
  ALWAYS_RESOLVED_PATTERN(Is)
  ALWAYS_RESOLVED_PATTERN(Paren)
  ALWAYS_RESOLVED_PATTERN(Tuple)
  ALWAYS_RESOLVED_PATTERN(NominalType)
  ALWAYS_RESOLVED_PATTERN(EnumElement)
  ALWAYS_RESOLVED_PATTERN(Typed)
#undef ALWAYS_RESOLVED_PATTERN

  Pattern *visitVarPattern(VarPattern *P) {
    Pattern *newSub = visit(P->getSubPattern());
    P->setSubPattern(newSub);
    return P;
  }

  Pattern *visitOptionalSomePattern(OptionalSomePattern *P) {
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
  
  // Convert a '_' expression to an AnyPattern.
  Pattern *visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) {
    return new (TC.Context) AnyPattern(E->getLoc(), E->isImplicit());
  }
  
  // Cast expressions 'x as T' get resolved to checked cast patterns.
  // Pattern resolution occurs before sequence resolution, so the cast will
  // appear as a SequenceExpr.
  Pattern *visitSequenceExpr(SequenceExpr *E) {
    if (E->getElements().size() != 3)
      return nullptr;
    auto cast = dyn_cast<CoerceExpr>(E->getElement(1));
    if (!cast)
      return nullptr;
    
    Pattern *subPattern = getSubExprPattern(E->getElement(0));
    return new (TC.Context) IsPattern(cast->getLoc(),
                                       cast->getCastTypeLoc(),
                                       subPattern,
                                       CheckedCastKind::Unresolved);
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

  Pattern *convertBindingsToOptionalSome(Expr *E) {
    auto *Bind = dyn_cast<BindOptionalExpr>(E->getSemanticsProvidingExpr());
    if (!Bind) return getSubExprPattern(E);

    auto sub = convertBindingsToOptionalSome(Bind->getSubExpr());
    return new (TC.Context) OptionalSomePattern(sub, Bind->getQuestionLoc());
  }

  // Convert a x? to OptionalSome pattern.  In the AST form, this will look like
  // an OptionalEvaluationExpr with an immediate BindOptionalExpr inside of it.
  Pattern *visitOptionalEvaluationExpr(OptionalEvaluationExpr *E) {
    // We only handle the case where one or more bind expressions are subexprs
    // of the optional evaluation.  Other cases are not simple postfix ?'s.
    if (!isa<BindOptionalExpr>(E->getSubExpr()->getSemanticsProvidingExpr()))
      return nullptr;

    return convertBindingsToOptionalSome(E->getSubExpr());
  }


  // Unresolved member syntax '.Element' forms an EnumElement pattern. The
  // element will be resolved when we type-check the pattern.
  Pattern *visitUnresolvedMemberExpr(UnresolvedMemberExpr *ume) {
    // We the unresolved member has an argument, turn it into a subpattern.
    Pattern *subPattern = nullptr;
    if (auto arg = ume->getArgument()) {
      subPattern = getSubExprPattern(arg);
    }
    
    return new (TC.Context) EnumElementPattern(TypeLoc(), ume->getDotLoc(),
                                               ume->getNameLoc(),
                                               ume->getName(),
                                               nullptr,
                                               subPattern);
  }
  
  // Member syntax 'T.Element' forms a pattern if 'T' is an enum and the
  // member name is a member of the enum.
  Pattern *visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    GenericTypeToArchetypeResolver resolver;
    SmallVector<ComponentIdentTypeRepr *, 2> components;
    if (!ExprToIdentTypeRepr(components, TC.Context).visit(ude->getBase()))
      return nullptr;
    
    auto *repr = IdentTypeRepr::create(TC.Context, components);
      
    // See if the repr resolves to a type.
    Type ty = TC.resolveIdentifierType(DC, repr, TR_AllowUnboundGenerics,
                                       /*diagnoseErrors*/false, &resolver);
    
    auto *enumDecl = dyn_cast_or_null<EnumDecl>(ty->getAnyNominal());
    if (!enumDecl)
      return nullptr;

    EnumElementDecl *referencedElement
      = lookupEnumMemberElement(TC, DC, ty, ude->getName());
    
    // Build a TypeRepr from the head of the full path.
    TypeLoc loc(repr);
    loc.setType(ty);
    return new (TC.Context) EnumElementPattern(loc,
                                               ude->getDotLoc(),
                                               ude->getNameLoc(),
                                               ude->getName(),
                                               referencedElement,
                                               nullptr);
  }
  
  // A DeclRef 'E' that refers to an enum element forms an EnumElementPattern.
  Pattern *visitDeclRefExpr(DeclRefExpr *de) {
    auto *elt = dyn_cast<EnumElementDecl>(de->getDecl());
    if (!elt)
      return nullptr;
    
    // Use the type of the enum from context.
    TypeLoc loc = TypeLoc::withoutLoc(
                            elt->getParentEnum()->getDeclaredTypeInContext());
    return new (TC.Context) EnumElementPattern(loc, SourceLoc(),
                                               de->getLoc(),
                                               elt->getName(),
                                               elt,
                                               nullptr);
  }
  Pattern *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *ude) {
    // Try looking up an enum element in context.
    EnumElementDecl *referencedElement
      = lookupUnqualifiedEnumMemberElement(TC, DC, ude->getName());
    
    if (!referencedElement)
      return nullptr;
    
    auto *enumDecl = referencedElement->getParentEnum();
    auto enumTy = enumDecl->getDeclaredTypeInContext();
    TypeLoc loc = TypeLoc::withoutLoc(enumTy);
    
    return new (TC.Context) EnumElementPattern(loc, SourceLoc(),
                                               ude->getLoc(),
                                               ude->getName(),
                                               referencedElement,
                                               nullptr);
  }
  
  // Call syntax forms a pattern if:
  // - the callee in 'Element(x...)' or '.Element(x...)'
  //   references an enum element. The arguments then form a tuple
  //   pattern matching the element's data.
  // - the callee in 'T(...)' is a struct or class type. The argument tuple is
  //   then required to have keywords for every argument that name properties
  //   of the type.
  Pattern *visitCallExpr(CallExpr *ce) {
    PartialGenericTypeToArchetypeResolver resolver(TC);
    
    SmallVector<ComponentIdentTypeRepr *, 2> components;
    if (!ExprToIdentTypeRepr(components, TC.Context).visit(ce->getFn()))
      return nullptr;
    
    if (components.empty())
      return nullptr;
    auto *repr = IdentTypeRepr::create(TC.Context, components);
    
    // See first if the entire repr resolves to a type.
    Type ty = TC.resolveIdentifierType(DC, repr, TR_AllowUnboundGenerics,
                                       /*diagnoseErrors*/false, &resolver);
    
    // If we got a fully valid type, then this is a nominal type pattern.
    // FIXME: Only when experimental patterns are enabled for now.
    if (!ty->is<ErrorType>()
        && TC.Context.LangOpts.EnableExperimentalPatterns) {
      // Validate the argument tuple elements as nominal type pattern fields.
      // They must all have keywords. For recovery, we still form the pattern
      // even if one or more elements are missing keywords.
      auto *argTuple = dyn_cast<TupleExpr>(ce->getArg());
      SmallVector<NominalTypePattern::Element, 4> elements;
      
      if (!argTuple) {
        TC.diagnose(ce->getArg()->getLoc(),
                    diag::nominal_type_subpattern_without_property_name);
        elements.push_back({SourceLoc(), Identifier(), nullptr,
                            SourceLoc(), getSubExprPattern(ce->getArg())});
      } else for (unsigned i = 0, e = argTuple->getNumElements(); i < e; ++i) {
        if (argTuple->getElementName(i).empty()) {
          TC.diagnose(argTuple->getElement(i)->getLoc(),
                      diag::nominal_type_subpattern_without_property_name);
        }
        
        // FIXME: TupleExpr doesn't preserve location of keyword name or colon.
        elements.push_back({SourceLoc(),
                            argTuple->getElementName(i),
                            nullptr,
                            SourceLoc(),
                            getSubExprPattern(argTuple->getElement(i))});
      }
      
      // Build a TypeLoc to preserve AST location info for the reference chain.
      TypeLoc loc(repr);
      loc.setType(ty);
      
      return NominalTypePattern::create(loc,
                                        ce->getArg()->getStartLoc(),
                                        elements,
                                        ce->getArg()->getEndLoc(),
                                        TC.Context);
    }

    // If we had a single component, try looking up an enum element in context.
    if (auto compId = dyn_cast<ComponentIdentTypeRepr>(repr)) {
      // Try looking up an enum element in context.
      EnumElementDecl *referencedElement
        = lookupUnqualifiedEnumMemberElement(TC, DC, compId->getIdentifier());
      
      if (!referencedElement)
        return nullptr;
      
      auto *enumDecl = referencedElement->getParentEnum();
      auto enumTy = enumDecl->getDeclaredTypeInContext();
      TypeLoc loc = TypeLoc::withoutLoc(enumTy);

      auto *subPattern = getSubExprPattern(ce->getArg());
      return new (TC.Context) EnumElementPattern(loc,
                                                 SourceLoc(),
                                                 compId->getIdLoc(),
                                                 compId->getIdentifier(),
                                                 referencedElement,
                                                 subPattern);
    }
    
    // Otherwise, see whether we had an enum type as the penultimate component,
    // and look up an element inside it.
    auto compoundR = cast<CompoundIdentTypeRepr>(repr);
    if (!compoundR->Components.end()[-2]->isBoundType())
      return nullptr;
    
    Type enumTy = compoundR->Components.end()[-2]->getBoundType();
    auto *enumDecl = dyn_cast_or_null<EnumDecl>(enumTy->getAnyNominal());
    if (!enumDecl)
      return nullptr;
    
    auto tailComponent = compoundR->Components.back();
    
    EnumElementDecl *referencedElement
      = lookupEnumMemberElement(TC, DC, enumTy, tailComponent->getIdentifier());
    if (!referencedElement)
      return nullptr;
    
    // Build a TypeRepr from the head of the full path.
    TypeLoc loc;
    IdentTypeRepr *subRepr;
    auto headComps =
      compoundR->Components.slice(0, compoundR->Components.size() - 1);
    if (headComps.size() == 1)
      subRepr = headComps.front();
    else
      subRepr = new (TC.Context) CompoundIdentTypeRepr(headComps);
    loc = TypeLoc(subRepr);
    loc.setType(enumTy);
    
    auto *subPattern = getSubExprPattern(ce->getArg());
    return new (TC.Context) EnumElementPattern(loc,
                                               SourceLoc(),
                                               tailComponent->getIdLoc(),
                                               tailComponent->getIdentifier(),
                                               referencedElement,
                                               subPattern);
  }
};

} // end anonymous namespace

/// Perform top-down syntactic disambiguation of a pattern. Where ambiguous
/// expr/pattern productions occur (tuples, function calls, etc.), favor the
/// pattern interpretation if it forms a valid pattern; otherwise, leave it as
/// an expression. This does no type-checking except for the bare minimum to
/// disambiguate semantics-dependent pattern forms.
Pattern *TypeChecker::resolvePattern(Pattern *P, DeclContext *DC) {
  return ResolvePattern(*this, DC).visit(P);
}

static bool validateTypedPattern(TypeChecker &TC, DeclContext *DC,
                                 TypedPattern *TP,
                                 TypeResolutionOptions options,
                                 GenericTypeResolver *resolver) {
  if (TP->hasType())
    return TP->getType()->is<ErrorType>();

  bool hadError = false;
  TypeLoc &TL = TP->getTypeLoc();
  if (TC.validateType(TL, DC, options, resolver))
    hadError = true;
  Type Ty = TL.getType();

  if ((options & TR_Variadic) && !hadError) {
    // If isn't legal to declare something both inout and variadic.
    if (Ty->is<InOutType>()) {
      TC.diagnose(TP->getLoc(), diag::inout_cant_be_variadic);
      hadError = true;
    } else {
      // FIXME: Use ellipsis loc for diagnostic.
      Ty = TC.getArraySliceType(TP->getLoc(), Ty);
      if (Ty.isNull())
        hadError = true;
    }
  }

  if (hadError) {
    TP->setType(ErrorType::get(TC.Context));
  } else {
    TP->setType(Ty);
  }
  return hadError;
}

bool TypeChecker::typeCheckPattern(Pattern *P, DeclContext *dc,
                                   TypeResolutionOptions options,
                                   GenericTypeResolver *resolver) {
  // Make sure we always have a resolver to use.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  TypeResolutionOptions subOptions = options - TR_Variadic;
  switch (P->getKind()) {
  // Type-check paren patterns by checking the sub-pattern and
  // propagating that type out.
  case PatternKind::Paren:
  case PatternKind::Var: {
    Pattern *SP;
    if (auto *PP = dyn_cast<ParenPattern>(P))
      SP = PP->getSubPattern();
    else
      SP = cast<VarPattern>(P)->getSubPattern();
    if (typeCheckPattern(SP, dc, subOptions, resolver)) {
      P->setType(ErrorType::get(Context));
      return true;
    }
    if (SP->hasType()) {
      auto type = SP->getType();
      if (P->getKind() == PatternKind::Paren)
        type = ParenType::get(Context, type);
      P->setType(type);
    }
    return false;
  }

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    TypedPattern *TP = cast<TypedPattern>(P);
    bool hadError = validateTypedPattern(*this, dc, TP, options, resolver);
    Pattern *subPattern = TP->getSubPattern();
    if (coercePatternToType(subPattern, dc, P->getType(),
                            options|TR_FromNonInferredPattern, resolver))
      hadError = true;
    else {
      TP->setSubPattern(subPattern);
      TP->setType(subPattern->getType());
    }
    return hadError;
  }

  // A wildcard or name pattern cannot appear by itself in a context
  // which requires an explicit type.
  case PatternKind::Any:
  case PatternKind::Named:
    // If we're type checking this pattern in a context that can provide type
    // information, then the lack of type information is not an error.
    if (options & TR_AllowUnspecifiedTypes)
      return false;

    diagnose(P->getLoc(), diag::cannot_infer_type_for_pattern);
    P->setType(ErrorType::get(Context));
    if (auto named = dyn_cast<NamedPattern>(P)) {
      if (auto var = named->getDecl()) {
        var->setType(ErrorType::get(Context));
      }
    }
    return true;

  // A tuple pattern propagates its tuple-ness out.
  case PatternKind::Tuple: {
    auto tuplePat = cast<TuplePattern>(P);
    bool hadError = false;
    SmallVector<TupleTypeElt, 8> typeElts;

    // If this is the top level of a function input list, peel off the
    // ImmediateFunctionInput marker and install a FunctionInput one instead.
    auto elementOptions = withoutContext(subOptions);
    if (subOptions & TR_ImmediateFunctionInput)
      elementOptions |= TR_FunctionInput;

    bool missingType = false;
    for (unsigned i = 0, e = tuplePat->getFields().size(); i != e; ++i) {
      TuplePatternElt &elt = tuplePat->getFields()[i];
      Pattern *pattern = elt.getPattern();
      bool isVararg = tuplePat->hasVararg() && i == e-1;
      TypeResolutionOptions eltOptions = elementOptions;
      if (isVararg)
        eltOptions |= TR_Variadic;
      if (typeCheckPattern(pattern, dc, eltOptions, resolver)){
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
    if (!missingType && !(options & TR_AllowUnspecifiedTypes))
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

/// Coerce the given 'isa' pattern via a conditional downcast.
///
/// This allows us to use an arbitrary conditional downcast to
/// evaluate an "is" / "as" pattern, which includes any kind of
/// downcast for which we don't have specialized logic.
static bool coercePatternViaConditionalDowncast(TypeChecker &tc, 
                                                Pattern *&pattern,
                                                DeclContext *dc,
                                                Type type,
                                                TypeResolutionOptions options) {
  auto isa = cast<IsPattern>(pattern);

  // FIXME: We can't handle subpatterns here.
  if (isa->getSubPattern()) {
    tc.diagnose(isa->getLoc(), diag::isa_pattern_value, 
                isa->getCastTypeLoc().getType());
    return false;
  }

  // Create a new match variable $match.
  auto *matchVar = new (tc.Context) VarDecl(/*static*/ false, /*IsLet*/true,
                                            pattern->getLoc(),
                                            tc.Context.getIdentifier("$match"),
                                            type, dc);
  matchVar->setHasNonPatternBindingInit();

  // Form the cast $match as? T, which produces an optional.
  Expr *matchRef = new (tc.Context) DeclRefExpr(matchVar, pattern->getLoc(),
                                                /*Implicit=*/true);
  Expr *cast = new (tc.Context) ConditionalCheckedCastExpr(
                                  matchRef,
                                  isa->getLoc(),
                                  isa->getLoc(),
                                  isa->getCastTypeLoc());

  // Type-check the cast as a condition.
  if (tc.typeCheckCondition(cast, dc))
    return true;

  // Form an expression pattern with this match.
  // FIXME: This is lossy; we can't get the value out.
  pattern = new (tc.Context) ExprPattern(matchRef, /*isResolved=*/true, 
                                         /*matchExpr=*/cast, matchVar,
                                         false);
  pattern->setType(isa->getCastTypeLoc().getType());
  return false;
}

/// Perform top-down type coercion on the given pattern.
bool TypeChecker::coercePatternToType(Pattern *&P, DeclContext *dc, Type type,
                                      TypeResolutionOptions options,
                                      GenericTypeResolver *resolver) {
  TypeResolutionOptions subOptions
    = options - TR_Variadic - TR_EnumPatternPayload;
  switch (P->getKind()) {
  // For parens and vars, just set the type annotation and propagate inwards.
  case PatternKind::Paren: {
    auto PP = cast<ParenPattern>(P);
    auto sub = PP->getSubPattern();
    auto semantic = P->getSemanticsProvidingPattern();
    // If this is the payload of an enum, and the type is a single-element
    // labeled tuple, treat this as a tuple pattern. It's unlikely that the
    // user is interested in binding a variable of type (foo: Int).
    if ((options & TR_EnumPatternPayload)
        && !isa<TuplePattern>(semantic)) {
      if (auto tupleType = type->getAs<TupleType>()) {
        if (tupleType->getNumElements() == 1
            && !tupleType->getFields()[0].isVararg()) {
          auto elementTy = tupleType->getElementType(0);
          if (coercePatternToType(sub, dc, elementTy, subOptions, resolver))
            return true;
          TuplePatternElt elt(sub);
          P = TuplePattern::create(Context, PP->getLParenLoc(), elt,
                                   PP->getRParenLoc());
          if (PP->isImplicit())
            P->setImplicit();
          P->setType(type);
          return false;
        }
      }
    }
  
    if (coercePatternToType(sub, dc, type, subOptions, resolver))
      return true;
    PP->setSubPattern(sub);
    PP->setType(sub->getType());
    return false;
  }
  case PatternKind::Var: {
    auto VP = cast<VarPattern>(P);
    
    Pattern *sub = VP->getSubPattern();
    if (coercePatternToType(sub, dc, type, subOptions, resolver))
      return true;
    VP->setSubPattern(sub);
    if (sub->hasType())
      VP->setType(sub->getType());
    return false;
  }

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    TypedPattern *TP = cast<TypedPattern>(P);
    bool hadError = validateTypedPattern(*this, dc, TP, options, resolver);
    if (!hadError) {
      if (!type->isEqual(TP->getType()) && !type->is<ErrorType>()) {
        if (options & TR_OverrideType) {
          TP->overwriteType(type);
        } else {
          diagnose(P->getLoc(), diag::pattern_type_mismatch_context, type);
          hadError = true;
        }
      }
    }

    Pattern *sub = TP->getSubPattern();
    hadError |= coercePatternToType(sub, dc, TP->getType(),
                                    subOptions | TR_FromNonInferredPattern,
                                    resolver);
    if (!hadError) {
      TP->setSubPattern(sub);
      TP->setType(sub->getType());
    }
    return hadError;
  }

  // For wildcard and name patterns, set the type.
  case PatternKind::Named: {
    NamedPattern *NP = cast<NamedPattern>(P);
    VarDecl *var = NP->getDecl();
    if (var->isInvalid())
      var->overwriteType(ErrorType::get(Context));
    else
      var->overwriteType(type);

    checkTypeModifyingDeclAttributes(var);
    if (type->is<InOutType>())
      NP->getDecl()->setLet(false);
    if (var->getAttrs().hasAttribute<OwnershipAttr>())
      type = getTypeOfRValue(var, true);
    else if (!var->isInvalid())
      type = var->getType();
    P->setType(type);
    
    // If we are inferring a variable to have type AnyObject, AnyObject.Type,
    // "()", or optional thereof, emit a diagnostic.  In the first 2 cases, the
    // coder probably forgot a cast and expected a concrete type.  In the later
    // case, they probably didn't mean to bind to a variable, or there is some
    // other bug.  We always tell them that they can silence the warning with an
    // explicit type annotation (and provide a fixit) as a note.
    Type diagTy = type->getAnyOptionalObjectType();
    if (!diagTy) diagTy = type;
    
    bool shouldRequireType = false;
    if (NP->isImplicit()) {
      // If the whole pattern is implicit, the user didn't write it.
      // Assume the compiler knows what it's doing.
    } else if (diagTy->getCanonicalType() == Context.TheEmptyTupleType) {
      shouldRequireType = true;
    } else if (auto protoTy = diagTy->getAs<ProtocolType>()) {
      shouldRequireType =
        protoTy->getDecl()->isSpecificProtocol(KnownProtocolKind::AnyObject);
    } else if (auto MTT = diagTy->getAs<AnyMetatypeType>()) {
      if (auto protoTy = MTT->getInstanceType()->getAs<ProtocolType>()) {
        shouldRequireType =
          protoTy->getDecl()->isSpecificProtocol(KnownProtocolKind::AnyObject);
      }
    }
    
    if (shouldRequireType && 
        !(options & TR_FromNonInferredPattern) &&
        !(options & TR_EnumerationVariable)) {
      diagnose(NP->getLoc(), diag::type_inferred_to_undesirable_type,
               NP->getDecl()->getName(), type, NP->getDecl()->isLet());

      SourceLoc fixItLoc = NP->getLoc();
      fixItLoc = Lexer::getLocForEndOfToken(Context.SourceMgr, fixItLoc);
      diagnose(NP->getLoc(), diag::add_explicit_type_annotation_to_silence)
        .fixItInsert(fixItLoc, ": " + type.getString());
    }

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
    
    // Sometimes a paren is just a paren. If the tuple pattern has a single
    // element, we can reduce it to a paren pattern.
    bool canDecayToParen = TP->getNumFields() == 1;
    auto decayToParen = [&]() -> bool {
      assert(canDecayToParen);
      Pattern *sub = TP->getFields()[0].getPattern();
      if (this->coercePatternToType(sub, dc, type, subOptions, resolver))
        return true;
      
      if (TP->getLParenLoc().isValid()) {
        P = new (Context) ParenPattern(TP->getLParenLoc(), sub,
                                       TP->getRParenLoc(),
                                       /*implicit*/ TP->isImplicit());
        P->setType(sub->getType());
      } else {
        P = sub;
      }
      return false;
    };

    // The context type must be a tuple.
    TupleType *tupleTy = type->getAs<TupleType>();
    if (!tupleTy && !hadError) {
      if (canDecayToParen)
        return decayToParen();
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_in_non_tuple_context,
               type);
      hadError = true;
    }

    // The number of elements must match exactly.
    // TODO: incomplete tuple patterns, with some syntax.
    if (!hadError && tupleTy->getFields().size() != TP->getNumFields()) {
      if (canDecayToParen)
        return decayToParen();
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

      TypeResolutionOptions subOptions = options - TR_Variadic;
      if (isVararg)
        subOptions |= TR_Variadic;
      hadError |= coercePatternToType(pattern, dc, CoercionType, subOptions, 
                                      resolver);
      if (!hadError)
        elt.setPattern(pattern);

      // Type-check the initialization expression.
      if (ExprHandle *initHandle = elt.getInit()) {
        Expr *init = initHandle->getExpr();
        if (initHandle->alreadyChecked()) {
          // Nothing to do
        } else if (typeCheckExpression(init, dc, CoercionType, Type(),
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
  case PatternKind::Is: {
    auto IP = cast<IsPattern>(P);

    // Type-check the type parameter.
    if (validateType(IP->getCastTypeLoc(), dc, TR_InExpression))
      return false;

    auto castType = IP->getCastTypeLoc().getType();

    // Determine whether we have an imbalance in the number of optionals.
    SmallVector<Type, 2> inputTypeOptionals;
    type->lookThroughAllAnyOptionalTypes(inputTypeOptionals);
    SmallVector<Type, 2> castTypeOptionals;
    castType->lookThroughAllAnyOptionalTypes(castTypeOptionals);

    // If we have extra optionals on the input type. Create ".Some" patterns
    // wrapping the isa pattern to balance out the optionals.
    int numExtraOptionals = inputTypeOptionals.size()-castTypeOptionals.size();
    if (numExtraOptionals > 0) {
      Pattern *sub = IP;
      for (int i = 0; i < numExtraOptionals; ++i) {
        sub = new (Context) EnumElementPattern(TypeLoc(),
                                               IP->getStartLoc(),
                                               IP->getEndLoc(),
                                               Context.Id_Some,
                                               nullptr, sub,
                                               /*Implicit=*/true);
      }

      P = sub;
      return coercePatternToType(P, dc, type, options);
    }


    CheckedCastKind castKind
      = typeCheckCheckedCast(type, IP->getCastTypeLoc().getType(), dc,
                             IP->getLoc(),
                             IP->getLoc(),IP->getCastTypeLoc().getSourceRange(),
                             [](Type) { return false; },
                             /*suppressDiagnostics=*/ false);
    switch (castKind) {
    case CheckedCastKind::Unresolved:
      return false;
    case CheckedCastKind::Coercion:
      diagnose(IP->getLoc(), diag::isa_is_always_true,
               type,
               IP->getCastTypeLoc().getType());
      IP->setCastKind(castKind);
      break;

    // Valid checks.
    case CheckedCastKind::ArrayDowncast:
    case CheckedCastKind::DictionaryDowncast:
    case CheckedCastKind::DictionaryDowncastBridged:
    case CheckedCastKind::SetDowncast:
    case CheckedCastKind::SetDowncastBridged:
      return coercePatternViaConditionalDowncast(
               *this, P, dc, type,
               subOptions|TR_FromNonInferredPattern);

    case CheckedCastKind::ValueCast:
    case CheckedCastKind::BridgeFromObjectiveC:
      IP->setCastKind(castKind);
      break;
    }
    IP->setType(type);
    
    // Coerce the subpattern to the destination type.
    if (Pattern *sub = IP->getSubPattern()) {
      if (coercePatternToType(sub, dc, IP->getCastTypeLoc().getType(),
                              subOptions|TR_FromNonInferredPattern))
        return true;
      IP->setSubPattern(sub);
    }
    
    return false;
  }
      
  case PatternKind::EnumElement: {
    auto *EEP = cast<EnumElementPattern>(P);
    
    // If the element decl was not resolved (because it was spelled without a
    // type as `.Foo`), resolve it now that we have a type.
    Optional<CheckedCastKind> castKind;
    
    Type enumTy;
    if (!EEP->getElementDecl()) {
      EnumElementDecl *element
        = lookupEnumMemberElement(*this, dc, type, EEP->getName());
      if (!element) {
        diagnose(EEP->getLoc(), diag::enum_element_pattern_member_not_found,
                 EEP->getName().str(), type);
        return true;
      }
      EEP->setElementDecl(element);
      enumTy = type;
    } else {
      // Check if the explicitly-written enum type matches the type we're
      // coercing to.
      assert(!EEP->getParentType().isNull()
             && "enum with resolved element doesn't specify parent type?!");
      auto parentTy = EEP->getParentType().getType();
      // If the type matches exactly, use it.
      if (parentTy->isEqual(type)) {
        enumTy = type;
      }
      // Otherwise, if the type is an unbound generic of the context type, use
      // the context type to resolve the parameters.
      else if (parentTy->is<UnboundGenericType>()) {
        if (parentTy->getAnyNominal() == type->getAnyNominal()) {
          enumTy = type;
        } else {
          diagnose(EEP->getLoc(), diag::ambiguous_enum_pattern_type,
                   parentTy, type);
          return true;
        }
      }
      // Otherwise, see if we can introduce a cast pattern to get from an
      // existential pattern type to the enum type.
      else if (type->isAnyExistentialType()) {
        auto foundCastKind = typeCheckCheckedCast(type, parentTy, dc,
                                                  SourceLoc(),
                                                  SourceRange(), SourceRange(),
                                                  [](Type) { return false; },
                                                  /*suppress diags*/ false);
        // If the cast failed, we can't resolve the pattern.
        if (foundCastKind < CheckedCastKind::First_Resolved)
          return true;
        
        // Otherwise, we can type-check as the enum type, and insert a cast
        // from the outer pattern type.
        castKind = foundCastKind;
        enumTy = parentTy;
      } else {
        diagnose(EEP->getLoc(),
                 diag::enum_element_pattern_not_member_of_enum,
                 EEP->getName().str(), type);
        return true;
      }
    }

    EnumElementDecl *elt = EEP->getElementDecl();
    
    // If there is a subpattern, push the enum element type down onto it.
    if (EEP->hasSubPattern()) {
      Type elementType;
      if (elt->hasArgumentType())
        elementType = enumTy->getTypeOfMember(elt->getModuleContext(),
                                              elt, this,
                                              elt->getArgumentInterfaceType());
      else
        elementType = TupleType::getEmpty(Context);
      Pattern *sub = EEP->getSubPattern();
      if (coercePatternToType(sub, dc, elementType,
                     subOptions|TR_FromNonInferredPattern|TR_EnumPatternPayload,
                     resolver))
        return true;
      EEP->setSubPattern(sub);
    }
    EEP->setType(enumTy);
    
    // Ensure that the type of our TypeLoc is fully resolved. If an unbound
    // generic type was spelled in the source (e.g. `case Optional.None:`) this
    // will fill in the generic parameters.
    EEP->getParentType().setType(enumTy, /*validated*/ true);
    
    // If we needed a cast, wrap the pattern in a cast pattern.
    if (castKind) {
      auto isPattern = new (Context) IsPattern(SourceLoc(),
                                               TypeLoc::withoutLoc(enumTy),
                                               EEP, *castKind,
                                               /*implicit*/true);
      isPattern->setType(type);
      P = isPattern;
    }
    
    return false;
  }

  case PatternKind::OptionalSome: {
    auto *OP = cast<OptionalSomePattern>(P);
    auto *enumDecl = type->getEnumOrBoundGenericEnum();
    if (!enumDecl) {
      diagnose(OP->getLoc(), diag::optional_element_pattern_not_valid_type,
               type);
      return true;
    }

    // If the element decl was not resolved (because it was spelled without a
    // type as `.Foo`), resolve it now that we have a type.
    if (!OP->getElementDecl()) {
      auto *element = lookupEnumMemberElement(*this, dc, type,
                                              Context.getIdentifier("Some"));
      if (!element) {
        diagnose(OP->getLoc(), diag::enum_element_pattern_member_not_found,
                 "Some", type);
        return true;
      }
      OP->setElementDecl(element);
    }

    EnumElementDecl *elt = OP->getElementDecl();
    // Is the enum element actually part of the enum type we're matching?
    if (elt->getParentEnum() != enumDecl) {
      diagnose(OP->getLoc(), diag::enum_element_pattern_not_member_of_enum,
               "Some", type);
      return true;
    }

    // Check the subpattern & push the enum element type down onto it.
    Type elementType;
    if (elt->hasArgumentType())
      elementType = type->getTypeOfMember(elt->getModuleContext(),
                                          elt, this,
                                          elt->getArgumentInterfaceType());
    else
      elementType = TupleType::getEmpty(Context);
    Pattern *sub = OP->getSubPattern();
    if (coercePatternToType(sub, dc, elementType,
                            subOptions|TR_FromNonInferredPattern|TR_EnumPatternPayload,
                            resolver))
      return true;
    OP->setSubPattern(sub);
    OP->setType(type);
    return false;
  }

      
  case PatternKind::NominalType: {
    auto NP = cast<NominalTypePattern>(P);
    
    // Type-check the type.
    if (validateType(NP->getCastTypeLoc(), dc, TR_InExpression))
      return false;
    
    Type patTy = NP->getCastTypeLoc().getType();

    // Check that the type is a nominal type.
    NominalTypeDecl *nomTy = patTy->getAnyNominal();
    if (!nomTy) {
      diagnose(NP->getLoc(), diag::nominal_type_pattern_not_nominal_type,
               patTy);
      return false;
    }
    
    // Check that the type matches the pattern type.
    // FIXME: We could insert an IsPattern if a checked cast can do the
    // conversion.
    
    // If a generic type name was given without arguments, allow a match to
    if (patTy->is<UnboundGenericType>()) {
      if (type->getNominalOrBoundGenericNominal() != nomTy) {
        diagnose(NP->getLoc(), diag::nominal_type_pattern_type_mismatch,
                 patTy, type);
        return false;
      }
    } else if (!patTy->isEqual(type)) {
      diagnose(NP->getLoc(), diag::nominal_type_pattern_type_mismatch,
               patTy, type);
      return false;
    }
    
    // Coerce each subpattern to its corresponding property's type, or raise an
    // error if the property doesn't exist.
    for (auto &elt : NP->getMutableElements()) {
      // Resolve the property reference.
      if (!elt.getProperty()) {
        // For recovery, skip elements that didn't have a name attached.
        if (elt.getPropertyName().empty())
          continue;
        VarDecl *prop = nullptr;
        SmallVector<ValueDecl *, 4> members;
        unsigned lookupOptions =
            NL_QualifiedDefault | NL_KnownNonCascadingDependency;
        if (!dc->lookupQualified(type, elt.getPropertyName(),
                                 lookupOptions, this, members)) {
          diagnose(elt.getSubPattern()->getLoc(),
                   diag::nominal_type_pattern_property_not_found,
                   elt.getPropertyName().str(), patTy);
          return true;
        }
        
        for (auto member : members) {
          auto vd = dyn_cast<VarDecl>(member);
          if (!vd) continue;
          // FIXME: can this happen?
          if (prop) {
            diagnose(elt.getSubPattern()->getLoc(),
                     diag::nominal_type_pattern_property_ambiguous,
                     elt.getPropertyName().str(), patTy);
            return true;
          }
          prop = vd;
        }
        
        if (!prop) {
          diagnose(elt.getSubPattern()->getLoc(),
                   diag::nominal_type_pattern_not_property,
                   elt.getPropertyName().str(), patTy);
          return true;
        }
        
        if (prop->isStatic()) {
          diagnose(elt.getSubPattern()->getLoc(),
                   diag::nominal_type_pattern_static_property,
                   elt.getPropertyName().str(), patTy);
        }
        
        elt.setProperty(prop);
      }
      
      // Coerce the subpattern.
      auto sub = elt.getSubPattern();
      Type propTy = type->getTypeOfMember(dc->getParentModule(),
                                          elt.getProperty(),
                                          this);
      if (coercePatternToType(sub, dc, propTy,
                              subOptions|TR_FromNonInferredPattern, resolver))
        return true;
      elt.setSubPattern(sub);
    }
    NP->setType(type);
    return false;
  }
  }
  llvm_unreachable("bad pattern kind!");
}
