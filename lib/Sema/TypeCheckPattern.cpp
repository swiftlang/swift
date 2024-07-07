//===--- TypeCheckPattern.cpp - Type Checking for Patterns ----------------===//
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
//
// This file implements semantic analysis for patterns, analyzing a
// pattern tree in both bottom-up and top-down ways.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckType.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/StringExtras.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/TypeCheckRequests.h"
#include "llvm/Support/SaveAndRestore.h"
#include <utility>
using namespace swift;

EnumElementDecl *
TypeChecker::tryExtractClangEnumElement(DeclContext *DC, SourceLoc UseLoc,
                                        const VarDecl *constant) {
  // Note: This requires the getter's body to have a certain syntactic form.
  // It should be kept in sync with importEnumCaseAlias in the ClangImporter
  // library.
  if (!constant->hasClangNode())
    return nullptr;

  ExportContext where = ExportContext::forFunctionBody(DC, UseLoc);
  diagnoseExplicitUnavailability(constant, UseLoc, where, nullptr);

  const FuncDecl *getter = constant->getAccessor(AccessorKind::Get);
  if (!getter)
    return nullptr;

  const BraceStmt *body = getter->getBody();
  if (!body || body->getNumElements() != 1)
    return nullptr;

  auto *retStmtRaw = body->getFirstElement().dyn_cast<Stmt *>();
  auto *retStmt = dyn_cast_or_null<ReturnStmt>(retStmtRaw);
  if (!retStmt)
    return nullptr;

  auto *resultExpr = dyn_cast_or_null<ApplyExpr>(retStmt->getResult());
  if (!resultExpr)
    return nullptr;

  auto *ctorExpr = dyn_cast<DeclRefExpr>(resultExpr->getFn());
  if (!ctorExpr)
    return nullptr;

  // If the declaration we found isn't in the same nominal type as the
  // constant, ignore it.
  if (ctorExpr->getDecl()->getDeclContext()->getSelfNominalTypeDecl() !=
        constant->getDeclContext()->getSelfNominalTypeDecl())
    return nullptr;

  return dyn_cast<EnumElementDecl>(ctorExpr->getDecl());
}

/// Find the first enum element in \p foundElements.
///
/// If there are no enum elements but there are properties, attempts to map
/// an arbitrary property to an enum element using extractEnumElement.
static EnumElementDecl *
filterForEnumElement(DeclContext *DC, SourceLoc UseLoc,
                     bool unqualifiedLookup, LookupResult foundElements) {
  EnumElementDecl *foundElement = nullptr;
  VarDecl *foundConstant = nullptr;

  for (const LookupResultEntry &result : foundElements) {
    ValueDecl *e = result.getValueDecl();
    assert(e);

    // Skip if the enum element was referenced as an instance member
    if (unqualifiedLookup) {
      if (!result.getBaseDecl() ||
          !result.getBaseDecl()->getInterfaceType()->is<MetatypeType>()) {
        continue;
      }
    }

    if (auto *oe = dyn_cast<EnumElementDecl>(e)) {
      // Note that there could be multiple elements with the same
      // name, such results in a re-declaration error, so let's
      // just always pick the last element, just like in `foundConstant`
      // case.
      foundElement = oe;
      continue;
    }

    if (auto *var = dyn_cast<VarDecl>(e)) {
      foundConstant = var;
      continue;
    }
  }

  if (!foundElement && foundConstant)
    return TypeChecker::tryExtractClangEnumElement(DC, UseLoc, foundConstant);

  return foundElement;
}

/// Find an unqualified enum element.
static EnumElementDecl *
lookupUnqualifiedEnumMemberElement(DeclContext *DC, DeclNameRef name,
                                   SourceLoc UseLoc) {
  // FIXME: We should probably pay attention to argument labels someday.
  name = name.withoutArgumentLabels();

  auto lookup =
      TypeChecker::lookupUnqualified(DC, name, UseLoc,
                                     defaultUnqualifiedLookupOptions);
  return filterForEnumElement(DC, UseLoc,
                              /*unqualifiedLookup=*/true, lookup);
}

static LookupResult lookupMembers(DeclContext *DC, Type ty, DeclNameRef name,
                                  SourceLoc UseLoc) {
  if (!ty->mayHaveMembers())
    return LookupResult();

  // FIXME: We should probably pay attention to argument labels someday.
  name = name.withoutArgumentLabels();

  // Look up the case inside the enum.
  // FIXME: We should be able to tell if this is a private lookup.
  NameLookupOptions lookupOptions = defaultMemberLookupOptions;
  return TypeChecker::lookupMember(DC, ty, name, UseLoc, lookupOptions);
}

/// Find an enum element in an enum type.
static EnumElementDecl *lookupEnumMemberElement(DeclContext *DC, Type ty,
                                                DeclNameRef name,
                                                SourceLoc UseLoc) {
  LookupResult foundElements = lookupMembers(DC, ty, name, UseLoc);
  return filterForEnumElement(DC, UseLoc,
                              /*unqualifiedLookup=*/false, foundElements);
}

static DeclRefTypeRepr *translateExprToDeclRefTypeRepr(Expr *E, ASTContext &C) {
  // FIXME: Support QualifiedIdentTypeRepr nodes with non-DeclRefTypeRepr bases.
  /// Translates an expression to a \c DeclRefTypeRepr.
  class ExprToDeclRefTypeRepr
      : public ExprVisitor<ExprToDeclRefTypeRepr, DeclRefTypeRepr *> {
    ASTContext &C;

  public:
    ExprToDeclRefTypeRepr(ASTContext &C) : C(C) {}

    DeclRefTypeRepr *visitExpr(Expr *e) { return nullptr; }

    DeclRefTypeRepr *visitTypeExpr(TypeExpr *te) {
      return dyn_cast_or_null<UnqualifiedIdentTypeRepr>(te->getTypeRepr());
    }

    DeclRefTypeRepr *visitDeclRefExpr(DeclRefExpr *dre) {
      // Get the declared type.
      auto *td = dyn_cast<TypeDecl>(dre->getDecl());
      if (!td) {
        return nullptr;
      }

      auto *repr = UnqualifiedIdentTypeRepr::create(C, dre->getNameLoc(),
                                                    td->createNameRef());
      repr->setValue(td, nullptr);

      return repr;
    }

    DeclRefTypeRepr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *udre) {
      return UnqualifiedIdentTypeRepr::create(C, udre->getNameLoc(),
                                              udre->getName());
    }

    DeclRefTypeRepr *visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
      auto *base = visit(ude->getBase());
      if (!base) {
        return nullptr;
      }

      return QualifiedIdentTypeRepr::create(C, base, ude->getNameLoc(),
                                            ude->getName());
    }

    DeclRefTypeRepr *
    visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *use) {
      auto *base = visit(use->getSubExpr());
      if (!base) {
        return nullptr;
      }

      assert(!base->hasGenericArgList() && "Already has generic arguments");

      return DeclRefTypeRepr::create(
          C, base->getBase(), base->getNameLoc(), base->getNameRef(),
          use->getUnresolvedParams(),
          SourceRange(use->getLAngleLoc(), use->getRAngleLoc()));
    }
  } translator(C);

  return translator.visit(E);
}

namespace {
class ResolvePattern : public ASTVisitor<ResolvePattern,
                                         /*ExprRetTy=*/Pattern*,
                                         /*StmtRetTy=*/void,
                                         /*DeclRetTy=*/void,
                                         /*PatternRetTy=*/Pattern*>
{
public:
  ASTContext &Context;
  DeclContext *DC;

  ResolvePattern(DeclContext *DC) : Context(DC->getASTContext()), DC(DC) {}

  // Convert a subexpression to a pattern if possible, or wrap it in an
  // ExprPattern.
  Pattern *getSubExprPattern(Expr *E) {
    if (Pattern *p = visit(E))
      return p;

    return ExprPattern::createResolved(Context, E, DC);
  }

  /// Turn an argument list into a matching tuple or paren pattern.
  Pattern *composeTupleOrParenPattern(ArgumentList *args) {
    assert(!args->hasAnyInOutArgs());
    if (auto *unary = args->getUnlabeledUnaryExpr()) {
      auto *subPattern = getSubExprPattern(unary);
      return new (Context)
          ParenPattern(args->getLParenLoc(), subPattern, args->getRParenLoc());
    }
    SmallVector<TuplePatternElt, 4> elts;
    for (auto arg : *args) {
      auto *subPattern = getSubExprPattern(arg.getExpr());
      elts.emplace_back(arg.getLabel(), arg.getLabelLoc(), subPattern);
    }
    return TuplePattern::create(Context, args->getLParenLoc(), elts,
                                args->getRParenLoc());
  }

  // Handle productions that are always leaf patterns or are already resolved.
#define ALWAYS_RESOLVED_PATTERN(Id) \
  Pattern *visit##Id##Pattern(Id##Pattern *P) { return P; }
  ALWAYS_RESOLVED_PATTERN(Named)
  ALWAYS_RESOLVED_PATTERN(Any)
  ALWAYS_RESOLVED_PATTERN(Is)
  ALWAYS_RESOLVED_PATTERN(Paren)
  ALWAYS_RESOLVED_PATTERN(Tuple)
  ALWAYS_RESOLVED_PATTERN(EnumElement)
  ALWAYS_RESOLVED_PATTERN(Bool)
#undef ALWAYS_RESOLVED_PATTERN

  Pattern *visitBindingPattern(BindingPattern *P) {
    // Keep track of the fact that we're inside of a var/let pattern.  This
    // affects how unqualified identifiers are processed.
    P->setSubPattern(visit(P->getSubPattern()));
    
    // If the var pattern has no variables bound underneath it, then emit a
    // warning that the var/let is pointless.
    if (!P->isImplicit()) {
      bool HasVariable = false;
      P->forEachVariable([&](VarDecl *VD) { HasVariable = true; });
      
      if (!HasVariable) {
        Context.Diags
            .diagnose(P->getLoc(), diag::var_pattern_didnt_bind_variables,
                      P->getIntroducerStringRef())
            .highlight(P->getSubPattern()->getSourceRange())
            .fixItRemove(P->getLoc());
      }
    }
    
    return P;
  }

  Pattern *visitOptionalSomePattern(OptionalSomePattern *P) {
    P->setSubPattern(visit(P->getSubPattern()));
    return P;
  }

  Pattern *visitTypedPattern(TypedPattern *P) {
    P->setSubPattern(visit(P->getSubPattern()));
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
    if (E->isImplicit()) {
      return AnyPattern::createImplicit(Context);
    }
    return new (Context) AnyPattern(E->getLoc());
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
    if (cast->isImplicit()) {
      return IsPattern::createImplicit(Context, cast->getCastType(), subPattern,
                                       CheckedCastKind::Unresolved);
    }
    auto *TE = new (Context) TypeExpr(cast->getCastTypeRepr());
    if (auto castTy = cast->getType())
      TE->setType(MetatypeType::get(castTy));
    return new (Context)
        IsPattern(cast->getLoc(), TE, subPattern, CheckedCastKind::Unresolved);
  }
  
  // Convert a paren expr to a pattern if it contains a pattern.
  Pattern *visitParenExpr(ParenExpr *E) {
    Pattern *subPattern = getSubExprPattern(E->getSubExpr());
    return new (Context)
        ParenPattern(E->getLParenLoc(), subPattern, E->getRParenLoc());
  }
  
  // Convert all tuples to patterns.
  Pattern *visitTupleExpr(TupleExpr *E) {
    // Construct a TuplePattern.
    SmallVector<TuplePatternElt, 4> patternElts;

    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      Pattern *pattern = getSubExprPattern(E->getElement(i));
      patternElts.push_back(TuplePatternElt(E->getElementName(i),
                                            E->getElementNameLoc(i),
                                            pattern));
    }

    return TuplePattern::create(Context, E->getLoc(), patternElts,
                                E->getRParenLoc());
  }

  Pattern *convertBindingsToOptionalSome(Expr *E) {
    auto *expr = E->getSemanticsProvidingExpr();
    auto *bindExpr = dyn_cast<BindOptionalExpr>(expr);
    if (!bindExpr) {
      // Let's see if this expression prefixed with any number of '?'
      // has any other disjoint 'BindOptionalExpr' inside of it, if so,
      // we need to wrap such sub-expression into `OptionalEvaluationExpr`.
      bool hasDisjointChaining = false;
      expr->forEachChildExpr([&](Expr *subExpr) -> Expr * {
        // If there is `OptionalEvaluationExpr` in the AST
        // it means that all of possible `BindOptionalExpr`
        // which follow are covered by it.
        if (isa<OptionalEvaluationExpr>(subExpr))
          return nullptr;

        if (isa<BindOptionalExpr>(subExpr)) {
          hasDisjointChaining = true;
          return nullptr;
        }

        return subExpr;
      });

      if (hasDisjointChaining)
        E = new (Context) OptionalEvaluationExpr(E);

      return getSubExprPattern(E);
    }

    auto *subExpr = convertBindingsToOptionalSome(bindExpr->getSubExpr());
    return OptionalSomePattern::create(Context, subExpr,
                                       bindExpr->getQuestionLoc());
  }

  // Convert a x? to OptionalSome pattern.  In the AST form, this will look like
  // an OptionalEvaluationExpr with an immediate BindOptionalExpr inside of it.
  Pattern *visitOptionalEvaluationExpr(OptionalEvaluationExpr *E) {
    auto *subExpr = E->getSubExpr();
    // We only handle the case where one or more bind expressions are subexprs
    // of the optional evaluation.  Other cases are not simple postfix ?'s.
    if (!isa<BindOptionalExpr>(subExpr->getSemanticsProvidingExpr()))
      return nullptr;

    return convertBindingsToOptionalSome(subExpr);
  }


  // Unresolved member syntax '.Element' forms an EnumElement pattern. The
  // element will be resolved when we type-check the pattern.
  Pattern *visitUnresolvedMemberExpr(UnresolvedMemberExpr *ume) {
    if (ume->getName().getBaseName().isSpecial())
      return nullptr;

    return EnumElementPattern::create(ume->getDotLoc(), ume->getNameLoc(),
                                      ume->getName(), ume,
                                      /*subPattern*/ nullptr, DC);
  }
  
  // Member syntax 'T.Element' forms a pattern if 'T' is an enum and the
  // member name is a member of the enum.
  Pattern *visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    DeclRefTypeRepr *repr =
        translateExprToDeclRefTypeRepr(ude->getBase(), Context);
    if (!repr) {
      return nullptr;
    }

    const auto options = TypeResolutionOptions(std::nullopt) |
                         TypeResolutionFlags::SilenceErrors;

    // See if the repr resolves to a type.
    const auto ty = TypeResolution::resolveContextualType(
        repr, DC, options,
        [](auto unboundTy) {
          // FIXME: Don't let unbound generic types escape type resolution.
          // For now, just return the unbound generic type.
          return unboundTy;
        },
        // FIXME: Don't let placeholder types escape type resolution.
        // For now, just return the placeholder type.
        PlaceholderType::get,
        /*packElementOpener*/ nullptr);

    auto *enumDecl = dyn_cast_or_null<EnumDecl>(ty->getAnyNominal());
    if (!enumDecl)
      return nullptr;

    EnumElementDecl *referencedElement
      = lookupEnumMemberElement(DC, ty, ude->getName(), ude->getLoc());
    if (!referencedElement)
      return nullptr;

    auto *base =
        TypeExpr::createForMemberDecl(repr, ude->getNameLoc(), enumDecl);
    base->setType(MetatypeType::get(ty));
    return EnumElementPattern::create(base, ude->getDotLoc(), ude->getNameLoc(),
                                      ude->getName(), referencedElement,
                                      /*subPattern*/ nullptr, DC);
  }
  
  // A DeclRef 'E' that refers to an enum element forms an EnumElementPattern.
  Pattern *visitDeclRefExpr(DeclRefExpr *de) {
    auto *elt = dyn_cast<EnumElementDecl>(de->getDecl());
    if (!elt)
      return nullptr;
    
    // Use the type of the enum from context.
    auto enumTy = elt->getParentEnum()->getDeclaredTypeInContext();
    auto *base = TypeExpr::createImplicit(enumTy, Context);

    return EnumElementPattern::create(base, SourceLoc(), de->getNameLoc(),
                                      elt->createNameRef(), elt,
                                      /*subPattern*/ nullptr, DC);
  }
  Pattern *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *ude) {
    // FIXME: This shouldn't be needed.  It is only necessary because of the
    // poor representation of clang enum aliases and should be removed when
    // rdar://20879992 is addressed.
    //
    // Try looking up an enum element in context.
    if (EnumElementDecl *referencedElement
        = lookupUnqualifiedEnumMemberElement(DC, ude->getName(),
                                             ude->getLoc())) {
      auto *enumDecl = referencedElement->getParentEnum();
      auto enumTy = enumDecl->getDeclaredTypeInContext();
      auto *base = TypeExpr::createImplicit(enumTy, Context);

      return EnumElementPattern::create(base, SourceLoc(), ude->getNameLoc(),
                                        ude->getName(), referencedElement,
                                        /*subPattern*/ nullptr, DC);
    }
      
    
    // Perform unqualified name lookup to find out what the UDRE is.
    return getSubExprPattern(TypeChecker::resolveDeclRefExpr(
        ude, DC, /*replaceInvalidRefsWithErrors=*/true));
  }

  // Call syntax forms a pattern if:
  // - the callee in 'Element(x...)' or '.Element(x...)'
  //   references an enum element. The arguments then form a tuple
  //   pattern matching the element's data.
  // - the callee in 'T(...)' is a struct or class type. The argument tuple is
  //   then required to have keywords for every argument that name properties
  //   of the type.
  Pattern *visitCallExpr(CallExpr *ce) {
    // Specialized call are not allowed anyway.
    // Let it be diagnosed as an expression.
    if (isa<UnresolvedSpecializeExpr>(ce->getFn()))
      return nullptr;

    if (isa<UnresolvedMemberExpr>(ce->getFn())) {
      auto *P = visit(ce->getFn());
      if (!P)
        return nullptr;

      auto *EEP = cast<EnumElementPattern>(P);
      EEP->setSubPattern(composeTupleOrParenPattern(ce->getArgs()));
      EEP->setUnresolvedOriginalExpr(ce);

      return P;
    }

    DeclRefTypeRepr *repr =
        translateExprToDeclRefTypeRepr(ce->getFn(), Context);
    if (!repr) {
      return nullptr;
    }

    EnumElementDecl *referencedElement = nullptr;
    TypeExpr *baseTE = nullptr;

    if (isa<UnqualifiedIdentTypeRepr>(repr)) {
      // Not qualified. Try looking up an enum element in context.
      referencedElement = lookupUnqualifiedEnumMemberElement(
          DC, repr->getNameRef(), repr->getLoc());
      if (!referencedElement)
        return nullptr;

      auto *enumDecl = referencedElement->getParentEnum();
      baseTE = TypeExpr::createImplicit(enumDecl->getDeclaredTypeInContext(),
                                        Context);
    } else {
      // Otherwise, see whether we had an enum type as the penultimate
      // component, and look up an element inside it.
      auto *qualIdentTR = cast<QualifiedIdentTypeRepr>(repr);

      const auto options = TypeResolutionOptions(std::nullopt) |
                           TypeResolutionFlags::SilenceErrors;

      // See first if the entire repr resolves to a type.
      const Type enumTy = TypeResolution::resolveContextualType(
          qualIdentTR->getBase(), DC, options,
          [](auto unboundTy) {
            // FIXME: Don't let unbound generic types escape type
            // resolution. For now, just return the unbound generic type.
            return unboundTy;
          },
          // FIXME: Don't let placeholder types escape type resolution.
          // For now, just return the placeholder type.
          PlaceholderType::get,
          /*packElementOpener*/ nullptr);

      auto *enumDecl = dyn_cast_or_null<EnumDecl>(enumTy->getAnyNominal());
      if (!enumDecl)
        return nullptr;

      referencedElement = lookupEnumMemberElement(
          DC, enumTy, qualIdentTR->getNameRef(), qualIdentTR->getLoc());
      if (!referencedElement)
        return nullptr;

      baseTE = TypeExpr::createForMemberDecl(
          qualIdentTR->getBase(), qualIdentTR->getNameLoc(), enumDecl);
      baseTE->setType(MetatypeType::get(enumTy));
    }

    assert(baseTE && baseTE->getType() && "Didn't initialize base expression?");
    assert(!repr->hasGenericArgList() && "should be handled above");

    auto *subPattern = composeTupleOrParenPattern(ce->getArgs());
    return EnumElementPattern::create(baseTE, SourceLoc(), repr->getNameLoc(),
                                      repr->getNameRef(), referencedElement,
                                      subPattern, DC);
  }
};

} // end anonymous namespace

Pattern *ResolvePatternRequest::evaluate(Evaluator &evaluator, Pattern *P,
                                         DeclContext *DC,
                                         bool isStmtCondition) const {
  P = ResolvePattern(DC).visit(P);

  TypeChecker::diagnoseDuplicateBoundVars(P);

  // If the entire pattern is "(pattern_expr (type_expr SomeType))", then this
  // is an invalid pattern.  If it were actually a value comparison (with ~=)
  // then the metatype would have had to be spelled with "SomeType.self".  What
  // they actually meant is to write "is SomeType", so we rewrite it to that
  // pattern for good QoI.
  auto &Context = DC->getASTContext();
  if (auto *EP = dyn_cast<ExprPattern>(P))
    if (auto *TE = dyn_cast<TypeExpr>(EP->getSubExpr())) {
      Context.Diags.diagnose(TE->getStartLoc(), diag::type_pattern_missing_is)
        .fixItInsert(TE->getStartLoc(), "is ");

      P = new (Context)
          IsPattern(TE->getStartLoc(), TE,
                    /*subpattern*/ nullptr, CheckedCastKind::Unresolved);
    }
  
  // Look through a TypedPattern if present.
  auto *InnerP = P;
  if (auto *TP = dyn_cast<TypedPattern>(P))
    InnerP = TP->getSubPattern();

  // If the pattern was valid, check for an implicit BindingPattern on the outer
  // level.  If so, we have an "if let" condition and we want to enforce some
  // more structure on it.
  if (isStmtCondition && isa<BindingPattern>(InnerP) && InnerP->isImplicit()) {
    auto *Body = cast<BindingPattern>(InnerP)->getSubPattern();

    // If they wrote a "x?" pattern, they probably meant "if let x".
    // Check for this and recover nicely if they wrote that.
    if (auto *OSP = dyn_cast<OptionalSomePattern>(Body)) {
      if (!OSP->getSubPattern()->isRefutablePattern()) {
        Context.Diags.diagnose(OSP->getStartLoc(),
                               diag::iflet_implicitly_unwraps)
          .highlight(OSP->getSourceRange())
          .fixItRemove(OSP->getQuestionLoc());
        return P;
      }
    }

    // If the pattern bound is some other refutable pattern, then they
    // probably meant:
    //   if case let <pattern> =
    if (Body->isRefutablePattern()) {
      Context.Diags.diagnose(P->getLoc(), diag::iflet_pattern_matching)
        .fixItInsert(P->getLoc(), "case ");
      return P;
    }

    // "if let" implicitly looks inside of an optional, so wrap it in an
    // OptionalSome pattern.
    P = OptionalSomePattern::createImplicit(Context, P, P->getEndLoc());
  }

  return P;
}

Pattern *TypeChecker::resolvePattern(Pattern *P, DeclContext *DC,
                                     bool isStmtCondition) {
  auto &eval = DC->getASTContext().evaluator;
  return evaluateOrDefault(eval, ResolvePatternRequest{P, DC, isStmtCondition},
                           nullptr);
}

static Type
validateTypedPattern(TypedPattern *TP, DeclContext *dc,
                     TypeResolutionOptions options,
                     OpenUnboundGenericTypeFn unboundTyOpener,
                     HandlePlaceholderTypeReprFn placeholderHandler,
                     OpenPackElementFn packElementOpener) {
  if (TP->hasType()) {
    return TP->getType();
  }

  // If the pattern declares an opaque type, and applies to a single
  // variable binding, then we can bind the opaque return type from the
  // property definition.
  auto &Context = dc->getASTContext();
  auto *Repr = TP->getTypeRepr();
  if (Repr && (Repr->hasOpaque() ||
               (Context.LangOpts.hasFeature(Feature::ImplicitSome) &&
                !collectOpaqueTypeReprs(Repr, Context, dc).empty()))) {
    auto named = dyn_cast<NamedPattern>(
        TP->getSubPattern()->getSemanticsProvidingPattern());
    if (!named) {
      Context.Diags.diagnose(TP->getLoc(),
                             diag::opaque_type_unsupported_pattern);
      return ErrorType::get(Context);
    }

    auto *var = named->getDecl();
    auto opaqueDecl = var->getOpaqueResultTypeDecl();
    if (!opaqueDecl) {
      return ErrorType::get(Context);
    }

    auto opaqueTy = opaqueDecl->getDeclaredInterfaceType();
    if (opaqueTy->hasError()) {
      return ErrorType::get(Context);
    }

    return named->getDecl()->getDeclContext()->mapTypeIntoContext(opaqueTy);
  }

  const auto ty = TypeResolution::resolveContextualType(
      Repr, dc, options, unboundTyOpener, placeholderHandler,
      packElementOpener);

  if (ty->hasError()) {
    return ErrorType::get(Context);
  }

  assert(!dyn_cast_or_null<SpecifierTypeRepr>(Repr) &&
         "Didn't resolve invalid type to error type!");
  return ty;
}

Type TypeChecker::typeCheckPattern(ContextualPattern pattern) {
  DeclContext *dc = pattern.getDeclContext();
  ASTContext &ctx = dc->getASTContext();
  if (auto type = evaluateOrDefault(ctx.evaluator, PatternTypeRequest{pattern},
                                    Type())) {
    ASSERT(!type->hasTypeParameter() &&
           "pattern should have a contextual type");
    return type;
  }

  return ErrorType::get(ctx);
}

/// Apply the contextual pattern's context to the type resolution options.
static TypeResolutionOptions applyContextualPatternOptions(
    TypeResolutionOptions options, ContextualPattern pattern) {
  if (pattern.allowsInference()) {
    options |= TypeResolutionFlags::AllowUnspecifiedTypes;
  }

  return options;
}

ExprPatternMatchResult
ExprPatternMatchRequest::evaluate(Evaluator &evaluator,
                                  const ExprPattern *EP) const {
  assert(EP->isResolved() && "Must only be queried once resolved");

  auto *DC = EP->getDeclContext();
  auto &ctx = DC->getASTContext();

  // Create a 'let' binding to stand in for the RHS value.
  auto *matchVar =
      new (ctx) VarDecl(/*IsStatic*/ false, VarDecl::Introducer::Let,
                        EP->getLoc(), ctx.Id_PatternMatchVar, DC);
  matchVar->setImplicit();

  // Build the 'expr ~= var' expression.
  auto *matchOp = new (ctx) UnresolvedDeclRefExpr(
      DeclNameRef(ctx.Id_MatchOperator), DeclRefKind::BinaryOperator,
      DeclNameLoc(EP->getLoc()));
  matchOp->setImplicit();

  // Note we use getEndLoc here to have the BinaryExpr source range be the same
  // as the expr pattern source range.
  auto *matchVarRef =
      new (ctx) DeclRefExpr(matchVar, DeclNameLoc(EP->getEndLoc()),
                            /*Implicit=*/true);
  auto *matchCall = BinaryExpr::create(ctx, EP->getSubExpr(), matchOp,
                                       matchVarRef, /*implicit*/ true);
  return {matchVar, matchCall};
}

ExprPattern *
EnumElementExprPatternRequest::evaluate(Evaluator &evaluator,
                                        const EnumElementPattern *EEP) const {
  assert(EEP->hasUnresolvedOriginalExpr());
  auto *DC = EEP->getDeclContext();
  return ExprPattern::createResolved(DC->getASTContext(),
                                     EEP->getUnresolvedOriginalExpr(), DC);
}

Type PatternTypeRequest::evaluate(Evaluator &evaluator,
                                  ContextualPattern pattern) const {
  Pattern *P = pattern.getPattern();
  DeclContext *dc = pattern.getDeclContext();

  TypeResolutionOptions options(pattern.getPatternBindingDecl()
                                  ? TypeResolverContext::PatternBindingDecl
                                  : TypeResolverContext::InExpression);
  options = applyContextualPatternOptions(options, pattern);
  if (!pattern.isTopLevel()) {
    options = options.withoutContext();
  }

  auto &Context = dc->getASTContext();
  switch (P->getKind()) {
  // Type-check paren patterns by checking the sub-pattern and
  // propagating that type out.
  case PatternKind::Paren:
  case PatternKind::Binding: {
    Pattern *SP;
    if (auto *PP = dyn_cast<ParenPattern>(P))
      SP = PP->getSubPattern();
    else
      SP = cast<BindingPattern>(P)->getSubPattern();
    Type subType = TypeChecker::typeCheckPattern(
        pattern.forSubPattern(SP, /*retainTopLevel=*/true));
    if (subType->hasError())
      return ErrorType::get(Context);

    auto type = subType;
    if (P->getKind() == PatternKind::Paren)
      type = ParenType::get(Context, type);
    return type;
  }

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    OpenUnboundGenericTypeFn unboundTyOpener = nullptr;
    HandlePlaceholderTypeReprFn placeholderHandler = nullptr;
    OpenPackElementFn packElementOpener = nullptr;
    if (pattern.allowsInference()) {
      unboundTyOpener = [](auto unboundTy) {
        // FIXME: Don't let unbound generic types escape type resolution.
        // For now, just return the unbound generic type.
        return unboundTy;
      };
      // FIXME: Don't let placeholder types escape type resolution.
      // For now, just return the placeholder type.
      placeholderHandler = PlaceholderType::get;
    }
    return validateTypedPattern(cast<TypedPattern>(P), dc, options,
                                unboundTyOpener, placeholderHandler,
                                packElementOpener);
  }

  // A wildcard or name pattern cannot appear by itself in a context
  // which requires an explicit type.
  case PatternKind::Any:
  case PatternKind::Named:
    // If we're type checking this pattern in a context that can provide type
    // information, then the lack of type information is not an error.
    if (options & TypeResolutionFlags::AllowUnspecifiedTypes)
      return Context.TheUnresolvedType;

    Context.Diags.diagnose(P->getLoc(), diag::cannot_infer_type_for_pattern);
    if (auto named = dyn_cast<NamedPattern>(P)) {
      if (auto var = named->getDecl()) {
        var->setInvalid();
      }
    }
    return ErrorType::get(Context);

  // A tuple pattern propagates its tuple-ness out.
  case PatternKind::Tuple: {
    auto tuplePat = cast<TuplePattern>(P);
    bool hadError = false;
    SmallVector<TupleTypeElt, 8> typeElts;

    for (unsigned i = 0, e = tuplePat->getNumElements(); i != e; ++i) {
      TuplePatternElt &elt = tuplePat->getElement(i);
      Type subType = TypeChecker::typeCheckPattern(
          pattern.forSubPattern(elt.getPattern(), /*retainTopLevel=*/false));
      if (subType->hasError())
        hadError = true;

      typeElts.push_back(TupleTypeElt(subType, elt.getLabel()));
    }

    if (hadError) {
      return ErrorType::get(Context);
    }

    return TupleType::get(typeElts, Context);
  }
      
  //--- Refutable patterns.
  //
  // Refutable patterns occur when checking the PatternBindingDecls in if/let,
  // while/let, and let/else conditions.
  case PatternKind::OptionalSome: {
    // Annotated if-let patterns are rewritten by TypeChecker::resolvePattern
    // to have an enclosing implicit (...)? pattern. If we can resolve the inner
    // typed pattern, the resulting pattern must have optional type.
    auto somePat = cast<OptionalSomePattern>(P);
    if (somePat->isImplicit() && isa<TypedPattern>(somePat->getSubPattern())) {
      OpenUnboundGenericTypeFn unboundTyOpener = nullptr;
      HandlePlaceholderTypeReprFn placeholderHandler = nullptr;
      OpenPackElementFn packElementOpener = nullptr;
      if (pattern.allowsInference()) {
        unboundTyOpener = [](auto unboundTy) {
          // FIXME: Don't let unbound generic types escape type resolution.
          // For now, just return the unbound generic type.
          return unboundTy;
        };
        // FIXME: Don't let placeholder types escape type resolution.
        // For now, just return the placeholder type.
        placeholderHandler = PlaceholderType::get;
      }

      const auto type =
          validateTypedPattern(cast<TypedPattern>(somePat->getSubPattern()), dc,
                               options, unboundTyOpener, placeholderHandler,
                               packElementOpener);

      if (!type->hasError()) {
        return OptionalType::get(type);
      }
    }
    LLVM_FALLTHROUGH;
  }

  case PatternKind::Is:
  case PatternKind::EnumElement:
  case PatternKind::Bool:
  case PatternKind::Expr:
    // In a let/else, these always require an initial value to match against.
    if (!(options & TypeResolutionFlags::AllowUnspecifiedTypes)) {
      Context.Diags.diagnose(P->getLoc(),
                             diag::refutable_pattern_requires_initializer);
      return ErrorType::get(Context);
    }

    return Context.TheUnresolvedType;
  }
  llvm_unreachable("bad pattern kind!");
}

/// Coerce the specified parameter list of a ClosureExpr to the specified
/// contextual type.
void TypeChecker::coerceParameterListToType(ParameterList *P,
                                            AnyFunctionType *FN) {

  // Local function to check if the given type is valid e.g. doesn't have
  // errors, type variables or unresolved types related to it.
  auto isValidType = [](Type type) -> bool {
    return !(type->hasError() || type->hasUnresolvedType());
  };

  // Local function to check whether type of given parameter
  // should be coerced to a given contextual type or not.
  auto shouldOverwriteParam = [&](ParamDecl *param) -> bool {
    return !isValidType(param->getTypeInContext());
  };

  auto handleParameter = [&](ParamDecl *param, Type ty, bool forceMutable) {
    if (forceMutable)
      param->setSpecifier(ParamDecl::Specifier::InOut);

    // If contextual type is invalid and we have a valid argument type
    // trying to coerce argument to contextual type would mean erasing
    // valuable diagnostic information.
    if (isValidType(ty) || shouldOverwriteParam(param)) {
      param->setInterfaceType(ty->mapTypeOutOfContext());
    }
  };

  // Coerce each parameter to the respective type.
  ArrayRef<AnyFunctionType::Param> params = FN->getParams();
  for (unsigned i = 0, e = P->size(); i != e; ++i) {
    auto &param = P->get(i);
    assert(param->getArgumentName().empty() &&
           "Closures cannot have API names");
    
    handleParameter(param,
                    params[i].getParameterType(),
                    params[i].isInOut());
    assert(!param->isDefaultArgument() && "Closures cannot have default args");
  }
}
