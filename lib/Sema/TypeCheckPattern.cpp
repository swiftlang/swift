//===--- TypeCheckPattern.cpp - Type Checking for Patterns ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "GenericTypeResolver.h"
#include "swift/Basic/StringExtras.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "llvm/Support/SaveAndRestore.h"
#include <utility>
using namespace swift;

/// If the given VarDecl is a computed property whose getter always returns a
/// particular enum element, return that element.
///
/// This requires the getter's body to have a certain syntactic form. It should
/// be kept in sync with importEnumCaseAlias in the ClangImporter library.
static EnumElementDecl *
extractEnumElement(TypeChecker &TC, DeclContext *DC, SourceLoc UseLoc,
                   const VarDecl *constant) {
  TC.diagnoseExplicitUnavailability(constant, UseLoc, DC, nullptr);

  const FuncDecl *getter = constant->getGetter();
  if (!getter)
    return nullptr;

  const BraceStmt *body = getter->getBody();
  if (!body || body->getNumElements() != 1)
    return nullptr;

  auto *retStmtRaw = body->getElement(0).dyn_cast<Stmt *>();
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
  if (ctorExpr->getDecl()->getDeclContext()
          ->getAsNominalTypeOrNominalTypeExtensionContext() != 
        constant->getDeclContext()
          ->getAsNominalTypeOrNominalTypeExtensionContext())
    return nullptr;

  return dyn_cast<EnumElementDecl>(ctorExpr->getDecl());
}

/// Find the first enum element in \p foundElements.
///
/// If there are no enum elements but there are properties, attempts to map
/// an arbitrary property to an enum element using extractEnumElement.
static EnumElementDecl *
filterForEnumElement(TypeChecker &TC, DeclContext *DC, SourceLoc UseLoc,
                     LookupResult foundElements) {
  EnumElementDecl *foundElement = nullptr;
  VarDecl *foundConstant = nullptr;

  for (LookupResult::Result result : foundElements) {
    ValueDecl *e = result.Decl;
    assert(e);
    if (e->isInvalid()) {
      continue;
    }
    // Skip if the enum element was referenced as an instance member
    if (!result.Base || !result.Base->getInterfaceType()->is<MetatypeType>()) {
      continue;
    }

    if (auto *oe = dyn_cast<EnumElementDecl>(e)) {
      // Ambiguities should be ruled out by parsing.
      assert(!foundElement && "ambiguity in enum case name lookup?!");
      foundElement = oe;
      continue;
    }

    if (auto *var = dyn_cast<VarDecl>(e)) {
      foundConstant = var;
      continue;
    }
  }

  if (!foundElement && foundConstant && foundConstant->hasClangNode())
    foundElement = extractEnumElement(TC, DC, UseLoc, foundConstant);

  return foundElement;
}

/// Find an unqualified enum element.
static EnumElementDecl *
lookupUnqualifiedEnumMemberElement(TypeChecker &TC, DeclContext *DC,
                                   Identifier name, SourceLoc UseLoc) {
  auto lookupOptions = defaultUnqualifiedLookupOptions;
  lookupOptions |= NameLookupFlags::KnownPrivate;
  auto lookup = TC.lookupUnqualified(DC, name, SourceLoc(), lookupOptions);
  return filterForEnumElement(TC, DC, UseLoc, lookup);
}

/// Find an enum element in an enum type.
static EnumElementDecl *
lookupEnumMemberElement(TypeChecker &TC, DeclContext *DC, Type ty,
                        Identifier name, SourceLoc UseLoc) {
  assert(ty->getAnyNominal());
  // Look up the case inside the enum.
  // FIXME: We should be able to tell if this is a private lookup.
  NameLookupOptions lookupOptions
    = defaultMemberLookupOptions - NameLookupFlags::DynamicLookup;
  LookupResult foundElements = TC.lookupMember(DC, ty, name, lookupOptions);
  return filterForEnumElement(TC, DC, UseLoc, foundElements);
}

namespace {
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
  
  bool visitTypeExpr(TypeExpr *te) {
    if (auto *TR = te->getTypeRepr())
      if (auto *CITR = dyn_cast<ComponentIdentTypeRepr>(TR)) {
        components.push_back(CITR);
        return true;
      }
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
  
  bool visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *udre) {
    assert(components.empty() && "decl ref should be root element of expr");
    // Track the AST location of the component.
    components.push_back(
      new (C) SimpleIdentTypeRepr(udre->getLoc(),
                                  udre->getName().getBaseName()));
    return true;
  }
  
  bool visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    if (!visit(ude->getBase()))
      return false;
    
    assert(!components.empty() && "no components before dot expr?!");

    // Track the AST location of the new component.
    components.push_back(
      new (C) SimpleIdentTypeRepr(ude->getLoc(), ude->getName().getBaseName()));
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
} // end anonymous namespace


namespace {
  class UnresolvedPatternFinder : public ASTWalker {
    bool &HadUnresolvedPattern;
  public:
    
    UnresolvedPatternFinder(bool &HadUnresolvedPattern)
      : HadUnresolvedPattern(HadUnresolvedPattern) {}
    
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // If we find an UnresolvedPatternExpr, return true.
      if (isa<UnresolvedPatternExpr>(E)) {
        HadUnresolvedPattern = true;
        return { false, E };
      }
      
      return { true, E };
    }
    
    static bool hasAny(Expr *E) {
      bool HasUnresolvedPattern = false;
      E->walk(UnresolvedPatternFinder(HasUnresolvedPattern));
      return HasUnresolvedPattern;
    }
  };
} // end anonymous namespace

namespace {
  
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
  
  // Convert a subexpression to a pattern if possible, or wrap it in an
  // ExprPattern.
  Pattern *getSubExprPattern(Expr *E) {
    if (Pattern *p = visit(E))
      return p;
    
    return new (TC.Context) ExprPattern(E, nullptr, nullptr);
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

  Pattern *visitVarPattern(VarPattern *P) {
    // Keep track of the fact that we're inside of a var/let pattern.  This
    // affects how unqualified identifiers are processed.
    P->setSubPattern(visit(P->getSubPattern()));
    
    // If the var pattern has no variables bound underneath it, then emit a
    // warning that the var/let is pointless.
    if (!P->isImplicit()) {
      bool HasVariable = false;
      P->forEachVariable([&](VarDecl *VD) { HasVariable = true; });
      
      if (!HasVariable) {
        TC.diagnose(P->getLoc(), diag::var_pattern_didnt_bind_variables,
                    P->isLet() ? "let" : "var")
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
    Pattern *subPattern = getSubExprPattern(E->getSubExpr());
    return new (TC.Context) ParenPattern(E->getLParenLoc(), subPattern,
                                         E->getRParenLoc());
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
    // If the unresolved member has an argument, turn it into a subpattern.
    Pattern *subPattern = nullptr;
    if (auto arg = ume->getArgument()) {
      subPattern = getSubExprPattern(arg);
    }
    
    // FIXME: Compound names.
    return new (TC.Context) EnumElementPattern(
                              ume->getDotLoc(),
                              ume->getNameLoc().getBaseNameLoc(),
                              ume->getName().getBaseName(),
                              subPattern,
                              ume);
  }
  
  // Member syntax 'T.Element' forms a pattern if 'T' is an enum and the
  // member name is a member of the enum.
  Pattern *visitUnresolvedDotExpr(UnresolvedDotExpr *ude) {
    GenericTypeToArchetypeResolver resolver(DC);

    SmallVector<ComponentIdentTypeRepr *, 2> components;
    if (!ExprToIdentTypeRepr(components, TC.Context).visit(ude->getBase()))
      return nullptr;
    
    auto *repr = IdentTypeRepr::create(TC.Context, components);
      
    // See if the repr resolves to a type.
    Type ty = TC.resolveIdentifierType(DC, repr, TR_AllowUnboundGenerics,
                                       /*diagnoseErrors*/false, &resolver,
                                       nullptr);
    
    auto *enumDecl = dyn_cast_or_null<EnumDecl>(ty->getAnyNominal());
    if (!enumDecl)
      return nullptr;

    // FIXME: Argument labels?
    EnumElementDecl *referencedElement
      = lookupEnumMemberElement(TC, DC, ty, ude->getName().getBaseName(),
                                ude->getLoc());
    if (!referencedElement)
      return nullptr;
    
    // Build a TypeRepr from the head of the full path.
    // FIXME: Compound names.
    TypeLoc loc(repr);
    loc.setType(ty);
    return new (TC.Context) EnumElementPattern(loc,
                                               ude->getDotLoc(),
                                               ude->getNameLoc()
                                                 .getBaseNameLoc(),
                                               ude->getName().getBaseName(),
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
    // FIXME: This shouldn't be needed.  It is only necessary because of the
    // poor representation of clang enum aliases and should be removed when
    // rdar://20879992 is addressed.
    //
    // Try looking up an enum element in context.
    if (EnumElementDecl *referencedElement
        = lookupUnqualifiedEnumMemberElement(TC, DC,
                                             ude->getName().getBaseName(),
                                             ude->getLoc())) {
      auto *enumDecl = referencedElement->getParentEnum();
      auto enumTy = enumDecl->getDeclaredTypeInContext();
      TypeLoc loc = TypeLoc::withoutLoc(enumTy);
      
      return new (TC.Context) EnumElementPattern(loc, SourceLoc(),
                                                 ude->getLoc(),
                                                 ude->getName().getBaseName(),
                                                 referencedElement,
                                                 nullptr);
    }
      
    
    // Perform unqualified name lookup to find out what the UDRE is.
    return getSubExprPattern(TC.resolveDeclRefExpr(ude, DC));
  }
  
  // Call syntax forms a pattern if:
  // - the callee in 'Element(x...)' or '.Element(x...)'
  //   references an enum element. The arguments then form a tuple
  //   pattern matching the element's data.
  // - the callee in 'T(...)' is a struct or class type. The argument tuple is
  //   then required to have keywords for every argument that name properties
  //   of the type.
  Pattern *visitCallExpr(CallExpr *ce) {
    GenericTypeToArchetypeResolver resolver(DC);

    if (!TC.Context.isSwiftVersion3()) {
      // swift(>=4) mode.
      // Specialized call are not allowed anyway.
      // Let it be diagnosed as an expression.
      // For Swift3 mode, we emit warnings just before constructing the
      // enum-element-pattern below.
      if (isa<UnresolvedSpecializeExpr>(ce->getFn()))
        return nullptr;
    }
    
    SmallVector<ComponentIdentTypeRepr *, 2> components;
    if (!ExprToIdentTypeRepr(components, TC.Context).visit(ce->getFn()))
      return nullptr;
    
    if (components.empty())
      return nullptr;

    auto tailComponent = components.pop_back_val();
    EnumElementDecl *referencedElement = nullptr;
    TypeLoc loc;

    if (components.empty()) {
      // Only one component. Try looking up an enum element in context.
      referencedElement
        = lookupUnqualifiedEnumMemberElement(TC, DC,
                                             tailComponent->getIdentifier(),
                                             tailComponent->getLoc());
      if (!referencedElement)
        return nullptr;

      auto *enumDecl = referencedElement->getParentEnum();
      loc = TypeLoc::withoutLoc(enumDecl->getDeclaredTypeInContext());
    } else {
      // Otherwise, see whether we had an enum type as the penultimate
      // component, and look up an element inside it.
      auto *prefixRepr = IdentTypeRepr::create(TC.Context, components);
      // See first if the entire repr resolves to a type.
      Type enumTy = TC.resolveIdentifierType(DC, prefixRepr,
                                             TR_AllowUnboundGenerics,
                                             /*diagnoseErrors*/false, &resolver,
                                             nullptr);
      if (!dyn_cast_or_null<EnumDecl>(enumTy->getAnyNominal()))
        return nullptr;

      referencedElement
        = lookupEnumMemberElement(TC, DC, enumTy,
                                  tailComponent->getIdentifier(),
                                  tailComponent->getLoc());
      if (!referencedElement)
        return nullptr;

      loc = TypeLoc(prefixRepr);
      loc.setType(enumTy);
    }

    if (auto generic = dyn_cast<GenericIdentTypeRepr>(tailComponent)) {
      assert(TC.Context.isSwiftVersion3() && "should be handled above");

      // Swift3 used to ignore the last generic argument clause:
      //   EnumTy.CaseVal<SomeType>()
      // used to be wrongfully converted to
      //   (pattern_enum_element type='EnumTy' EnumTy.CaseVal
      //     (pattern_tuple type='()' names=))
      // To keep source compatibility, just emit a warning with fix-it.
      TC.diagnose(generic->getAngleBrackets().Start,
                  diag::swift3_ignore_specialized_enum_element_call)
        .fixItRemove(generic->getAngleBrackets());
    }

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
Pattern *TypeChecker::resolvePattern(Pattern *P, DeclContext *DC,
                                     bool isStmtCondition) {
  P = ResolvePattern(*this, DC).visit(P);

  // If the entire pattern is "(pattern_expr (type_expr SomeType))", then this
  // is an invalid pattern.  If it were actually a value comparison (with ~=)
  // then the metatype would have had to be spelled with "SomeType.self".  What
  // they actually meant is to write "is SomeType", so we rewrite it to that
  // pattern for good QoI.
  if (auto *EP = dyn_cast<ExprPattern>(P))
    if (auto *TE = dyn_cast<TypeExpr>(EP->getSubExpr())) {
      diagnose(TE->getStartLoc(), diag::type_pattern_missing_is)
        .fixItInsert(TE->getStartLoc(), "is ");
      
      P = new (Context) IsPattern(TE->getStartLoc(), TE->getTypeLoc(),
                                  /*subpattern*/nullptr,
                                  CheckedCastKind::Unresolved);
    }
  
  // Look through a TypedPattern if present.
  auto *InnerP = P;
  if (auto *TP = dyn_cast<TypedPattern>(P))
    InnerP = TP->getSubPattern();

  // If the pattern was valid, check for an implicit VarPattern on the outer
  // level.  If so, we have an "if let" condition and we want to enforce some
  // more structure on it.
  if (isStmtCondition && isa<VarPattern>(InnerP) && InnerP->isImplicit()) {
    auto *Body = cast<VarPattern>(InnerP)->getSubPattern();

    // If they wrote a "x?" pattern, they probably meant "if let x".
    // Check for this and recover nicely if they wrote that.
    if (auto *OSP = dyn_cast<OptionalSomePattern>(Body)) {
      if (!OSP->getSubPattern()->isRefutablePattern()) {
        diagnose(OSP->getStartLoc(), diag::iflet_implicitly_unwraps)
          .highlight(OSP->getSourceRange())
          .fixItRemove(OSP->getQuestionLoc());
        return P;
      }
    }

    // If the pattern bound is some other refutable pattern, then they
    // probably meant:
    //   if case let <pattern> =
    if (Body->isRefutablePattern()) {
      diagnose(P->getLoc(), diag::iflet_pattern_matching)
        .fixItInsert(P->getLoc(), "case ");
      return P;
    }

    // "if let" implicitly looks inside of an optional, so wrap it in an
    // OptionalSome pattern.
    InnerP = new (Context) OptionalSomePattern(InnerP, InnerP->getEndLoc(),
                                               true);
    if (auto *TP = dyn_cast<TypedPattern>(P))
      TP->setSubPattern(InnerP);
    else
      P = InnerP;
  }

  return P;
}

static bool validateTypedPattern(TypeChecker &TC, DeclContext *DC,
                                 TypedPattern *TP,
                                 TypeResolutionOptions options,
                                 GenericTypeResolver *resolver) {
  if (TP->hasType())
    return TP->getType()->hasError();

  TypeLoc &TL = TP->getTypeLoc();
  bool hadError = TC.validateType(TL, DC, options, resolver);

  if (hadError)
    TP->setType(ErrorType::get(TC.Context));
  else
    TP->setType(TL.getType());
  return hadError;
}

static void diagnoseAndMigrateVarParameterToBody(ParamDecl *decl,
                                                 AbstractFunctionDecl *func,
                                                 TypeChecker &TC) {
  if (!func || !func->hasBody()) {
    // If there is no function body, just suggest removal.
    TC.diagnose(decl->getLetVarInOutLoc(),
                diag::var_parameter_not_allowed)
      .fixItRemove(decl->getLetVarInOutLoc());
    return;
  }
  // Insert the shadow copy. The computations that follow attempt to
  // 'best guess' the indentation and new lines so that the user
  // doesn't have to add any whitespace.
  auto declBody = func->getBody();
  
  auto &SM = TC.Context.SourceMgr;
  
  SourceLoc insertionStartLoc;
  std::string start;
  std::string end;
  
  auto lBraceLine = SM.getLineNumber(declBody->getLBraceLoc());
  auto rBraceLine = SM.getLineNumber(declBody->getRBraceLoc());

  if (!declBody->getNumElements()) {
    
    // Empty function body.
    insertionStartLoc = declBody->getRBraceLoc();
    
    if (lBraceLine == rBraceLine) {
      // Same line braces, means we probably have something
      // like {} as the func body. Insert directly into body with spaces.
      start = " ";
      end = " ";
    } else {
      // Different line braces, so use RBrace's indentation.
      end = "\n" + Lexer::getIndentationForLine(SM, declBody->
                                                getRBraceLoc()).str();
      start = "    "; // Guess 4 spaces as extra indentation.
    }
  } else {
    auto firstLine = declBody->getElement(0);
    insertionStartLoc = firstLine.getStartLoc();
    if (lBraceLine == SM.getLineNumber(firstLine.getStartLoc())) {
      // Function on same line, insert with semi-colon. Not ideal but
      // better than weird space alignment.
      start = "";
      end = "; ";
    } else {
      start = "";
      end = "\n" + Lexer::getIndentationForLine(SM, firstLine.
                                                getStartLoc()).str();
    }
  }
  if (insertionStartLoc.isInvalid()) {
    TC.diagnose(decl->getLetVarInOutLoc(),
                diag::var_parameter_not_allowed)
    .fixItRemove(decl->getLetVarInOutLoc());
    return;
  }
  auto parameterName = decl->getNameStr().str();
  TC.diagnose(decl->getLetVarInOutLoc(),
              diag::var_parameter_not_allowed)
  .fixItRemove(decl->getLetVarInOutLoc())
  .fixItInsert(insertionStartLoc, start + "var " + parameterName + " = " +
               parameterName + end);
}

static bool validateParameterType(ParamDecl *decl, DeclContext *DC,
                                  TypeResolutionOptions options,
                                  GenericTypeResolver &resolver,
                                  TypeChecker &TC) {
  if (auto ty = decl->getTypeLoc().getType())
    return ty->hasError();

  // If the element is a variadic parameter, resolve the parameter type as if
  // it were in non-parameter position, since we want functions to be
  // @escaping in this case.
  auto elementOptions = (options |
                         (decl->isVariadic() ? TR_VariadicFunctionInput
                                             : TR_FunctionInput));
  bool hadError = false;

  // We might have a null typeLoc if this is a closure parameter list,
  // where parameters are allowed to elide their types.
  if (!decl->getTypeLoc().isNull()) {
    hadError |= TC.validateType(decl->getTypeLoc(), DC,
                                elementOptions, &resolver);
  }

  Type Ty = decl->getTypeLoc().getType();
  if (decl->isVariadic() && !Ty.isNull() && !hadError) {
    Ty = TC.getArraySliceType(decl->getStartLoc(), Ty);
    if (Ty.isNull()) {
      hadError = true;
    }
    decl->getTypeLoc().setType(Ty);
  }
  // If the param is not a 'let' and it is not an 'inout'.
  // It must be a 'var'. Provide helpful diagnostics like a shadow copy
  // in the function body to fix the 'var' attribute.
  if (!decl->isLet() &&
      !decl->isImplicit() &&
      (Ty.isNull() || !Ty->is<InOutType>()) &&
      !hadError) {
    auto func = dyn_cast_or_null<AbstractFunctionDecl>(DC);
    diagnoseAndMigrateVarParameterToBody(decl, func, TC);
    decl->setInvalid();
    hadError = true;
  }

  if (hadError)
    decl->getTypeLoc().setType(ErrorType::get(TC.Context), /*validated*/true);

  return hadError;
}

/// Type check a parameter list.
bool TypeChecker::typeCheckParameterList(ParameterList *PL, DeclContext *DC,
                                         TypeResolutionOptions options,
                                         GenericTypeResolver &resolver) {
  bool hadError = false;
  
  for (auto param : *PL) {
    if (!param->getTypeLoc().getTypeRepr() &&
        param->hasInterfaceType()) {
      hadError |= param->isInvalid();
      continue;
    }

    hadError |= validateParameterType(param, DC, options, resolver, *this);
    
    auto type = param->getTypeLoc().getType();

    // If there was no type specified, and if we're not looking at a
    // ClosureExpr, then we have a parse error (no type was specified).  The
    // parser will have already diagnosed this, but treat this as a type error
    // as well to get the ParamDecl marked invalid and to get an ErrorType.
    if (!type) {
      // Closure argument lists are allowed to be missing types.
      if (options & TR_InExpression)
        continue;
      param->setInvalid();
    }
    
    if (param->isInvalid() || type->hasError()) {
      param->markInvalid();
      hadError = true;
    } else
      resolver.recordParamType(param, type);
    
    checkTypeModifyingDeclAttributes(param);
    if (!hadError && type->is<InOutType>()) {
      param->setLet(false);
    }
  }
  
  return hadError;
}


bool TypeChecker::typeCheckPattern(Pattern *P, DeclContext *dc,
                                   TypeResolutionOptions options) {
  GenericTypeToArchetypeResolver resolver(dc);

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
    if (typeCheckPattern(SP, dc, options)) {
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
    bool hadError = validateTypedPattern(*this, dc, TP, options, &resolver);
    Pattern *subPattern = TP->getSubPattern();
    if (coercePatternToType(subPattern, dc, P->getType(),
                            options|TR_FromNonInferredPattern, &resolver,
                            TP->getTypeLoc()))
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
        var->markInvalid();
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
    auto elementOptions = withoutContext(options);
    if (options & TR_ImmediateFunctionInput)
      elementOptions |= TR_FunctionInput;

    bool missingType = false;
    for (unsigned i = 0, e = tuplePat->getNumElements(); i != e; ++i) {
      TuplePatternElt &elt = tuplePat->getElement(i);
      Pattern *pattern = elt.getPattern();
      if (typeCheckPattern(pattern, dc, elementOptions)) {
        hadError = true;
        continue;
      }
      if (!pattern->hasType()) {
        missingType = true;
        continue;
      }

      typeElts.push_back(TupleTypeElt(pattern->getType(), elt.getLabel()));
    }

    if (hadError) {
      P->setType(ErrorType::get(Context));
      return true;
    }
    if (!missingType && !(options & TR_AllowUnspecifiedTypes)) {
      P->setType(TupleType::get(typeElts, Context));
    }
    return false;
  }
      
  //--- Refutable patterns.
  //
  // Refutable patterns occur when checking the PatternBindingDecls in if/let,
  // while/let, and let/else conditions.
  case PatternKind::Is:
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
  case PatternKind::Bool:
  case PatternKind::Expr:
    // In a let/else, these always require an initial value to match against.
    if (!(options & TR_AllowUnspecifiedTypes)) {
      diagnose(P->getLoc(), diag::refutable_pattern_requires_initializer);
      P->setType(ErrorType::get(Context));
      return true;
    }

    return false;
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
  auto *matchVar = new (tc.Context) VarDecl(/*IsStatic*/false, /*IsLet*/true,
                                            /*IsCaptureList*/false,
                                            pattern->getLoc(),
                                            tc.Context.getIdentifier("$match"),
                                            type, dc);
  matchVar->setInterfaceType(dc->mapTypeOutOfContext(type));
  matchVar->setHasNonPatternBindingInit();

  // Form the cast $match as? T, which produces an optional.
  Expr *matchRef = new (tc.Context) DeclRefExpr(matchVar,
                                                DeclNameLoc(pattern->getLoc()),
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
                                      GenericTypeResolver *resolver,
                                      TypeLoc tyLoc) {
recur:
  if (tyLoc.isNull()) {
    tyLoc = TypeLoc::withoutLoc(type);
  }

  TypeResolutionOptions subOptions = options - TR_EnumPatternPayload;
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
            && !tupleType->getElement(0).isVararg()) {
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
      if (!type->isEqual(TP->getType()) && !type->hasError()) {
        if (options & TR_OverrideType) {
          TP->setType(type);
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
      type = ErrorType::get(Context);
    var->setType(type);
    // FIXME: wtf
    if (type->hasTypeParameter())
      var->setInterfaceType(type);
    else
      var->setInterfaceType(var->getDeclContext()->mapTypeOutOfContext(type));

    checkTypeModifyingDeclAttributes(var);
    if (type->is<InOutType>()) {
      NP->getDecl()->setLet(false);
    }
    if (var->getAttrs().hasAttribute<OwnershipAttr>())
      type = getTypeOfRValue(var, false);
    else if (!var->isInvalid())
      type = var->getType();
    P->setType(type);
    var->getTypeLoc() = tyLoc;
    var->getTypeLoc().setType(var->getType());

    // If we are inferring a variable to have type AnyObject.Type,
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
    } else if (diagTy->isEqual(Context.TheEmptyTupleType)) {
      shouldRequireType = true;
    } else if (auto MTT = diagTy->getAs<AnyMetatypeType>()) {
      if (auto protoTy = MTT->getInstanceType()->getAs<ProtocolType>()) {
        shouldRequireType =
          protoTy->getDecl()->isSpecificProtocol(KnownProtocolKind::AnyObject);
      }
    }
    
    if (shouldRequireType && 
        !(options & TR_FromNonInferredPattern) &&
        !(options & TR_EnumerationVariable) &&
        !(options & TR_EditorPlaceholder)) {
      diagnose(NP->getLoc(), diag::type_inferred_to_undesirable_type,
               NP->getDecl()->getName(), type, NP->getDecl()->isLet());
      diagnose(NP->getLoc(), diag::add_explicit_type_annotation_to_silence);
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
    bool hadError = type->hasError();
    
    // Sometimes a paren is just a paren. If the tuple pattern has a single
    // element, we can reduce it to a paren pattern.
    bool canDecayToParen = TP->getNumElements() == 1;
    auto decayToParen = [&]() -> bool {
      assert(canDecayToParen);
      Pattern *sub = TP->getElement(0).getPattern();
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
      diagnose(TP->getStartLoc(), diag::tuple_pattern_in_non_tuple_context,
               type);
      hadError = true;
    }

    // The number of elements must match exactly.
    if (!hadError && tupleTy->getNumElements() != TP->getNumElements()) {
      if (canDecayToParen)
        return decayToParen();
      
      diagnose(TP->getStartLoc(), diag::tuple_pattern_length_mismatch, type);
      hadError = true;
    }

    // Coerce each tuple element to the respective type.
    P->setType(type);

    for (unsigned i = 0, e = TP->getNumElements(); i != e; ++i) {
      TuplePatternElt &elt = TP->getElement(i);
      Pattern *pattern = elt.getPattern();

      Type CoercionType;
      if (hadError)
        CoercionType = ErrorType::get(Context);
      else
        CoercionType = tupleTy->getElement(i).getType();
      
      // If the tuple pattern had a label for the tuple element, it must match
      // the label for the tuple type being matched.
      if (!hadError && !elt.getLabel().empty() &&
          elt.getLabel() != tupleTy->getElement(i).getName()) {
        diagnose(elt.getLabelLoc(), diag::tuple_pattern_label_mismatch,
                 elt.getLabel(), tupleTy->getElement(i).getName());
        hadError = true;
      }
      
      hadError |= coercePatternToType(pattern, dc, CoercionType,
                                      options, resolver);
      if (!hadError)
        elt.setPattern(pattern);
    }

    return hadError;
  }

  // Coerce expressions by finding a '~=' operator that can compare the
  // expression to a value of the coerced type.
  case PatternKind::Expr: {
    assert(cast<ExprPattern>(P)->isResolved()
           && "coercing unresolved expr pattern!");
    if (type->getAnyNominal() == Context.getBoolDecl()) {
      // The type is Bool.
      // Check if the pattern is a Bool literal
      auto EP = cast<ExprPattern>(P);
      if (auto *BLE = dyn_cast<BooleanLiteralExpr>(
              EP->getSubExpr()->getSemanticsProvidingExpr())) {
        P = new (Context) BoolPattern(BLE->getLoc(), BLE->getValue());
        P->setType(type);
        return false;
      }
    }

    // case nil is equivalent to .none when switching on Optionals.
    OptionalTypeKind Kind;
    if (type->getAnyOptionalObjectType(Kind)) {
      auto EP = cast<ExprPattern>(P);
      if (auto *NLE = dyn_cast<NilLiteralExpr>(EP->getSubExpr())) {
        auto *NoneEnumElement = Context.getOptionalNoneDecl(Kind);
        P = new (Context) EnumElementPattern(TypeLoc::withoutLoc(type),
                                             NLE->getLoc(), NLE->getLoc(),
                                             NoneEnumElement->getName(),
                                             NoneEnumElement, nullptr, false);
        return coercePatternToType(P, dc, type, options, resolver);
      }
    }
    return typeCheckExprPattern(cast<ExprPattern>(P), dc, type);
  }
      
  // Coerce an 'is' pattern by determining the cast kind.
  case PatternKind::Is: {
    auto IP = cast<IsPattern>(P);

    // Type-check the type parameter.
    if (validateType(IP->getCastTypeLoc(), dc, TR_InExpression))
      return true;

    auto castType = IP->getCastTypeLoc().getType();

    // Make sure we use any bridged NSError-related conformances.
    useBridgedNSErrorConformances(dc, castType);

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
                                               Context.Id_some,
                                               nullptr, sub,
                                               /*Implicit=*/true);
      }

      P = sub;
      return coercePatternToType(P, dc, type, options);
    }


    CheckedCastKind castKind
      = typeCheckCheckedCast(type, IP->getCastTypeLoc().getType(),
                             type->hasError()
                               ? CheckedCastContextKind::None
                               : CheckedCastContextKind::IsPattern,
                             dc,
                             IP->getLoc(),
                             nullptr,
                             IP->getCastTypeLoc().getSourceRange());
    switch (castKind) {
    case CheckedCastKind::Unresolved:
      return true;
    case CheckedCastKind::Coercion:
    case CheckedCastKind::BridgingCoercion:
      // If this is an 'as' pattern coercing between two different types, then
      // it is "useful" because it is providing a different type to the
      // sub-pattern.  If this is an 'is' pattern or an 'as' pattern where the
      // types are the same, then produce a warning.
      if (!IP->getSubPattern() ||
          type->isEqual(IP->getCastTypeLoc().getType())) {
        diagnose(IP->getLoc(), diag::isa_is_always_true,
                 IP->getSubPattern() ? "as" : "is");
      }
      IP->setCastKind(castKind);
      break;

    // Valid checks.
    case CheckedCastKind::ArrayDowncast:
    case CheckedCastKind::DictionaryDowncast:
    case CheckedCastKind::SetDowncast:
      return coercePatternViaConditionalDowncast(
               *this, P, dc, type,
               subOptions|TR_FromNonInferredPattern);

    case CheckedCastKind::ValueCast:
    case CheckedCastKind::Swift3BridgingDowncast:
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
    
    EnumElementDecl *elt = EEP->getElementDecl();
    
    Type enumTy;
    if (!elt) {
      if (type->getAnyNominal()) {
        elt = lookupEnumMemberElement(*this, dc, type, EEP->getName(),
                                      EEP->getLoc());
      }
      if (!elt) {
        if (!type->hasError()) {
          // Lowercasing of Swift.Optional's cases is handled in the
          // standard library itself, not through the clang importer,
          // so we have to do this check here. Additionally, .Some
          // isn't a static VarDecl, so the existing mechanics in
          // extractEnumElement won't work.
          if (type->getAnyNominal() == Context.getOptionalDecl()) {
            if (EEP->getName().str() == "None" ||
                EEP->getName().str() == "Some") {
              SmallString<4> Rename;
              camel_case::toLowercaseWord(EEP->getName().str(), Rename);
              diagnose(EEP->getLoc(), diag::availability_decl_unavailable_rename,
                          EEP->getName(), /*replaced*/false,
                          /*special kind*/0, Rename.str())
                .fixItReplace(EEP->getLoc(), Rename.str());

              return true;
            }
          
          // If we have the original expression parse tree, try reinterpreting
          // it as an expr-pattern if enum element lookup failed, since `.foo`
          // could also refer to a static member of the context type.
          } else if (EEP->hasUnresolvedOriginalExpr()) {
            P = new (Context) ExprPattern(EEP->getUnresolvedOriginalExpr(),
                                          nullptr, nullptr);
            goto recur;
          }

          diagnose(EEP->getLoc(), diag::enum_element_pattern_member_not_found,
                   EEP->getName().str(), type);
        }
        return true;
      }
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
        auto foundCastKind =
          typeCheckCheckedCast(type, parentTy,
                               CheckedCastContextKind::EnumElementPattern,
                               dc,
                               EEP->getLoc(),
                               nullptr, SourceRange());
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

    // If there is a subpattern, push the enum element type down onto it.
    if (EEP->hasSubPattern()) {
      Type elementType;
      if (auto argType = elt->getArgumentInterfaceType())
        elementType = enumTy->getTypeOfMember(elt->getModuleContext(),
                                              elt, argType);
      else
        elementType = TupleType::getEmpty(Context);
      Pattern *sub = EEP->getSubPattern();
      if (coercePatternToType(sub, dc, elementType,
                     subOptions|TR_FromNonInferredPattern|TR_EnumPatternPayload,
                     resolver))
        return true;
      EEP->setSubPattern(sub);
    }

    EEP->setElementDecl(elt);
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
    OptionalTypeKind optionalKind;
    Type elementType = type->getAnyOptionalObjectType(optionalKind);

    if (elementType.isNull()) {
      auto diagID = diag::optional_element_pattern_not_valid_type;
      SourceLoc loc = OP->getQuestionLoc();
      // Produce tailored diagnostic for if/let and other conditions.
      if (OP->isImplicit()) {
        diagID = diag::condition_optional_element_pattern_not_valid_type;
        loc = OP->getLoc();
      }

      diagnose(loc, diagID, type);
      return true;
    }

    EnumElementDecl *elementDecl = Context.getOptionalSomeDecl(optionalKind);
    assert(elementDecl && "missing optional some decl?!");
    OP->setElementDecl(elementDecl);

    Pattern *sub = OP->getSubPattern();
    if (coercePatternToType(sub, dc, elementType,
                            subOptions|TR_FromNonInferredPattern|TR_EnumPatternPayload,
                            resolver))
      return true;
    OP->setSubPattern(sub);
    OP->setType(type);
    return false;
  }

  case PatternKind::Bool:
    P->setType(type);
    return false;
  }
  llvm_unreachable("bad pattern kind!");
}


/// Coerce the specified parameter list of a ClosureExpr to the specified
/// contextual type.
///
/// \returns true if an error occurred, false otherwise.
///
/// TODO: These diagnostics should be a lot better now that we know this is
/// all specific to closures.
///
bool TypeChecker::coerceParameterListToType(ParameterList *P, ClosureExpr *CE,
                                            AnyFunctionType *FN) {
  Type paramListType = FN->getInput();
  bool hadError = paramListType->hasError();

  // Local function to check if the given type is valid e.g. doesn't have
  // errors, type variables or unresolved types related to it.
  auto isValidType = [](Type type) -> bool {
    return !(type.isNull() || type->hasError() || type->hasUnresolvedType() ||
             type->hasTypeVariable());
  };

  // Local function to check whether type of given parameter
  // should be coerced to a given contextual type or not.
  auto shouldOverwriteParam = [&](ParamDecl *param) -> bool {
    if (param->isInvalid())
      return true;

    if (auto type = param->getTypeLoc().getType())
      return !isValidType(type);

    return true;
  };

  GenericTypeToArchetypeResolver resolver(CE);

  // Sometimes a scalar type gets applied to a single-argument parameter list.
  auto handleParameter = [&](ParamDecl *param, Type ty) -> bool {
    bool hadError = false;
    
    // Check that the type, if explicitly spelled, is ok.
    if (param->getTypeLoc().getTypeRepr()) {
      hadError |= validateParameterType(param, CE, TypeResolutionOptions(),
                                        resolver, *this);
      
      // Now that we've type checked the explicit argument type, see if it
      // agrees with the contextual type.
      auto paramType = param->getTypeLoc().getType();
      // Coerce explicitly specified argument type to contextual type
      // only if both types are valid and do not match.
      if (!hadError && isValidType(ty) && !ty->isEqual(paramType)) {
        param->setType(ty);
        param->setInterfaceType(CE->mapTypeOutOfContext(ty));
      }
    }

    if (!ty->isMaterializable()) {
      if (ty->is<InOutType>()) {
        param->setLet(false);
      } else if (param->hasName()) {
        diagnose(param->getStartLoc(),
                 diag::param_type_non_materializable_tuple, ty);
      }
    }

    // If contextual type is invalid and we have a valid argument type
    // trying to coerce argument to contextual type would mean erasing
    // valuable diagnostic information.
    if (isValidType(ty) || shouldOverwriteParam(param)) {
      param->setType(ty);
      param->setInterfaceType(CE->mapTypeOutOfContext(ty));
    }
    
    checkTypeModifyingDeclAttributes(param);
    return hadError;
  };

  // Check if paramListType only contains one single tuple.
  // If it is, then paramListType would be sugared ParenType
  // with a single underlying TupleType. In that case, check if
  // the closure argument is also one to avoid the tuple splat
  // from happening.
  if (!hadError && isa<ParenType>(paramListType.getPointer())) {
    auto underlyingTy = cast<ParenType>(paramListType.getPointer())
      ->getUnderlyingType();
    
    if (underlyingTy->is<TupleType>() &&
        !underlyingTy->castTo<TupleType>()->getVarArgsBaseType()) {
      if (P->size() == 1)
        return handleParameter(P->get(0), underlyingTy);
    }
    
    //pass
  }
  
  // The context type must be a tuple.
  TupleType *tupleTy = paramListType->getAs<TupleType>();
  if (!tupleTy && !hadError) {
    if (P->size() == 1)
      return handleParameter(P->get(0), paramListType);
    diagnose(P->getStartLoc(), diag::tuple_pattern_in_non_tuple_context,
             paramListType);
    hadError = true;
  }
  
  // The number of elements must match exactly.
  // TODO: incomplete tuple patterns, with some syntax.
  if (!hadError && tupleTy->getNumElements() != P->size()) {
    auto fnType = FunctionType::get(paramListType->getDesugaredType(),
                                    FN->getResult());
    diagnose(P->getStartLoc(), diag::closure_argument_list_tuple,
             fnType, tupleTy->getNumElements(),
             P->size(), (P->size() == 1));
    hadError = true;
  }

  // Coerce each parameter to the respective type.
  for (unsigned i = 0, e = P->size(); i != e; ++i) {
    auto &param = P->get(i);
    
    Type CoercionType;
    if (hadError)
      CoercionType = ErrorType::get(Context);
    else
      CoercionType = tupleTy->getElement(i).getType();
    
    assert(param->getArgumentName().empty() &&
           "Closures cannot have API names");
    
    hadError |= handleParameter(param, CoercionType);
    assert(!param->isDefaultArgument() && "Closures cannot have default args");
  }
  
  return hadError;
}
