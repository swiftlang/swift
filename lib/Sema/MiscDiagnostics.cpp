//===--- MiscDiagnostics.cpp - AST-Level Diagnostics ----------------------===//
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
// This file implements AST-level diagnostics.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/MapVector.h"
using namespace swift;

//===--------------------------------------------------------------------===//
// Diagnose assigning variable to itself.
//===--------------------------------------------------------------------===//

static Decl *findSimpleReferencedDecl(const Expr *E) {
  if (auto *LE = dyn_cast<LoadExpr>(E))
    E = LE->getSubExpr();

  if (auto *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl();

  return nullptr;
}

static std::pair<Decl *, Decl *> findReferencedDecl(const Expr *E) {
  if (auto *LE = dyn_cast<LoadExpr>(E))
    E = LE->getSubExpr();

  if (auto *D = findSimpleReferencedDecl(E))
    return std::make_pair(nullptr, D);

  if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
    if (auto *BaseDecl = findSimpleReferencedDecl(MRE->getBase()))
      return std::make_pair(BaseDecl, MRE->getMember().getDecl());
  }

  return std::make_pair(nullptr, nullptr);
}

/// Diagnose assigning variable to itself.
static void diagSelfAssignment(TypeChecker &TC, const Expr *E) {
  auto *AE = dyn_cast<AssignExpr>(E);
  if (!AE)
    return;

  auto LHSDecl = findReferencedDecl(AE->getDest());
  auto RHSDecl = findReferencedDecl(AE->getSrc());
  if (LHSDecl.second && LHSDecl == RHSDecl) {
    TC.diagnose(AE->getLoc(), LHSDecl.first ? diag::self_assignment_prop
                                            : diag::self_assignment_var)
        .highlight(AE->getDest()->getSourceRange())
        .highlight(AE->getSrc()->getSourceRange());
  }
}


/// Issue a warning on code where a returned expression is on a different line
/// than the return keyword, but both have the same indentation.
///
/// \code
///   ...
///   return
///   foo()
/// \endcode
static void diagUnreachableCode(TypeChecker &TC, const Stmt *S) {
  auto *RS = dyn_cast<ReturnStmt>(S);
  if (!RS)
    return;
  if (!RS->hasResult())
    return;

  auto RetExpr = RS->getResult();
  auto RSLoc = RS->getStartLoc();
  auto RetExprLoc = RetExpr->getStartLoc();
  // FIXME: Expose getColumnNumber() in LLVM SourceMgr to make this check
  // cheaper.
  if (RSLoc.isInvalid() || RetExprLoc.isInvalid() || (RSLoc == RetExprLoc))
    return;
  SourceManager &SM = TC.Context.SourceMgr;
  if (SM.getLineAndColumn(RSLoc).second ==
      SM.getLineAndColumn(RetExprLoc).second) {
    TC.diagnose(RetExpr->getStartLoc(), diag::unindented_code_after_return);
    TC.diagnose(RetExpr->getStartLoc(), diag::indent_expression_to_silence);
    return;
  }
  return;
}


/// Diagnose syntactic restrictions of expressions:
///   - Module values may only occur as part of qualification.
///   - Metatype names cannot generally be used as values: they need a "T.self"
///     qualification unless used in narrow case (e.g. T() for construction).
///
static void diagSyntacticUseRestrictions(TypeChecker &TC, const Expr *E) {
  class DiagnoseWalker : public ASTWalker {
    SmallPtrSet<Expr*, 4> AlreadyDiagnosedMetatypes;
    SmallPtrSet<DeclRefExpr*, 4> AlreadyDiagnosedNoEscapes;
  public:
    TypeChecker &TC;

    DiagnoseWalker(TypeChecker &TC) : TC(TC) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // See through implicit conversions of the expression.  We want to be able
      // to associate the parent of this expression with the ultimate callee.
      auto Base = E;
      while (auto Conv = dyn_cast<ImplicitConversionExpr>(Base))
        Base = Conv->getSubExpr();

      if (auto *DRE = dyn_cast<DeclRefExpr>(Base)) {
        // Verify metatype uses.
        if (isa<TypeDecl>(DRE->getDecl())) {
          if (isa<ModuleDecl>(DRE->getDecl()))
            checkUseOfModule(DRE);
          else
            checkUseOfMetaTypeName(Base);
        }

        // Verify noescape parameter uses.
        checkNoEscapeParameterUse(DRE, nullptr);
      }
      if (auto *MRE = dyn_cast<MemberRefExpr>(Base))
        if (isa<TypeDecl>(MRE->getMember().getDecl()))
          checkUseOfMetaTypeName(Base);
      if (isa<TypeExpr>(Base))
        checkUseOfMetaTypeName(Base);

      // Check function calls, looking through implicit conversions on the
      // function and inspecting the arguments directly.
      if (auto *Call = dyn_cast<ApplyExpr>(E)) {
        // Check the callee, looking through implicit conversions.
        auto Base = Call->getFn();
        while (auto Conv = dyn_cast<ImplicitConversionExpr>(Base))
          Base = Conv->getSubExpr();
        if (auto *DRE = dyn_cast<DeclRefExpr>(Base))
          checkNoEscapeParameterUse(DRE, Call);

        auto *Arg = Call->getArg();

        // The argument could be shuffled if it includes default arguments,
        // label differences, or other exciting things like that.
        if (auto *TSE = dyn_cast<TupleShuffleExpr>(Arg))
          Arg = TSE->getSubExpr();

        // The argument is either a ParenExpr or TupleExpr.
        ArrayRef<Expr*> arguments;
        if (auto *TE = dyn_cast<TupleExpr>(Arg))
          arguments = TE->getElements();
        else
          arguments = Call->getArg();

        // Check each argument.
        for (auto arg : arguments) {
          while (1) {
            if (auto conv = dyn_cast<ImplicitConversionExpr>(arg))
              arg = conv->getSubExpr();
            else if (auto *PE = dyn_cast<ParenExpr>(arg))
              arg = PE->getSubExpr();
            else
              break;
          }

          if (auto *DRE = dyn_cast<DeclRefExpr>(arg))
            checkNoEscapeParameterUse(DRE, Call);
        }
      }

      return { true, E };
    }

    void checkUseOfModule(DeclRefExpr *E) {
      // Allow module values as a part of:
      // - ignored base expressions;
      // - expressions that failed to type check.
      if (auto *ParentExpr = Parent.getAsExpr()) {
        if (isa<DotSyntaxBaseIgnoredExpr>(ParentExpr) ||
            isa<UnresolvedDotExpr>(ParentExpr))
          return;
      }

      TC.diagnose(E->getStartLoc(), diag::value_of_module_type);
    }

    /// The DRE argument is a reference to a noescape parameter.  Verify that
    /// its uses are ok.
    void checkNoEscapeParameterUse(DeclRefExpr *DRE, Expr *ParentExpr=nullptr) {
      // This only cares about declarations marked noescape.
      if (!DRE->getDecl()->getAttrs().hasAttribute<NoEscapeAttr>())
        return;

      // Only diagnose this once.  If we check and accept this use higher up in
      // the AST, don't recheck here.
      if (!AlreadyDiagnosedNoEscapes.insert(DRE).second)
        return;

      // The only valid use of the noescape parameter is an immediate call,
      // either as the callee or as an argument (in which case, the typechecker
      // validates that the noescape bit didn't get stripped off).
      if (ParentExpr && isa<ApplyExpr>(ParentExpr)) // param()
        return;

      TC.diagnose(DRE->getStartLoc(), diag::invalid_noescape_use,
                  DRE->getDecl()->getName());
      if (DRE->getDecl()->getAttrs().hasAttribute<AutoClosureAttr>() &&
          DRE->getDecl()->getAttrs().getAttribute<NoEscapeAttr>()->isImplicit())
        TC.diagnose(DRE->getDecl()->getLoc(), diag::noescape_autoclosure,
                    DRE->getDecl()->getName());
    }

    // Diagnose metatype values that don't appear as part of a property,
    // method, or constructor reference.
    void checkUseOfMetaTypeName(Expr *E) {
      // If we've already checked this at a higher level, we're done.
      if (!AlreadyDiagnosedMetatypes.insert(E).second)
        return;

      // Allow references to types as a part of:
      // - member references T.foo, T.Type, T.self, etc. (but *not* T.type)
      // - constructor calls T()
      if (auto *ParentExpr = Parent.getAsExpr()) {
        // Reject use of "T.dynamicType", it should be written as "T.self".
        if (auto metaExpr = dyn_cast<DynamicTypeExpr>(ParentExpr)) {
          // Add a fixit to replace '.dynamicType' with '.self'.
          TC.diagnose(E->getStartLoc(), diag::type_of_metatype)
            .fixItReplace(metaExpr->getMetatypeLoc(), "self");
          return;
        }

        // This is the white-list of accepted syntactic forms.
        if (isa<ErrorExpr>(ParentExpr) ||
            isa<DotSelfExpr>(ParentExpr) ||               // T.self
            isa<CallExpr>(ParentExpr) ||                  // T()
            isa<MemberRefExpr>(ParentExpr) ||             // T.foo
            isa<UnresolvedMemberExpr>(ParentExpr) ||
            isa<SelfApplyExpr>(ParentExpr) ||             // T.foo()  T()
            isa<UnresolvedDotExpr>(ParentExpr) ||
            isa<DotSyntaxBaseIgnoredExpr>(ParentExpr) ||
            isa<UnresolvedSelectorExpr>(ParentExpr) ||
            isa<UnresolvedSpecializeExpr>(ParentExpr) ||
            isa<OpenExistentialExpr>(ParentExpr)) {
          return;
        }
      }

      TC.diagnose(E->getStartLoc(), diag::value_of_metatype_type);
      // Add fixits to insert '()' or '.self'.
      TC.diagnose(E->getEndLoc(), diag::add_parens_to_type)
        .fixItInsertAfter(E->getEndLoc(), "()");
      TC.diagnose(E->getEndLoc(), diag::add_self_to_type)
        .fixItInsertAfter(E->getEndLoc(), ".self");
    }
  };

  DiagnoseWalker Walker(TC);
  const_cast<Expr *>(E)->walk(Walker);
}


/// Diagnose recursive use of properties within their own accessors
static void diagRecursivePropertyAccess(TypeChecker &TC, const Expr *E,
                                        const DeclContext *DC) {
  auto fn = dyn_cast<FuncDecl>(DC);
  if (!fn || !fn->isAccessor())
    return;

  auto var = dyn_cast<VarDecl>(fn->getAccessorStorageDecl());
  if (!var)  // Ignore subscripts
    return;

  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;
    VarDecl *Var;
    const FuncDecl *Accessor;

  public:
    explicit DiagnoseWalker(TypeChecker &TC, VarDecl *var,
                            const FuncDecl *Accessor)
      : TC(TC), Var(var), Accessor(Accessor) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
        // Handle local and top-level computed variables.
        if (DRE->getDecl() == Var &&
            DRE->getAccessSemantics() != AccessSemantics::DirectToStorage &&
            Accessor->getAccessorKind() != AccessorKind::IsMaterializeForSet) {
          bool shouldDiagnose = true;
          if (auto *ParentExpr = Parent.getAsExpr()) {
            if (isa<DotSyntaxBaseIgnoredExpr>(ParentExpr))
              shouldDiagnose = false;
            else if (Accessor->isSetter())
              shouldDiagnose = !isa<LoadExpr>(ParentExpr);
          }
          if (shouldDiagnose) {
            TC.diagnose(E->getLoc(), diag::recursive_accessor_reference,
                        Var->getName(), Accessor->isSetter());
          }
        }
        
        // If this is a direct store in a "willSet", we reject this because
        // it is about to get overwritten.
        if (DRE->getDecl() == Var &&
            DRE->getAccessSemantics() == AccessSemantics::DirectToStorage &&
            !dyn_cast_or_null<LoadExpr>(Parent.getAsExpr()) &&
            Accessor->getAccessorKind() == AccessorKind::IsWillSet) {
          TC.diagnose(E->getLoc(), diag::store_in_willset, Var->getName());
        }


      } else if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
        // Handle instance and type computed variables.
        // Find MemberRefExprs that have an implicit "self" base.
        if (MRE->getMember().getDecl() == Var &&
            isa<DeclRefExpr>(MRE->getBase()) &&
            MRE->getBase()->isImplicit()) {
          
          if (MRE->getAccessSemantics() != AccessSemantics::DirectToStorage) {
            bool shouldDiagnose = false;
            // Warn about any property access in the getter.
            if (Accessor->isGetter())
              shouldDiagnose = true;
            // Warn about stores in the setter, but allow loads.
            if (Accessor->isSetter())
              shouldDiagnose = !dyn_cast_or_null<LoadExpr>(Parent.getAsExpr());

            if (shouldDiagnose) {
              TC.diagnose(E->getLoc(), diag::recursive_accessor_reference,
                          Var->getName(), Accessor->isSetter());
              TC.diagnose(E->getLoc(),
                          diag::recursive_accessor_reference_silence)
              .fixItInsert(E->getStartLoc(), "self.");
            }
          } else {
            // If this is a direct store in a "willSet", we reject this because
            // it is about to get overwritten.
            if (!dyn_cast_or_null<LoadExpr>(Parent.getAsExpr()) &&
                Accessor->getAccessorKind() == AccessorKind::IsWillSet) {
              TC.diagnose(E->getLoc(), diag::store_in_willset, Var->getName());
            }
          }
        }

      }

      return { true, E };
    }
  };

  DiagnoseWalker walker(TC, var, fn);
  const_cast<Expr *>(E)->walk(walker);
}

/// Look for any property references in closures that lack a "self." qualifier.
/// Within a closure, we require that the source code contain "self." explicitly
/// because 'self' is captured, not the property value.  This is a common source
/// of confusion, so we force an explicit self.
static void diagnoseImplicitSelfUseInClosure(TypeChecker &TC, const Expr *E,
                                             const DeclContext *DC) {
  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;
    unsigned InClosure;
  public:
    explicit DiagnoseWalker(TypeChecker &TC, bool isAlreadyInClosure)
        : TC(TC), InClosure(isAlreadyInClosure) {}

    /// Return true if this is an implicit reference to self.
    static bool isImplicitSelfUse(Expr *E) {
      auto *DRE = dyn_cast<DeclRefExpr>(E);
      return DRE && DRE->isImplicit() && DRE->getDecl()->hasName() &&
             DRE->getDecl()->getName().str() == "self";
    }

    /// Return true if this is a closure expression that will require "self."
    /// qualification of member references.
    static bool isClosureRequiringSelfQualification(
                  const AbstractClosureExpr *CE) {
      if (auto *CCE = dyn_cast<ClosureExpr>(CE))
        if (CCE->isDeferBody())
          return false;
      
      // If the closure's type was inferred to be noescape, then it doesn't
      // need qualification.
      return !AnyFunctionRef(const_cast<AbstractClosureExpr *>(CE))
               .isKnownNoEscape();
    }


    // Don't walk into nested decls.
    bool walkToDeclPre(Decl *D) override {
      return false;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto *CE = dyn_cast<AbstractClosureExpr>(E)) {
        if (!CE->hasSingleExpressionBody())
          return { false, E };

        // If this is a potentially-escaping closure expression, start looking
        // for references to self if we aren't already.
        if (isClosureRequiringSelfQualification(CE))
          ++InClosure;
      }


      // If we aren't in a closure, no diagnostics will be produced.
      if (!InClosure)
        return { true, E };

      // If we see a property reference with an implicit base from within a
      // closure, then reject it as requiring an explicit "self." qualifier.  We
      // do this in explicit closures, not autoclosures, because otherwise the
      // transparence of autoclosures is lost.
      if (auto *MRE = dyn_cast<MemberRefExpr>(E))
        if (isImplicitSelfUse(MRE->getBase())) {
          TC.diagnose(MRE->getLoc(),
                      diag::property_use_in_closure_without_explicit_self,
                      MRE->getMember().getDecl()->getName())
            .fixItInsert(MRE->getLoc(), "self.");
          return { false, E };
        }

      // Handle method calls with a specific diagnostic + fixit.
      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(E))
        if (isImplicitSelfUse(DSCE->getBase()) &&
            isa<DeclRefExpr>(DSCE->getFn())) {
          auto MethodExpr = cast<DeclRefExpr>(DSCE->getFn());
          TC.diagnose(DSCE->getLoc(),
                      diag::method_call_in_closure_without_explicit_self,
                      MethodExpr->getDecl()->getName())
            .fixItInsert(DSCE->getLoc(), "self.");
          return { false, E };
        }

      // Catch any other implicit uses of self with a generic diagnostic.
      if (isImplicitSelfUse(E))
        TC.diagnose(E->getLoc(), diag::implicit_use_of_self_in_closure);

      return { true, E };
    }
    
    Expr *walkToExprPost(Expr *E) override {
      if (auto *CE = dyn_cast<AbstractClosureExpr>(E)) {
        if (isClosureRequiringSelfQualification(CE)) {
          assert(InClosure);
          --InClosure;
        }
      }
      
      return E;
    }
  };

  bool isAlreadyInClosure = false;
  if (DC->isLocalContext()) {
    while (DC->getParent()->isLocalContext() && !isAlreadyInClosure) {
      if (auto *closure = dyn_cast<AbstractClosureExpr>(DC))
        if (DiagnoseWalker::isClosureRequiringSelfQualification(closure))
          isAlreadyInClosure = true;
      DC = DC->getParent();
    }
  }
  const_cast<Expr *>(E)->walk(DiagnoseWalker(TC, isAlreadyInClosure));
}

//===--------------------------------------------------------------------===//
// Diagnose availability.
//===--------------------------------------------------------------------===//

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted=".
static bool diagnoseExplicitUnavailability(TypeChecker &TC, const ValueDecl *D,
                                           SourceRange R,
                                           const DeclContext *DC) {
  auto *Attr = AvailableAttr::isUnavailable(D);
  if (!Attr)
    return false;

  // Suppress the diagnostic if we are in synthesized code inside
  // a synthesized function and the reference is lexically
  // contained in a declaration that is itself marked unavailable.
  // The right thing to do here is to not synthesize that code in the
  // first place. rdar://problem/20491640
  if (R.isInvalid() && TC.isInsideImplicitFunction(R, DC) &&
      TC.isInsideUnavailableDeclaration(R, DC)) {
    return false;
  }

  SourceLoc Loc = R.Start;
  auto Name = D->getFullName();

  switch (Attr->getUnconditionalAvailability()) {
  case UnconditionalAvailabilityKind::Deprecated:
    break;

  case UnconditionalAvailabilityKind::None:
  case UnconditionalAvailabilityKind::Unavailable:
    if (!Attr->Rename.empty()) {
      TC.diagnose(Loc, diag::availability_decl_unavailable_rename, Name,
                  Attr->Rename).fixItReplace(R, Attr->Rename);
    } else if (Attr->Message.empty()) {
      TC.diagnose(Loc, diag::availability_decl_unavailable, Name).highlight(R);
    } else {
      TC.diagnose(Loc, diag::availability_decl_unavailable_msg, Name,
                  Attr->Message).highlight(R);
    }
    break;

  case UnconditionalAvailabilityKind::UnavailableInSwift:
    if (Attr->Message.empty()) {
      TC.diagnose(Loc, diag::availability_decl_unavailable_in_swift, Name)
          .highlight(R);
    } else {
      TC.diagnose(Loc, diag::availability_decl_unavailable_in_swift_msg, Name,
                  Attr->Message).highlight(R);
    }
    break;
  }

  auto MinVersion = TC.Context.LangOpts.getMinPlatformVersion();
  switch (Attr->getMinVersionAvailability(MinVersion)) {
  case MinVersionComparison::Available:
  case MinVersionComparison::PotentiallyUnavailable:
    llvm_unreachable("These aren't considered unavailable");

  case MinVersionComparison::Unavailable:
    TC.diagnose(D, diag::availability_marked_unavailable, Name)
        .highlight(Attr->getRange());
    break;

  case MinVersionComparison::Obsoleted:
    // FIXME: Use of the platformString here is non-awesome for application
    // extensions.
    TC.diagnose(D, diag::availability_obsoleted, Name,
                Attr->prettyPlatformString(),
                *Attr->Obsoleted).highlight(Attr->getRange());
    break;
  }
  return true;
}

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
static bool diagAvailability(TypeChecker &TC, const ValueDecl *D,
                             SourceRange R, const DeclContext *DC) {
  if (!D)
    return false;

  if (diagnoseExplicitUnavailability(TC, D, R, DC))
    return true;

  // Diagnose for deprecation
  if (const AvailableAttr *Attr = TypeChecker::getDeprecated(D)) {
    TC.diagnoseDeprecated(R, DC, Attr, D->getFullName());
  }
  
  // We only diagnose potentially unavailability here if availability checking
  // is turned on, but we are not treating unavailable symbols as having
  // optional type.
  if (TC.getLangOpts().DisableAvailabilityChecking ||
      TC.getLangOpts().EnableExperimentalUnavailableAsOptional) {
    return false;
  }

  // Diagnose for potential unavailability
  auto maybeUnavail = TC.checkDeclarationAvailability(D, R.Start, DC);
  if (maybeUnavail.hasValue()) {
    TC.diagnosePotentialUnavailability(D, R, DC, maybeUnavail.getValue());
    return true;
  }
  return false;
}

namespace {
class AvailabilityWalker : public ASTWalker {
  /// Describes how the next member reference will be treated as we traverse
  /// the AST.
  enum class MemberAccessContext : unsigned {
    /// The member reference is in a context where an access will call
    /// the getter.
    Getter,

    /// The member reference is in a context where an access will call
    /// the setter.
    Setter,

    /// The member reference is in a context where it will be turned into
    /// an inout argument. (Once this happens, we have to conservatively assume
    /// that both the getter and setter could be called.)
    InOut
  };

  TypeChecker &TC;
  const DeclContext *DC;
  const MemberAccessContext AccessContext;

public:
  AvailabilityWalker(
      TypeChecker &TC, const DeclContext *DC,
      MemberAccessContext AccessContext = MemberAccessContext::Getter)
      : TC(TC), DC(DC), AccessContext(AccessContext) {}

  virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (auto DR = dyn_cast<DeclRefExpr>(E))
      diagAvailability(TC, DR->getDecl(), DR->getSourceRange(), DC);
    if (auto MR = dyn_cast<MemberRefExpr>(E)) {
      walkMemberRef(MR);
      return std::make_pair(false, E);
    }
    if (auto OCDR = dyn_cast<OtherConstructorDeclRefExpr>(E))
      diagAvailability(TC, OCDR->getDecl(), OCDR->getConstructorLoc(), DC);
    if (auto DMR = dyn_cast<DynamicMemberRefExpr>(E))
      diagAvailability(TC, DMR->getMember().getDecl(), DMR->getNameLoc(), DC);
    if (auto DS = dyn_cast<DynamicSubscriptExpr>(E))
      diagAvailability(TC, DS->getMember().getDecl(), DS->getSourceRange(), DC);
    if (auto S = dyn_cast<SubscriptExpr>(E)) {
      if (S->hasDecl())
        diagAvailability(TC, S->getDecl().getDecl(), S->getSourceRange(), DC);
    }
    if (auto A = dyn_cast<AssignExpr>(E)) {
      walkAssignExpr(A);
      return std::make_pair(false, E);
    }
    if (auto IO = dyn_cast<InOutExpr>(E)) {
      walkInOutExpr(IO);
      return std::make_pair(false, E);
    }
    
    return std::make_pair(true, E);
  }

private:
  /// Walk an assignment expression, checking for availability.
  void walkAssignExpr(AssignExpr *E) const {
    // We take over recursive walking of assignment expressions in order to
    // walk the destination and source expressions in different member
    // access contexts.
    Expr *Dest = E->getDest();
    if (!Dest) {
      return;
    }

    // Check the Dest expression in a setter context.
    // We have an implicit assumption here that the first MemberRefExpr
    // encountered walking (pre-order) is the Dest is the destination of the
    // write. For the moment this is fine -- but future syntax might violate
    // this assumption.
    walkInContext(Dest, MemberAccessContext::Setter);

    // Check RHS in getter context
    Expr *Source = E->getSrc();
    if (!Source) {
      return;
    }
    walkInContext(Source, MemberAccessContext::Getter);
  }
  
  /// Walk a member reference expression, checking for availability.
  void walkMemberRef(MemberRefExpr *E) {
    // Walk the base in a getter context.
    walkInContext(E->getBase(), MemberAccessContext::Getter);

    ValueDecl *D = E->getMember().getDecl();
    // Diagnose for the the member declaration itself.
    if (diagAvailability(TC, D, E->getNameLoc(), DC)) {
      return;
    }

    if (TC.getLangOpts().DisableAvailabilityChecking ||
        TC.getLangOpts().EnableExperimentalUnavailableAsOptional) {
      return;
    }

    if (auto *ASD = dyn_cast<AbstractStorageDecl>(D)) {
      // Diagnose for appropriate accessors, given the access context.
      diagStorageAccess(ASD, E->getSourceRange(), DC);
    }
  }
  
  /// Walk an inout expression, checking for availability.
  void walkInOutExpr(InOutExpr *E) {
    walkInContext(E->getSubExpr(), MemberAccessContext::InOut);
  }

  /// Walk the given expression in the member access context.
  void walkInContext(Expr *E, MemberAccessContext AccessContext) const {
    E->walk(AvailabilityWalker(TC, DC, AccessContext));
  }

  /// Emit diagnostics, if necessary, for accesses to storage where
  /// the accessor for the AccessContext is not available.
  void diagStorageAccess(AbstractStorageDecl *D,
                         SourceRange ReferenceRange,
                         const DeclContext *ReferenceDC) const {
    if (!D->hasAccessorFunctions()) {
      return;
    }
    
    // Check availability of accessor functions
    switch (AccessContext) {
    case MemberAccessContext::Getter:
      diagAccessorAvailability(D->getGetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/false);
      break;

    case MemberAccessContext::Setter:
      diagAccessorAvailability(D->getSetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/false);
      break;

    case MemberAccessContext::InOut:
      diagAccessorAvailability(D->getGetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/true);

      diagAccessorAvailability(D->getSetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/true);
      break;
    }
  }

  /// Emit a diagnostic, if necessary for a potentially unavailable accessor.
  /// Returns true if a diagnostic was emitted.
  void diagAccessorAvailability(FuncDecl *D, SourceRange ReferenceRange,
                                const DeclContext *ReferenceDC,
                                bool ForInout) const {
    if (!D) {
      return;
    }
    auto MaybeUnavail = TC.checkDeclarationAvailability(D, ReferenceRange.Start,
                                                        DC);
    if (MaybeUnavail.hasValue()) {
      TC.diagnosePotentialAccessorUnavailability(D, ReferenceRange, ReferenceDC,
                                                 MaybeUnavail.getValue(),
                                                 ForInout);
    }
  }
};
}

/// Diagnose uses of unavailable declarations.
static void diagAvailability(TypeChecker &TC, const Expr *E,
                             const DeclContext *DC) {
  AvailabilityWalker walker(TC, DC);
  const_cast<Expr*>(E)->walk(walker);
}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//

void swift::performExprDiagnostics(TypeChecker &TC, const Expr *E,
                                   const DeclContext *DC) {
  diagSelfAssignment(TC, E);
  diagSyntacticUseRestrictions(TC, E);
  diagRecursivePropertyAccess(TC, E, DC);
  diagnoseImplicitSelfUseInClosure(TC, E, DC);
  diagAvailability(TC, E, DC);
}

void swift::performStmtDiagnostics(TypeChecker &TC, const Stmt *S) {
  TC.checkUnsupportedProtocolType(const_cast<Stmt *>(S));
  return diagUnreachableCode(TC, S);
}

//===--------------------------------------------------------------------===//
// Per func/init diagnostics
//===--------------------------------------------------------------------===//

namespace {
class VarDeclUsageChecker : public ASTWalker {
  TypeChecker &TC;
  
  // Keep track of some information about a variable.
  enum {
    RK_Read    = 1,      ///< Whether it was ever read.
    RK_Written = 2,      ///< Whether it was ever written or passed inout.
  };
  
  /// These are all of the variables that we are tracking.  VarDecls get added
  /// to this when the declaration is seen.  We use a MapVector to keep the
  /// diagnostics emission in deterministic order.
  llvm::SmallMapVector<VarDecl*, unsigned, 32> VarDecls;
  
  bool sawError = false;
  
public:
  VarDeclUsageChecker(TypeChecker &TC, AbstractFunctionDecl *AFD) : TC(TC) {
    // Track the parameters of the function.
    for (auto P : AFD->getBodyParamPatterns())
      P->forEachVariable([&](VarDecl *VD) {
        if (shouldTrackVarDecl(VD))
          VarDecls[VD] = 0;
      });
  }

  // After we have scanned the entire region, diagnose variables that could be
  // declared with a narrower usage kind.
  ~VarDeclUsageChecker();

  /// Check to see if the specified VarDecl is part of a larger
  /// PatternBindingDecl, where some other bound variable was mutated.  In this
  /// case we don't want to generate a "variable never mutated" warning, because
  /// it would require splitting up the destructuring of the tuple, which is
  ///  more code turmoil than the warning is worth.
  bool isVarDeclPartOfPBDThatHadSomeMutation(VarDecl *VD) {
    auto *PBD = VD->getParentPatternBinding();
    if (!PBD) return false;

    bool sawMutation = false;
    for (const auto &PBE : PBD->getPatternList()) {
      PBE.ThePattern->forEachVariable([&](VarDecl *VD) {
        auto it = VarDecls.find(VD);
        sawMutation |= it != VarDecls.end() && (it->second & RK_Written);
      });
    }
    return sawMutation;
  }

  bool shouldTrackVarDecl(VarDecl *VD) {
    // If the variable is implicit, ignore it.
    if (VD->isImplicit() || VD->getLoc().isInvalid())
      return false;
    
    // If the variable was invalid, ignore it and notice that the code is
    // malformed.
    if (VD->isInvalid() || !VD->hasType()) {
      sawError = true;
      return false;
    }
    
    // If the variable is already unnamed, ignore it.
    if (!VD->hasName() || VD->getName().str() == "_")
      return false;
    
    return true;
  }

  void addMark(Decl *D, unsigned Flag) {
    auto *vd = dyn_cast<VarDecl>(D);
    if (!vd) return;

    auto vdi = VarDecls.find(vd);
    if (vdi != VarDecls.end())
      vdi->second |= Flag;
  }
  
  void markBaseOfAbstractStorageDeclStore(Expr *E, ConcreteDeclRef decl);
  
  void markStoredOrInOutExpr(Expr *E, unsigned Flags);
  
  // We generally walk into declarations, other than types and nested functions.
  // FIXME: peek into capture lists of nested functions.
  bool walkToDeclPre(Decl *D) override {
    if (isa<TypeDecl>(D))
      return false;
      
    // If this is a VarDecl, then add it to our list of things to track.
    if (auto *vd = dyn_cast<VarDecl>(D))
      if (shouldTrackVarDecl(vd))
        VarDecls[vd] = 0;

    if (auto *afd = dyn_cast<AbstractFunctionDecl>(D)) {
      // If this is a nested function with a capture list, mark any captured
      // variables.
      if (afd->isBodyTypeChecked()) {
        for (const auto &capture : afd->getCaptureInfo().getCaptures())
          addMark(capture.getDecl(), RK_Read|RK_Written);
      } else {
        // If the body hasn't been type checked yet, be super-conservative and
        // mark all variables as used.  This can be improved later, e.g. by
        // walking the untype-checked body to look for things that could
        // possibly be used.
        VarDecls.clear();
      }
      
      // Don't walk into it though, it may not even be type checked yet.
      return false;
    }

    
    // Note that we ignore the initialization behavior of PatternBindingDecls,
    // but we do want to walk into them, because we want to see any uses or
    // other things going on in the initializer expressions.
    return true;
  }
  
  /// The heavy lifting happens when visiting expressions.
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;
};
}


// After we have scanned the entire region, diagnose variables that could be
// declared with a narrower usage kind.
VarDeclUsageChecker::~VarDeclUsageChecker() {
  // If we saw an ErrorExpr somewhere in the body, then we have a malformed AST
  // and we know stuff got dropped.  Instead of producing these diagnostics,
  // lets let the bigger issues get resolved first.
  if (sawError)
    return;
  
  for (auto elt : VarDecls) {
    auto *var = elt.first;
    unsigned access = elt.second;

    // If this is a 'let' value, any stores to it are actually initializations,
    // not mutations.
    if (var->isLet())
      access &= ~RK_Written;
    
    // If this variable has WeakStorageType, then it can be mutated in ways we
    // don't know.
    if (var->getType()->is<WeakStorageType>())
      access |= RK_Written;
    
    // If this is a vardecl with 'inout' type, then it is an inout argument to a
    // function, never diagnose anything related to it.
    if (var->getType()->is<InOutType>())
      continue;    
    
    // Consider parameters to always have been read.  It is common to name a
    // parameter and not use it (e.g. because you are an override or want the
    // named keyword, etc).  Warning to rewrite it to _ is more annoying than
    // it is useful.
    if (isa<ParamDecl>(var))
      access |= RK_Read;
    
    // Diagnose variables that were never used (other than their
    // initialization).
    //
    if (access == 0) {
      // If the source of the VarDecl is a trivial PatternBinding with only a
      // single binding, rewrite the whole thing into an assignment.
      //    let x = foo()
      //  ->
      //    _ = foo()
      if (auto *pbd = var->getParentPatternBinding())
        if (pbd->getSingleVar() == var && pbd->getInit(0) != nullptr) {
          unsigned varKind = var->isLet();
          TC.diagnose(var->getLoc(), diag::pbd_never_used,
                      var->getName(), varKind)
            .fixItReplace(SourceRange(pbd->getLoc(), var->getLoc()), "_");
          continue;
        }
      
      // Otherwise, this is something more complex, perhaps
      //    let (a,b) = foo()
      // Just rewrite the one variable with a _.
      unsigned varKind = var->isLet();
      TC.diagnose(var->getLoc(), diag::variable_never_used,
                  var->getName(), varKind)
        .fixItReplace(var->getLoc(), "_");
      continue;
    }
    
    // If this is a mutable 'var', and it was never written to, suggest
    // upgrading to 'let'.  We do this even for a parameter.
    if (!var->isLet() && (access & RK_Written) == 0 &&
        // Don't warn if we have something like "let (x,y) = ..." and 'y' was
        // never mutated, but 'x' was.
        !isVarDeclPartOfPBDThatHadSomeMutation(var)) {
      SourceLoc FixItLoc;
      
      // Try to find the location of the 'var' so we can produce a fixit.  If
      // this is a simple PatternBinding, use its location.
      if (auto *PBD = var->getParentPatternBinding())
        if (PBD->getSingleVar() == var)
          FixItLoc = PBD->getLoc();
      
      // If this is a parameter explicitly marked 'var', remove it.
      if (auto *param = dyn_cast<ParamDecl>(var))
        if (auto *pattern = param->getParamParentPattern())
          if (auto *vp = dyn_cast<VarPattern>(pattern)) {
            TC.diagnose(var->getLoc(), diag::variable_never_mutated,
                        var->getName(), /*param*/1)
              .fixItRemove(vp->getLoc());
            continue;
          }
      
      unsigned varKind = isa<ParamDecl>(var);
      // FIXME: fixit when we can find a pattern binding.
      if (FixItLoc.isInvalid())
        TC.diagnose(var->getLoc(), diag::variable_never_mutated,
                    var->getName(), varKind);
      else
        TC.diagnose(var->getLoc(), diag::variable_never_mutated,
                    var->getName(), varKind)
          .fixItReplace(FixItLoc, "let");
      continue;
    }
    
    // If this is a variable that was only written to, emit a warning.
    if ((access & RK_Read) == 0) {
      TC.diagnose(var->getLoc(), diag::variable_never_read, var->getName(),
                  isa<ParamDecl>(var));
      continue;
    }
  }
}

/// Handle a store to "x.y" where 'base' is the expression for x and 'decl' is
/// the decl for 'y'.
void VarDeclUsageChecker::
markBaseOfAbstractStorageDeclStore(Expr *base, ConcreteDeclRef decl) {
  // If the base is a class or an rvalue, then this store just loads the base.
  if (base->getType()->isAnyClassReferenceType() ||
      !(base->getType()->isLValueType() || base->getType()->is<InOutType>())) {
    base->walk(*this);
    return;
  }

  // If the store is to a non-mutating member, then this is just a load, even
  // if the base is an inout expr.
  auto *ASD = cast<AbstractStorageDecl>(decl.getDecl());
  if ((ASD->hasAccessorFunctions() && ASD->getSetter() &&
       !ASD->getSetter()->isMutating()) ||
      (ASD->hasAddressors() && ASD->getMutableAddressor() &&
       !ASD->getMutableAddressor()->isMutating())) {
    // Sema conservatively converts the base to inout expr when it is an lvalue.
    // Look through it because we know it isn't actually doing a load/store.
    if (auto *ioe = dyn_cast<InOutExpr>(base))
      base = ioe->getSubExpr();
    base->walk(*this);
    return;
  }
  
  // Otherwise this is a read and write of the base.
  return markStoredOrInOutExpr(base, RK_Written|RK_Read);
}


 void VarDeclUsageChecker::markStoredOrInOutExpr(Expr *E, unsigned Flags) {
   // Sema leaves some subexpressions null, which seems really unfortunate.  It
   // should replace them with ErrorExpr.
   if (E == nullptr || !E->getType() || E->getType()->is<ErrorType>()) {
     sawError = true;
     return;
   }
   
   // Ignore parens and other easy cases.
   E = E->getSemanticsProvidingExpr();
   
   // If we found a decl that is being assigned to, then mark it.
   if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
     addMark(DRE->getDecl(), Flags);
     return;
   }

   if (auto *TE = dyn_cast<TupleExpr>(E)) {
     for (auto &elt : TE->getElements())
       markStoredOrInOutExpr(elt, Flags);
     return;
   }
   
   // If this is an assignment into a mutating subscript lvalue expr, then we
   // are mutating the base expression.  We also need to visit the index
   // expressions as loads though.
   if (auto *SE = dyn_cast<SubscriptExpr>(E)) {
     // The index of the subscript is evaluted as an rvalue.
     SE->getIndex()->walk(*this);
     if (SE->hasDecl())
       markBaseOfAbstractStorageDeclStore(SE->getBase(), SE->getDecl());
     else  // FIXME: Should not be needed!
       markStoredOrInOutExpr(SE->getBase(), RK_Written|RK_Read);
     
     return;
   }
   
   if (auto *ioe = dyn_cast<InOutExpr>(E))
     return markStoredOrInOutExpr(ioe->getSubExpr(), RK_Written|RK_Read);
   
   if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
     markBaseOfAbstractStorageDeclStore(MRE->getBase(), MRE->getMember());
     return;
   }
   
   if (auto *TEE = dyn_cast<TupleElementExpr>(E))
     return markStoredOrInOutExpr(TEE->getBase(), Flags);

   if (auto *FVE = dyn_cast<ForceValueExpr>(E))
     return markStoredOrInOutExpr(FVE->getSubExpr(), Flags);

   // If we don't know what kind of expression this is, assume it's a reference
   // and mark it as a read.
   E->walk(*this);
}

   

/// The heavy lifting happens when visiting expressions.
std::pair<bool, Expr *> VarDeclUsageChecker::walkToExprPre(Expr *E) {
  // Sema leaves some subexpressions null, which seems really unfortunate.  It
  // should replace them with ErrorExpr.
  if (E == nullptr || !E->getType() || E->getType()->is<ErrorType>()) {
    sawError = true;
    return { false, E };
  }

  // If this is a DeclRefExpr found in a random place, it is a load of the
  // vardecl.
  if (auto *DRE = dyn_cast<DeclRefExpr>(E))
    addMark(DRE->getDecl(), RK_Read);

  // If this is an AssignExpr, see if we're mutating something that we know
  // about.
  if (auto *assign = dyn_cast<AssignExpr>(E)) {
    markStoredOrInOutExpr(assign->getDest(), RK_Written);
    
    // Don't walk into the LHS of the assignment, only the RHS.
    assign->getSrc()->walk(*this);
    return { false, E };
  }
  
  // '&x' is a read and write of 'x'.
  if (auto *io = dyn_cast<InOutExpr>(E)) {
    markStoredOrInOutExpr(io->getSubExpr(), RK_Read|RK_Written);
    // Don't bother walking into this.
    return { false, E };
  }
  
  // If we saw an ErrorExpr, take note of this.
  if (isa<ErrorExpr>(E))
    sawError = true;
  
  return { true, E };
}



/// Perform diagnostics for func/init/deinit declarations.
void swift::performAbstractFuncDeclDiagnostics(TypeChecker &TC,
                                               AbstractFunctionDecl *AFD) {
  assert(AFD->getBody() && "Need a body to check");
  
  // Don't produce these diagnostics for implicitly generated code.
  if (AFD->getLoc().isInvalid() || AFD->isImplicit() || AFD->isInvalid())
    return;
  
  // Check for unused variables, as well as variables that are could be
  // declared as constants.
  AFD->getBody()->walk(VarDeclUsageChecker(TC, AFD));
}




//===--------------------------------------------------------------------===//
// Utility functions
//===--------------------------------------------------------------------===//

void swift::fixItAccessibility(InFlightDiagnostic &diag, ValueDecl *VD,
                               Accessibility desiredAccess, bool isForSetter) {
  StringRef fixItString;
  switch (desiredAccess) {
  case Accessibility::Private:  fixItString = "private ";  break;
  case Accessibility::Internal: fixItString = "internal "; break;
  case Accessibility::Public:   fixItString = "public ";   break;
  }

  DeclAttributes &attrs = VD->getAttrs();
  DeclAttribute *attr;
  if (isForSetter) {
    attr = attrs.getAttribute<SetterAccessibilityAttr>();
    cast<AbstractStorageDecl>(VD)->overwriteSetterAccessibility(desiredAccess);
  } else {
    attr = attrs.getAttribute<AccessibilityAttr>();
    VD->overwriteAccessibility(desiredAccess);

    if (auto *ASD = dyn_cast<AbstractStorageDecl>(VD)) {
      if (auto *getter = ASD->getGetter())
        getter->overwriteAccessibility(desiredAccess);

      if (auto *setterAttr = attrs.getAttribute<SetterAccessibilityAttr>()) {
        if (setterAttr->getAccess() > desiredAccess)
          fixItAccessibility(diag, VD, desiredAccess, true);
      } else {
        ASD->overwriteSetterAccessibility(desiredAccess);
      }
    }
  }

  if (isForSetter && VD->getFormalAccess() == desiredAccess) {
    assert(attr);
    attr->setInvalid();
    if (!attr->Range.isValid())
      return;

    // Remove the setter attribute along with a possible single trailing space.
    SourceManager &sourceMgr = VD->getASTContext().SourceMgr;
    SourceLoc nextCharLoc = Lexer::getLocForEndOfToken(sourceMgr,
                                                       attr->Range.End);
    StringRef nextChar = sourceMgr.extractText({ nextCharLoc, 1 });
    if (nextChar == " ")
      diag.fixItRemoveChars(attr->Range.Start, nextCharLoc.getAdvancedLoc(1));
    else
      diag.fixItRemove(attr->Range);

  } else if (attr) {
    // This uses getLocation() instead of getRange() because we don't want to
    // replace the "(set)" part of a setter attribute.
    diag.fixItReplace(attr->getLocation(), fixItString.drop_back());
    attr->setInvalid();

  } else if (auto var = dyn_cast<VarDecl>(VD)) {
    if (auto PBD = var->getParentPatternBinding())
      diag.fixItInsert(PBD->getStartLoc(), fixItString);

  } else {
    diag.fixItInsert(VD->getStartLoc(), fixItString);
  }
}
