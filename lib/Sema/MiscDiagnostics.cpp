//===--- MiscDiagnostics.cpp - AST-Level Diagnostics ----------------------===//
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
// This file implements AST-level diagnostics.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

/// Return true if this expression is an implicit promotion from T to T?.
static Expr *isImplicitPromotionToOptional(Expr *E) {
  if (E->isImplicit())
    if (auto IIOE = dyn_cast<InjectIntoOptionalExpr>(
                                               E->getSemanticsProvidingExpr()))
      return IIOE->getSubExpr();
  return nullptr;
}

/// Diagnose syntactic restrictions of expressions.
///
///   - Module values may only occur as part of qualification.
///   - Metatype names cannot generally be used as values: they need a "T.self"
///     qualification unless used in narrow case (e.g. T() for construction).
///   - '_' may only exist on the LHS of an assignment expression.
///   - warn_unqualified_access values must not be accessed except via qualified
///     lookup.
///   - Partial application of some decls isn't allowed due to implementation
///     limitations.
///   - "&" (aka InOutExpressions) may only exist directly in function call
///     argument lists.
///   - 'self.init' and 'super.init' cannot be wrapped in a larger expression
///     or statement.
///   - Warn about promotions to optional in specific syntactic forms.
///   - Error about collection literals that default to Any collections in
///     invalid positions.
///
static void diagSyntacticUseRestrictions(TypeChecker &TC, const Expr *E,
                                         const DeclContext *DC,
                                         bool isExprStmt) {
  class DiagnoseWalker : public ASTWalker {
    SmallPtrSet<Expr*, 4> AlreadyDiagnosedMetatypes;
    SmallPtrSet<DeclRefExpr*, 4> AlreadyDiagnosedNoEscapes;
    SmallPtrSet<DeclRefExpr*, 4> AlreadyDiagnosedBitCasts;
    
    // Keep track of acceptable DiscardAssignmentExpr's.
    SmallPtrSet<DiscardAssignmentExpr*, 2> CorrectDiscardAssignmentExprs;

    /// Keep track of InOutExprs
    SmallPtrSet<InOutExpr*, 2> AcceptableInOutExprs;

    /// Keep track of the arguments to CallExprs.
    SmallPtrSet<Expr *, 2> CallArgs;

    bool IsExprStmt;

  public:
    TypeChecker &TC;
    const DeclContext *DC;

    DiagnoseWalker(TypeChecker &TC, const DeclContext *DC, bool isExprStmt)
      : IsExprStmt(isExprStmt), TC(TC), DC(DC) {}

    // Selector for the partial_application_of_function_invalid diagnostic
    // message.
    struct PartialApplication {
      enum : unsigned {
        MutatingMethod,
        SuperInit,
        SelfInit,
      };
      enum : unsigned {
        Error,
        CompatibilityWarning,
      };
      unsigned compatibilityWarning: 1;
      unsigned kind : 2;
      unsigned level : 29;
    };

    // Partial applications of functions that are not permitted.  This is
    // tracked in post-order and unraveled as subsequent applications complete
    // the call (or not).
    llvm::SmallDenseMap<Expr*, PartialApplication,2> InvalidPartialApplications;

    ~DiagnoseWalker() override {
      for (auto &unapplied : InvalidPartialApplications) {
        unsigned kind = unapplied.second.kind;
        if (unapplied.second.compatibilityWarning) {
          TC.diagnose(unapplied.first->getLoc(),
                      diag::partial_application_of_function_invalid_swift4,
                      kind);
        } else {
          TC.diagnose(unapplied.first->getLoc(),
                      diag::partial_application_of_function_invalid,
                      kind);
        }
      }
    }

    /// methods are fully applied when they can't support partial application.
    void checkInvalidPartialApplication(Expr *E) {
      if (auto AE = dyn_cast<ApplyExpr>(E)) {
        Expr *fnExpr = AE->getSemanticFn();
        if (auto forceExpr = dyn_cast<ForceValueExpr>(fnExpr))
          fnExpr = forceExpr->getSubExpr()->getSemanticsProvidingExpr();
        if (auto dotSyntaxExpr = dyn_cast<DotSyntaxBaseIgnoredExpr>(fnExpr))
          fnExpr = dotSyntaxExpr->getRHS();

        // Check to see if this is a potentially unsupported partial
        // application of a constructor delegation.
        if (isa<OtherConstructorDeclRefExpr>(fnExpr)) {
          auto kind = AE->getArg()->isSuperExpr()
                    ? PartialApplication::SuperInit
                    : PartialApplication::SelfInit;

          // Partial applications of delegated initializers aren't allowed, and
          // don't really make sense to begin with.
          InvalidPartialApplications.insert(
            {E, {PartialApplication::Error, kind, 1}});
          return;
        }

        // If this is adding a level to an active partial application, advance
        // it to the next level.
        auto foundApplication = InvalidPartialApplications.find(fnExpr);
        if (foundApplication == InvalidPartialApplications.end())
          return;

        unsigned level = foundApplication->second.level;
        auto kind = foundApplication->second.kind;
        assert(level > 0);
        InvalidPartialApplications.erase(foundApplication);
        if (level > 1) {
          // We have remaining argument clauses.
          // Partial applications were always diagnosed in Swift 4 and before,
          // so there's no need to preserve the compatibility warning bit.
          InvalidPartialApplications.insert(
            {AE, {PartialApplication::Error, kind, level - 1}});
        }
        return;
      }
      
      /// If this is a reference to a mutating method, it cannot be partially
      /// applied or even referenced without full application, so arrange for
      /// us to check that it gets fully applied.
      auto fnDeclRef = dyn_cast<DeclRefExpr>(E);
      if (!fnDeclRef)
        return;

      auto fn = dyn_cast<FuncDecl>(fnDeclRef->getDecl());
      if (!fn || !fn->isInstanceMember() || !fn->isMutating())
        return;

      // Swift 4 and earlier failed to diagnose a reference to a mutating method
      // without any applications at all, which would get miscompiled into a
      // function with undefined behavior. Warn for source compatibility.
      auto errorBehavior = TC.Context.LangOpts.isSwiftVersionAtLeast(5)
        ? PartialApplication::Error
        : PartialApplication::CompatibilityWarning;

      InvalidPartialApplications.insert(
        {fnDeclRef, {errorBehavior,
                     PartialApplication::MutatingMethod, 2}});
    }

    // Not interested in going outside a basic expression.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      return { false, P };
    }
    bool walkToDeclPre(Decl *D) override { return false; }
    bool walkToTypeReprPre(TypeRepr *T) override { return true; }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // See through implicit conversions of the expression.  We want to be able
      // to associate the parent of this expression with the ultimate callee.
      auto Base = E;
      while (auto Conv = dyn_cast<ImplicitConversionExpr>(Base))
        Base = Conv->getSubExpr();

      // Record call arguments.
      if (auto Call = dyn_cast<CallExpr>(Base))
        CallArgs.insert(Call->getArg());

      if (auto *DRE = dyn_cast<DeclRefExpr>(Base)) {
        // Verify metatype uses.
        if (isa<TypeDecl>(DRE->getDecl())) {
          if (isa<ModuleDecl>(DRE->getDecl()))
            checkUseOfModule(DRE);
          else
            checkUseOfMetaTypeName(Base);
        }

        // Verify noescape parameter uses.
        checkNoEscapeParameterUse(DRE, Parent.getAsExpr(), OperandKind::None);

        // Verify warn_unqualified_access uses.
        checkUnqualifiedAccessUse(DRE);
        
        // Verify that special decls are eliminated.
        checkForDeclWithSpecialTypeCheckingSemantics(DRE);
        
        // Verify that `unsafeBitCast` isn't misused.
        checkForSuspiciousBitCasts(DRE, nullptr);
      }
      if (auto *MRE = dyn_cast<MemberRefExpr>(Base)) {
        if (isa<TypeDecl>(MRE->getMember().getDecl()))
          checkUseOfMetaTypeName(Base);
      }
      if (isa<TypeExpr>(Base))
        checkUseOfMetaTypeName(Base);

      if (auto *SE = dyn_cast<SubscriptExpr>(E)) {
        // Implicit InOutExpr's are allowed in the base of a subscript expr.
        if (auto *IOE = dyn_cast<InOutExpr>(SE->getBase()))
          if (IOE->isImplicit())
            AcceptableInOutExprs.insert(IOE);

        visitIndices(SE, [&](unsigned argIndex, Expr *arg) {
          arg = lookThroughArgument(arg);
          if (auto *DRE = dyn_cast<DeclRefExpr>(arg))
            checkNoEscapeParameterUse(DRE, SE, OperandKind::Argument);
        });
      }

      if (auto *AE = dyn_cast<CollectionExpr>(E)) {
        visitCollectionElements(AE, [&](unsigned argIndex, Expr *arg) {
          arg = lookThroughArgument(arg);
          if (auto *DRE = dyn_cast<DeclRefExpr>(arg))
            checkNoEscapeParameterUse(DRE, AE, OperandKind::Argument);
        });
      }

      // Check decl refs in withoutActuallyEscaping blocks.
      if (auto MakeEsc = dyn_cast<MakeTemporarilyEscapableExpr>(E)) {
        if (auto DRE =
              dyn_cast<DeclRefExpr>(MakeEsc->getNonescapingClosureValue()))
          checkNoEscapeParameterUse(DRE, MakeEsc, OperandKind::MakeEscapable);
      }

      // Check function calls, looking through implicit conversions on the
      // function and inspecting the arguments directly.
      if (auto *Call = dyn_cast<ApplyExpr>(E)) {
        // Warn about surprising implicit optional promotions.
        checkOptionalPromotions(Call);
        
        // Check the callee, looking through implicit conversions.
        auto base = Call->getFn();
        unsigned uncurryLevel = 0;
        while (auto conv = dyn_cast<ImplicitConversionExpr>(base))
          base = conv->getSubExpr();

        const auto findDynamicMemberRefExpr =
            [](Expr *e) -> DynamicMemberRefExpr* {
          if (auto open = dyn_cast<OpenExistentialExpr>(e)) {
            return dyn_cast<DynamicMemberRefExpr>(open->getSubExpr());
          }
          return nullptr;
        };

        if (auto force = dyn_cast<ForceValueExpr>(base)) {
          if (auto ref = findDynamicMemberRefExpr(force->getSubExpr()))
            base = ref;
        } else if (auto bind = dyn_cast<BindOptionalExpr>(base)) {
          if (auto ref = findDynamicMemberRefExpr(bind->getSubExpr()))
            base = ref;
        }

        while (auto ignoredBase = dyn_cast<DotSyntaxBaseIgnoredExpr>(base))
          base = ignoredBase->getRHS();

        ConcreteDeclRef callee;
        if (auto *calleeDRE = dyn_cast<DeclRefExpr>(base)) {
          checkNoEscapeParameterUse(calleeDRE, Call, OperandKind::Callee);
          checkForSuspiciousBitCasts(calleeDRE, Call);
          callee = calleeDRE->getDeclRef();

        // Otherwise, try to drill down through member calls for the purposes
        // of argument-matching code below.
        } else if (auto selfApply = dyn_cast<SelfApplyExpr>(base)) {
          uncurryLevel++;
          base = selfApply->getSemanticFn();
          if (auto calleeDRE = dyn_cast<DeclRefExpr>(base))
            callee = calleeDRE->getDeclRef();

        // Otherwise, check for a dynamic member.
        } else if (auto dynamicMRE = dyn_cast<DynamicMemberRefExpr>(base)) {
          uncurryLevel++;
          callee = dynamicMRE->getMember();
        }

        visitArguments(Call, [&](unsigned argIndex, Expr *arg) {
          // InOutExpr's are allowed in argument lists directly.
          if (auto *IOE = dyn_cast<InOutExpr>(arg)) {
            if (isa<CallExpr>(Call))
              AcceptableInOutExprs.insert(IOE);
          }
          // InOutExprs can be wrapped in some implicit casts.
          Expr *unwrapped = arg;
          if (auto *IIO = dyn_cast<InjectIntoOptionalExpr>(arg))
            unwrapped = IIO->getSubExpr();

          if (isa<InOutToPointerExpr>(unwrapped) ||
              isa<ArrayToPointerExpr>(unwrapped) ||
              isa<ErasureExpr>(unwrapped)) {
            auto operand =
              cast<ImplicitConversionExpr>(unwrapped)->getSubExpr();
            if (auto *IOE = dyn_cast<InOutExpr>(operand)) {
              AcceptableInOutExprs.insert(IOE);
              operand = IOE->getSubExpr();
            }

            // Also do some additional work based on how the function uses
            // the argument.
            if (callee) {
              checkConvertedPointerArgument(callee, uncurryLevel, argIndex,
                                            unwrapped, operand);
            }
          }

          // Also give special treatment to noescape function arguments.
          arg = lookThroughArgument(arg);

          if (auto *DRE = dyn_cast<DeclRefExpr>(arg))
            checkNoEscapeParameterUse(DRE, Call, OperandKind::Argument);
        });
      }
      
      // If we have an assignment expression, scout ahead for acceptable _'s.
      if (auto *AE = dyn_cast<AssignExpr>(E))
        markAcceptableDiscardExprs(AE->getDest());

      /// Diagnose a '_' that isn't on the immediate LHS of an assignment.
      if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(E)) {
        if (!CorrectDiscardAssignmentExprs.count(DAE) &&
            !DAE->getType()->hasError())
          TC.diagnose(DAE->getLoc(), diag::discard_expr_outside_of_assignment);
      }

      // Diagnose an '&' that isn't in an argument lists.
      if (auto *IOE = dyn_cast<InOutExpr>(E)) {
        if (!IOE->isImplicit() && !AcceptableInOutExprs.count(IOE) &&
            !IOE->getType()->hasError())
          TC.diagnose(IOE->getLoc(), diag::inout_expr_outside_of_call)
            .highlight(IOE->getSubExpr()->getSourceRange());
      }

      // Diagnose 'self.init' or 'super.init' nested in another expression
      // or closure.
      if (auto *rebindSelfExpr = dyn_cast<RebindSelfInConstructorExpr>(E)) {
        if (!Parent.isNull() || !IsExprStmt || DC->getParent()->isLocalContext()) {
          bool isChainToSuper;
          (void)rebindSelfExpr->getCalledConstructor(isChainToSuper);
          TC.diagnose(E->getLoc(), diag::init_delegation_nested,
                      isChainToSuper, !IsExprStmt);
        }
      }

      return { true, E };
    }

    /// Visit the argument/s represented by either a ParenExpr or TupleExpr,
    /// unshuffling if needed. If any other kind of expression, will pass it
    /// straight back.
    static void argExprVisitArguments(Expr* arg,
                                      llvm::function_ref
                                        <void(unsigned, Expr*)> fn) {
      // The argument could be shuffled if it includes default arguments,
      // label differences, or other exciting things like that.
      if (auto *TSE = dyn_cast<TupleShuffleExpr>(arg))
        arg = TSE->getSubExpr();

      // The argument is either a ParenExpr or TupleExpr.
      if (auto *TE = dyn_cast<TupleExpr>(arg)) {
        auto elts = TE->getElements();
        for (auto i : indices(elts))
          fn(i, elts[i]);
      } else if (auto *PE = dyn_cast<ParenExpr>(arg)) {
        fn(0, PE->getSubExpr());
      } else {
        fn(0, arg);
      }
    }

    static void visitIndices(SubscriptExpr *subscript,
                             llvm::function_ref<void(unsigned, Expr*)> fn) {
      auto *indexArgs = subscript->getIndex();
      argExprVisitArguments(indexArgs, fn);
    }

    static void visitArguments(ApplyExpr *apply,
                               llvm::function_ref<void(unsigned, Expr*)> fn) {
      auto *arg = apply->getArg();
      argExprVisitArguments(arg, fn);
    }

    static void visitCollectionElements(CollectionExpr *collection,
                               llvm::function_ref<void(unsigned, Expr*)> fn) {
      auto elts = collection->getElements();
      for (auto i : indices(elts))
        fn(i, elts[i]);
    }

    static Expr *lookThroughArgument(Expr *arg) {
      while (1) {
        if (auto conv = dyn_cast<ImplicitConversionExpr>(arg))
          arg = conv->getSubExpr();
        else if (auto *PE = dyn_cast<ParenExpr>(arg))
          arg = PE->getSubExpr();
        else
          break;
      }
      return arg;
    }

    Expr *walkToExprPost(Expr *E) override {
      checkInvalidPartialApplication(E);
      return E;
    }

    void checkConvertedPointerArgument(ConcreteDeclRef callee,
                                       unsigned uncurryLevel,
                                       unsigned argIndex,
                                       Expr *pointerExpr,
                                       Expr *storage) {
      if (!isPointerIdentityArgument(callee, uncurryLevel, argIndex))
        return;

      // Flag that the argument is non-accessing.
      if (auto inout = dyn_cast<InOutToPointerExpr>(pointerExpr)) {
        inout->setNonAccessing(true);
      } else if (auto array = dyn_cast<ArrayToPointerExpr>(pointerExpr)) {
        array->setNonAccessing(true);
      }

      // TODO: warn if taking the address of 'storage' will definitely
      // yield a temporary address.
    }

    /// Is the given call argument, known to be of pointer type, just used
    /// for its pointer identity?
    bool isPointerIdentityArgument(ConcreteDeclRef ref, unsigned uncurryLevel,
                                   unsigned argIndex) {
      // FIXME: derive this from an attribute instead of hacking it based
      // on the target name!
      auto decl = ref.getDecl();

      // Assume that == and != are non-accessing uses.
      if (decl->isOperator()) {
        auto op = decl->getBaseName();
        if (op == "==" || op == "!=")
          return true;
        return false;
      }

      // NSObject.addObserver(_:forKeyPath:options:context:)
      if (uncurryLevel == 1 && argIndex == 3) {
        return decl->getFullName().isCompoundName("addObserver",
                                                  { "", "forKeyPath",
                                                    "options", "context" });
      }

      // NSObject.removeObserver(_:forKeyPath:context:)
      if (uncurryLevel == 1 && argIndex == 2) {
        return decl->getFullName().isCompoundName("removeObserver",
                                                  { "", "forKeyPath",
                                                    "context" });
      }

      return false;
    }

    /// We have a collection literal with a defaulted type, e.g. of [Any].  Emit
    /// an error if it was inferred to this type in an invalid context, which is
    /// one in which the parent expression is not itself a collection literal.
    void checkTypeDefaultedCollectionExpr(CollectionExpr *c) {
      // If the parent is a non-expression, or is not itself a literal, then
      // produce an error with a fixit to add the type as an explicit
      // annotation.
      if (c->getNumElements() == 0)
        TC.diagnose(c->getLoc(), diag::collection_literal_empty)
          .highlight(c->getSourceRange());
      else {
        TC.diagnose(c->getLoc(), diag::collection_literal_heterogeneous,
                    c->getType())
          .highlight(c->getSourceRange())
          .fixItInsertAfter(c->getEndLoc(), " as " + c->getType()->getString());
      }
    }


    /// Scout out the specified destination of an AssignExpr to recursively
    /// identify DiscardAssignmentExpr in legal places.  We can only allow them
    /// in simple pattern-like expressions, so we reject anything complex here.
    void markAcceptableDiscardExprs(Expr *E) {
      if (!E) return;
      
      if (auto *PE = dyn_cast<ParenExpr>(E))
        return markAcceptableDiscardExprs(PE->getSubExpr());
      if (auto *TE = dyn_cast<TupleExpr>(E)) {
        for (auto &elt : TE->getElements())
          markAcceptableDiscardExprs(elt);
        return;
      }
      if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(E))
        CorrectDiscardAssignmentExprs.insert(DAE);

      // Otherwise, we can't support this.
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

    class NoEscapeArgument {
      llvm::PointerIntPair<ParamDecl*, 1, bool> ParamAndIsCapture;
    public:
      NoEscapeArgument() {}
      NoEscapeArgument(ParamDecl *param, bool isCapture)
          : ParamAndIsCapture(param, isCapture) {
        assert(param);
      }

      explicit operator bool() const {
        return ParamAndIsCapture.getPointer() != nullptr;
      }

      ParamDecl *getDecl() const { return ParamAndIsCapture.getPointer(); }
      bool isDeclACapture() const { return ParamAndIsCapture.getInt(); }

      static NoEscapeArgument find(TypeChecker &tc, ValueDecl *decl,
                                   bool isCapture) {
        if (auto param = dyn_cast<ParamDecl>(decl)) {
          if (auto fnType =
                param->getInterfaceType()->getAs<AnyFunctionType>()) {
            if (fnType->isNoEscape())
              return { param, isCapture };
          }
          return {};
        }

        if (auto fn = dyn_cast<AbstractFunctionDecl>(decl)) {
          if (fn->getDeclContext()->isLocalContext()) {
            return findInCaptures(tc, fn);
          }
          return {};
        }

        // FIXME: captures of computed local vars?  Can these be non-escaping?
        return {};
      }

      static NoEscapeArgument findInCaptures(TypeChecker &tc,
                                             AnyFunctionRef fn) {
        // Ensure we have accurate capture information for the function.
        tc.computeCaptures(fn);

        for (const auto &capture : fn.getCaptureInfo().getCaptures()) {
          if (capture.isDynamicSelfMetadata()) continue;
          if (auto param = find(tc, capture.getDecl(), true))
            return param;
        }
        return {};
      }
    };

    /// Enforce the exclusivity rule against calling a non-escaping
    /// function parameter with another non-escaping function parameter
    /// as an argument.
    void checkNoEscapeParameterCall(ApplyExpr *apply) {
      NoEscapeArgument noescapeArgument;
      Expr *problematicArg = nullptr;

      visitArguments(apply, [&](unsigned argIndex, Expr *arg) {
        // Just find the first problematic argument.
        if (noescapeArgument) return;

        // Remember the expression which used the argument.
        problematicArg = arg;

        // Look through the same set of nodes that we look through when
        // checking for no-escape functions.
        arg = lookThroughArgument(arg);

        // If the argument isn't noescape, ignore it.
        auto fnType = arg->getType()->getAs<AnyFunctionType>();
        if (!fnType || !fnType->isNoEscape())
          return;

        // Okay, it should be a closure or a decl ref.
        if (auto declRef = dyn_cast<DeclRefExpr>(arg)) {
          noescapeArgument =
            NoEscapeArgument::find(TC, declRef->getDecl(), false);
        } else if (auto closure = dyn_cast<AbstractClosureExpr>(arg)) {
          noescapeArgument =
            NoEscapeArgument::findInCaptures(TC, closure);
        } else {
          // This can happen with withoutActuallyEscaping.
          assert(isa<OpaqueValueExpr>(arg) &&
                 "unexpected expression yielding noescape closure");
        }
      });

      if (!noescapeArgument) return;

      TC.diagnose(apply->getLoc(),
                  diag::err_noescape_param_call,
                  noescapeArgument.getDecl()->getName(),
                  noescapeArgument.isDeclACapture())
        .highlight(problematicArg->getSourceRange());
    }

    enum class OperandKind {
      None,
      Callee,
      Argument,
      MakeEscapable,
    };

    /// The DRE argument is a reference to a noescape parameter.  Verify that
    /// its uses are ok.
    void checkNoEscapeParameterUse(DeclRefExpr *DRE, Expr *parent,
                                   OperandKind useKind) {
      // This only cares about declarations of noescape function type.
      auto AFT = DRE->getDecl()->getInterfaceType()->getAs<AnyFunctionType>();
      if (!AFT || !AFT->isNoEscape())
        return;

      // Only diagnose this once.  If we check and accept this use higher up in
      // the AST, don't recheck here.
      if (!AlreadyDiagnosedNoEscapes.insert(DRE).second)
        return;

      // The only valid use of the noescape parameter is an immediate call,
      // either as the callee or as an argument (in which case, the typechecker
      // validates that the noescape bit didn't get stripped off), or as
      // a special case, e.g. in the binding of a withoutActuallyEscaping block
      // or the argument of a type(of: ...).
      if (parent) {
        if (auto apply = dyn_cast<ApplyExpr>(parent)) {
          if (isa<ParamDecl>(DRE->getDecl()) && useKind == OperandKind::Callee)
            checkNoEscapeParameterCall(apply);
          return;
        } else if (isa<SubscriptExpr>(parent)
                   && useKind == OperandKind::Argument) {
          return;
        } else if (isa<MakeTemporarilyEscapableExpr>(parent)) {
          return;
        } else if (isa<DynamicTypeExpr>(parent)) {
          return;
        }
      }

      TC.diagnose(DRE->getStartLoc(), diag::invalid_noescape_use,
                  cast<VarDecl>(DRE->getDecl())->getName(),
                  isa<ParamDecl>(DRE->getDecl()));

      // If we're a parameter, emit a helpful fixit to add @escaping
      auto paramDecl = dyn_cast<ParamDecl>(DRE->getDecl());
      if (paramDecl) {
        TC.diagnose(paramDecl->getStartLoc(), diag::noescape_parameter,
                    paramDecl->getName())
            .fixItInsert(paramDecl->getTypeLoc().getSourceRange().Start,
                         "@escaping ");
      }
    }

    // Diagnose metatype values that don't appear as part of a property,
    // method, or constructor reference.
    void checkUseOfMetaTypeName(Expr *E) {
      // If we've already checked this at a higher level, we're done.
      if (!AlreadyDiagnosedMetatypes.insert(E).second)
        return;

      // Allow references to types as a part of:
      // - member references T.foo, T.Type, T.self, etc.
      // - constructor calls T()
      if (auto *ParentExpr = Parent.getAsExpr()) {
        // This is an exhaustive list of the accepted syntactic forms.
        if (isa<ErrorExpr>(ParentExpr) ||
            isa<DotSelfExpr>(ParentExpr) ||               // T.self
            isa<CallExpr>(ParentExpr) ||                  // T()
            isa<MemberRefExpr>(ParentExpr) ||             // T.foo
            isa<UnresolvedMemberExpr>(ParentExpr) ||
            isa<SelfApplyExpr>(ParentExpr) ||             // T.foo()  T()
            isa<UnresolvedDotExpr>(ParentExpr) ||
            isa<DotSyntaxBaseIgnoredExpr>(ParentExpr) ||
            isa<UnresolvedSpecializeExpr>(ParentExpr) ||
            isa<OpenExistentialExpr>(ParentExpr)) {
          return;
        }
      }

      // Is this a protocol metatype?

      TC.diagnose(E->getStartLoc(), diag::value_of_metatype_type);

      // Add fix-it to insert '()', only if this is a metatype of
      // non-existential type and has any initializers.
      bool isExistential = false;
      if (auto metaTy = E->getType()->getAs<MetatypeType>()) {
        auto instanceTy = metaTy->getInstanceType();
        isExistential = instanceTy->isExistentialType();
        if (!isExistential &&
            instanceTy->mayHaveMembers() &&
            !TC.lookupConstructors(const_cast<DeclContext *>(DC),
                                   instanceTy).empty()) {
          TC.diagnose(E->getEndLoc(), diag::add_parens_to_type)
            .fixItInsertAfter(E->getEndLoc(), "()");
        }
      }

      // Add fix-it to insert ".self".
      auto diag = TC.diagnose(E->getEndLoc(), diag::add_self_to_type);
      if (E->canAppendPostfixExpression()) {
        diag.fixItInsertAfter(E->getEndLoc(), ".self");
      } else {
        diag.fixItInsert(E->getStartLoc(), "(");
        diag.fixItInsertAfter(E->getEndLoc(), ").self");
      }
    }

    void checkUnqualifiedAccessUse(const DeclRefExpr *DRE) {
      const Decl *D = DRE->getDecl();
      if (!D->getAttrs().hasAttribute<WarnUnqualifiedAccessAttr>())
        return;

      if (auto *parentExpr = Parent.getAsExpr()) {
        if (auto *ignoredBase = dyn_cast<DotSyntaxBaseIgnoredExpr>(parentExpr)){
          if (!ignoredBase->isImplicit())
            return;
        }
        if (auto *calledBase = dyn_cast<DotSyntaxCallExpr>(parentExpr)) {
          if (!calledBase->isImplicit())
            return;
        }
      }

      const auto *VD = cast<ValueDecl>(D);
      const TypeDecl *declParent =
          VD->getDeclContext()->getSelfNominalTypeDecl();
      if (!declParent) {
        // If the declaration has been validated but not fully type-checked,
        // the attribute might be applied to something invalid.
        if (!VD->getDeclContext()->isModuleScopeContext())
          return;
        declParent = VD->getDeclContext()->getParentModule();
      }

      TC.diagnose(DRE->getLoc(), diag::warn_unqualified_access,
                  VD->getBaseName().getIdentifier(), VD->getDescriptiveKind(),
                  declParent->getDescriptiveKind(), declParent->getFullName());
      TC.diagnose(VD, diag::decl_declared_here, VD->getFullName());

      if (VD->getDeclContext()->isTypeContext()) {
        TC.diagnose(DRE->getLoc(), diag::fix_unqualified_access_member)
          .fixItInsert(DRE->getStartLoc(), "self.");
      }

      DeclContext *topLevelContext = DC->getModuleScopeContext();
      UnqualifiedLookup lookup(VD->getBaseName(), topLevelContext, &TC,
                               /*Loc=*/SourceLoc(),
                               UnqualifiedLookup::Flags::KnownPrivate);

      // Group results by module. Pick an arbitrary result from each module.
      llvm::SmallDenseMap<const ModuleDecl*,const ValueDecl*,4> resultsByModule;
      for (auto &result : lookup.Results) {
        const ValueDecl *value = result.getValueDecl();
        resultsByModule.insert(std::make_pair(value->getModuleContext(),value));
      }

      // Sort by module name.
      using ModuleValuePair = std::pair<const ModuleDecl *, const ValueDecl *>;
      SmallVector<ModuleValuePair, 4> sortedResults{
        resultsByModule.begin(), resultsByModule.end()
      };
      llvm::array_pod_sort(sortedResults.begin(), sortedResults.end(),
                           [](const ModuleValuePair *lhs,
                              const ModuleValuePair *rhs) {
        return lhs->first->getName().compare(rhs->first->getName());
      });

      auto topLevelDiag = diag::fix_unqualified_access_top_level;
      if (sortedResults.size() > 1)
        topLevelDiag = diag::fix_unqualified_access_top_level_multi;

      for (const ModuleValuePair &pair : sortedResults) {
        DescriptiveDeclKind k = pair.second->getDescriptiveKind();

        SmallString<32> namePlusDot = pair.first->getName().str();
        namePlusDot.push_back('.');

        TC.diagnose(DRE->getLoc(), topLevelDiag,
                    namePlusDot, k, pair.first->getName())
          .fixItInsert(DRE->getStartLoc(), namePlusDot);
      }
    }
    
    void checkForDeclWithSpecialTypeCheckingSemantics(const DeclRefExpr *DRE) {
      // Referencing type(of:) and other decls with special type-checking
      // behavior as functions is not implemented. Maybe we could wrap up the
      // special-case behavior in a closure someday...
      if (TC.getDeclTypeCheckingSemantics(DRE->getDecl())
            != DeclTypeCheckingSemantics::Normal) {
        TC.diagnose(DRE->getLoc(), diag::unsupported_special_decl_ref,
                    DRE->getDecl()->getBaseName().getIdentifier());
      }
    }
    
    enum BitcastableNumberKind {
      BNK_None = 0,
      BNK_Int8,
      BNK_Int16,
      BNK_Int32,
      BNK_Int64,
      BNK_Int,
      BNK_UInt8,
      BNK_UInt16,
      BNK_UInt32,
      BNK_UInt64,
      BNK_UInt,
      BNK_Float,
      BNK_Double,
    };
    BitcastableNumberKind getBitcastableNumberKind(Type t) const {
      auto decl = t->getNominalOrBoundGenericNominal();
#define MATCH_DECL(type) \
      if (decl == TC.Context.get##type##Decl()) \
        return BNK_##type;
      MATCH_DECL(Int8)
      MATCH_DECL(Int16)
      MATCH_DECL(Int32)
      MATCH_DECL(Int64)
      MATCH_DECL(Int)
      MATCH_DECL(UInt8)
      MATCH_DECL(UInt16)
      MATCH_DECL(UInt32)
      MATCH_DECL(UInt64)
      MATCH_DECL(UInt)
      MATCH_DECL(Float)
      MATCH_DECL(Double)
#undef MATCH_DECL
      
      return BNK_None;
    }
    
    static constexpr unsigned BNKPair(BitcastableNumberKind a,
                                      BitcastableNumberKind b) {
      return (a << 8) | b;
    }
    
    void checkForSuspiciousBitCasts(DeclRefExpr *DRE,
                                    Expr *Parent = nullptr) {
      if (DRE->getDecl() != TC.Context.getUnsafeBitCast(&TC))
        return;
      
      if (DRE->getDeclRef().getSubstitutions().empty())
        return;
      
      // Don't check the same use of unsafeBitCast twice.
      if (!AlreadyDiagnosedBitCasts.insert(DRE).second)
        return;

      auto subMap = DRE->getDeclRef().getSubstitutions();
      auto fromTy =
        Type(GenericTypeParamType::get(0, 0, TC.Context)).subst(subMap);
      auto toTy =
        Type(GenericTypeParamType::get(0, 1, TC.Context)).subst(subMap);

      // Warn about `unsafeBitCast` formulations that are undefined behavior
      // or have better-defined alternative APIs that can be used instead.
      
      // If we have a parent ApplyExpr that calls bitcast, extract the argument
      // for fixits.
      Expr *subExpr = nullptr;
      CharSourceRange removeBeforeRange, removeAfterRange;
      if (auto apply = dyn_cast_or_null<ApplyExpr>(Parent)) {
        if (auto args = dyn_cast<TupleExpr>(apply->getArg())) {
          subExpr = args->getElement(0);
          // Determine the fixit range from the start of the application to
          // the first argument, `unsafeBitCast(`
          removeBeforeRange = CharSourceRange(TC.Context.SourceMgr,
                                              DRE->getLoc(),
                                              subExpr->getStartLoc());
          // Determine the fixit range from the end of the first argument to
          // the end of the application, `, to: T.self)`
          removeAfterRange = CharSourceRange(TC.Context.SourceMgr,
                         Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                                                    subExpr->getEndLoc()),
                         Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                                                    apply->getEndLoc()));          
        }
      }
  
      // Casting to the same type or a superclass is a no-op.
      if (toTy->isEqual(fromTy) ||
          toTy->isExactSuperclassOf(fromTy)) {
        auto d = TC.diagnose(DRE->getLoc(), diag::bitcasting_is_no_op,
                             fromTy, toTy);
        if (subExpr) {
          d.fixItRemoveChars(removeBeforeRange.getStart(),
                             removeBeforeRange.getEnd())
           .fixItRemoveChars(removeAfterRange.getStart(),
                             removeAfterRange.getEnd());
        }
        return;
      }
      
     if (auto fromFnTy = fromTy->getAs<FunctionType>()) {
        if (auto toFnTy = toTy->getAs<FunctionType>()) {
          // Casting a nonescaping function to escaping is UB.
          // `withoutActuallyEscaping` ought to be used instead.
          if (fromFnTy->isNoEscape() && !toFnTy->isNoEscape()) {
            TC.diagnose(DRE->getLoc(), diag::bitcasting_away_noescape,
                        fromTy, toTy);
          }
          // Changing function representation (say, to try to force a
          // @convention(c) function pointer to exist) is also unlikely to work.
          if (fromFnTy->getRepresentation() != toFnTy->getRepresentation()) {
            TC.diagnose(DRE->getLoc(), diag::bitcasting_to_change_function_rep,
                        fromTy, toTy);
          }
          return;
        }
      }
      
      // Unchecked casting to a subclass is better done by unsafeDowncast.
      if (fromTy->isBindableToSuperclassOf(toTy)) {
        TC.diagnose(DRE->getLoc(), diag::bitcasting_to_downcast,
                    fromTy, toTy)
          .fixItReplace(DRE->getNameLoc().getBaseNameLoc(),
                        "unsafeDowncast");
        return;
      }

      // Casting among pointer types should use the Unsafe*Pointer APIs for
      // rebinding typed memory or accessing raw memory instead.
      PointerTypeKind fromPTK, toPTK;
      Type fromPointee = fromTy->getAnyPointerElementType(fromPTK);
      Type toPointee = toTy->getAnyPointerElementType(toPTK);
      if (fromPointee && toPointee) {
        // Casting to a pointer to the same type or UnsafeRawPointer can use
        // normal initializers on the destination type.
        if (toPointee->isEqual(fromPointee)
            || isRawPointerKind(toPTK)) {
          auto d = TC.diagnose(DRE->getLoc(),
                         diag::bitcasting_to_change_pointer_kind,
                         fromTy, toTy,
                         toTy->getStructOrBoundGenericStruct()->getName());
          if (subExpr) {
            StringRef before, after;
            switch (toPTK) {
            case PTK_UnsafePointer:
              // UnsafePointer(mutablePointer)
              before = "UnsafePointer(";
              after = ")";
              break;
            case PTK_UnsafeMutablePointer:
            case PTK_AutoreleasingUnsafeMutablePointer:
              before = "UnsafeMutablePointer(mutating: ";
              after = ")";
              break;
              
            case PTK_UnsafeRawPointer:
              // UnsafeRawPointer(pointer)
              before = "UnsafeRawPointer(";
              after = ")";
              break;
              
            case PTK_UnsafeMutableRawPointer:
              // UnsafeMutableRawPointer(mutating: rawPointer)
              before = fromPTK == PTK_UnsafeMutablePointer
                ? "UnsafeMutableRawPointer("
                : "UnsafeMutableRawPointer(mutating: ";
              after = ")";
              break;
            }
            d.fixItReplaceChars(removeBeforeRange.getStart(),
                                removeBeforeRange.getEnd(),
                                before)
             .fixItReplaceChars(removeAfterRange.getStart(),
                                removeAfterRange.getEnd(),
                                after);
          }
          return;
        }
        
        // Casting to a different typed pointer type should use
        // withMemoryRebound.
        if (!isRawPointerKind(fromPTK) && !isRawPointerKind(toPTK)) {
          TC.diagnose(DRE->getLoc(),
                      diag::bitcasting_to_change_pointee_type,
                      fromTy, toTy);
          return;
        }
        
        // Casting a raw pointer to a typed pointer should bind the memory
        // (or assume it's already bound).
        assert(isRawPointerKind(fromPTK) && !isRawPointerKind(toPTK)
               && "unhandled cast combo?!");
        TC.diagnose(DRE->getLoc(),
                    diag::bitcasting_to_give_type_to_raw_pointer,
                    fromTy, toTy);
        if (subExpr) {
          SmallString<64> fixitBuf;
          {
            llvm::raw_svector_ostream os(fixitBuf);
            os << ".assumingMemoryBound(to: ";
            toPointee->print(os);
            os << ".self)";
          }
          TC.diagnose(DRE->getLoc(),
                      diag::bitcast_assume_memory_rebound,
                      toPointee)
            .fixItRemoveChars(removeBeforeRange.getStart(),
                              removeBeforeRange.getEnd())
            .fixItReplaceChars(removeAfterRange.getStart(),
                               removeAfterRange.getEnd(),
                               fixitBuf);
          fixitBuf.clear();
          {
            llvm::raw_svector_ostream os(fixitBuf);
            os << ".bindMemory(to: ";
            toPointee->print(os);
            os << ".self, capacity: <""#capacity#"">)";
          }
          TC.diagnose(DRE->getLoc(),
                      diag::bitcast_bind_memory,
                      toPointee)
            .fixItRemoveChars(removeBeforeRange.getStart(),
                              removeBeforeRange.getEnd())
            .fixItReplaceChars(removeAfterRange.getStart(),
                               removeAfterRange.getEnd(),
                               fixitBuf);
        }
        return;
      }
      
      StringRef replaceBefore, replaceAfter;
      Optional<Diag<Type, Type>> diagID;
      SmallString<64> replaceBeforeBuf;

      // Bitcasting among numeric types should use `bitPattern:` initializers.
      auto fromBNK = getBitcastableNumberKind(fromTy);
      auto toBNK = getBitcastableNumberKind(toTy);
      if (fromBNK && toBNK) {
        switch (BNKPair(fromBNK, toBNK)) {
        // Combos that can be bitPattern-ed with a constructor
        case BNKPair(BNK_Int8, BNK_UInt8):
        case BNKPair(BNK_UInt8, BNK_Int8):
        case BNKPair(BNK_Int16, BNK_UInt16):
        case BNKPair(BNK_UInt16, BNK_Int16):
        case BNKPair(BNK_Int32, BNK_UInt32):
        case BNKPair(BNK_UInt32, BNK_Int32):
        case BNKPair(BNK_Int64, BNK_UInt64):
        case BNKPair(BNK_UInt64, BNK_Int64):
        case BNKPair(BNK_Int, BNK_UInt):
        case BNKPair(BNK_UInt, BNK_Int):
        case BNKPair(BNK_UInt32, BNK_Float):
        case BNKPair(BNK_UInt64, BNK_Double):
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ")";
          break;
        
        // Combos that can be bitPattern-ed with a constructor and sign flip
        case BNKPair(BNK_Int32, BNK_Float):
        case BNKPair(BNK_Int64, BNK_Double):
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
            if (fromBNK == BNK_Int32)
              os << "UInt32(bitPattern: ";
            else
              os << "UInt64(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = "))";
          break;
        
        // Combos that can be bitPattern-ed with a property
        case BNKPair(BNK_Float, BNK_UInt32):
        case BNKPair(BNK_Double, BNK_UInt64):
          diagID = diag::bitcasting_for_number_bit_pattern_property;
          replaceAfter = ".bitPattern";
          break;
        
        // Combos that can be bitPattern-ed with a property and sign flip
        case BNKPair(BNK_Float, BNK_Int32):
        case BNKPair(BNK_Double, BNK_Int64):
          diagID = diag::bitcasting_for_number_bit_pattern_property;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ")";
          break;

        // Combos that can be bitPattern-ed with a constructor once (U)Int is
        // converted to a sized type.
        case BNKPair(BNK_UInt, BNK_Float):
        case BNKPair(BNK_Int, BNK_UInt32):
        case BNKPair(BNK_UInt, BNK_Int32):
        case BNKPair(BNK_Int, BNK_UInt64):
        case BNKPair(BNK_UInt, BNK_Int64):
        case BNKPair(BNK_UInt, BNK_Double):
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
            
            if (fromBNK == BNK_Int)
              os << "Int";
            else
              os << "UInt";
            
            if (toBNK == BNK_Float
                || toBNK == BNK_Int32
                || toBNK == BNK_UInt32)
              os << "32(";
            else
              os << "64(";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = "))";
          break;

        case BNKPair(BNK_Int, BNK_Float):
        case BNKPair(BNK_Int, BNK_Double):
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: UInt";
            
            if (toBNK == BNK_Float
                || toBNK == BNK_Int32
                || toBNK == BNK_UInt32)
              os << "32(bitPattern: Int32(";
            else
              os << "64(bitPattern: Int64(";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ")))";
          break;
    
        // Combos that can be bitPattern-ed then converted from a sized type
        // to (U)Int.
        case BNKPair(BNK_Int32, BNK_UInt):
        case BNKPair(BNK_UInt32, BNK_Int):
        case BNKPair(BNK_Int64, BNK_UInt):
        case BNKPair(BNK_UInt64, BNK_Int):
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(";
            if (toBNK == BNK_UInt)
              os << "UInt";
            else
              os << "Int";
            if (fromBNK == BNK_Int32 || fromBNK == BNK_UInt32)
              os << "32(bitPattern: ";
            else
              os << "64(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = "))";
          break;
        
        case BNKPair(BNK_Float, BNK_UInt):
        case BNKPair(BNK_Double, BNK_UInt):
          diagID = diag::bitcasting_for_number_bit_pattern_property;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ".bitPattern)";
          break;
          
        case BNKPair(BNK_Float, BNK_Int):
        case BNKPair(BNK_Double, BNK_Int):
          diagID = diag::bitcasting_for_number_bit_pattern_property;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: UInt(";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ".bitPattern))";
          break;
        
        // Combos that should be done with a value-preserving initializer.
        case BNKPair(BNK_Int, BNK_Int32):
        case BNKPair(BNK_Int, BNK_Int64):
        case BNKPair(BNK_UInt, BNK_UInt32):
        case BNKPair(BNK_UInt, BNK_UInt64):
        case BNKPair(BNK_Int32, BNK_Int):
        case BNKPair(BNK_Int64, BNK_Int):
        case BNKPair(BNK_UInt32, BNK_UInt):
        case BNKPair(BNK_UInt64, BNK_UInt):
          diagID = diag::bitcasting_to_change_from_unsized_to_sized_int;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << '(';
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ")";
          break;
        
        default:
          // Leave other combos alone.
          break;
        }
      }
      
      // Casting a pointer to an int or back should also use bitPattern
      // initializers.
      if (fromPointee && toBNK) {
        switch (toBNK) {
        case BNK_UInt:
        case BNK_Int:
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ")";
          break;
          
        case BNK_UInt64:
        case BNK_UInt32:
        case BNK_Int64:
        case BNK_Int32:
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << '(';
            if (toBNK == BNK_UInt32 || toBNK == BNK_UInt64)
              os << "UInt(bitPattern: ";
            else
              os << "Int(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = "))";
          break;
        
        default:
          break;
        }
      }
      if (fromBNK && toPointee) {
        switch (fromBNK) {
        case BNK_UInt:
        case BNK_Int:
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = ")";
          break;
          
        case BNK_UInt64:
        case BNK_UInt32:
        case BNK_Int64:
        case BNK_Int32:
          diagID = diag::bitcasting_for_number_bit_pattern_init;
          {
            llvm::raw_svector_ostream os(replaceBeforeBuf);
            toTy->print(os);
            os << "(bitPattern: ";
            if (fromBNK == BNK_Int32 || fromBNK == BNK_Int64)
              os << "Int(";
            else
              os << "UInt(";
          }
          replaceBefore = replaceBeforeBuf;
          replaceAfter = "))";
          break;

        default:
          break;
        }
      }
      
      if (diagID) {
        auto d = TC.diagnose(DRE->getLoc(), *diagID, fromTy, toTy);
        if (subExpr) {
          d.fixItReplaceChars(removeBeforeRange.getStart(),
                              removeBeforeRange.getEnd(),
                              replaceBefore);
          d.fixItReplaceChars(removeAfterRange.getStart(),
                              removeAfterRange.getEnd(),
                              replaceAfter);
        }
      }

    }
    
    /// Return true if this is a 'nil' literal.  This looks
    /// like this if the type is Optional<T>:
    ///
    ///   (dot_syntax_call_expr implicit type='Int?'
    ///     (declref_expr implicit decl=Optional.none)
    ///     (type_expr type=Int?))
    ///
    /// Or like this if it is any other ExpressibleByNilLiteral type:
    ///
    ///   (dot_syntax_call_expr implicit type='Int?'
    ///     (declref_expr implicit decl=Optional.none)
    ///     (type_expr type=Int?))
    ///
    bool isTypeCheckedOptionalNil(Expr *E) {
      auto CE = dyn_cast<ApplyExpr>(E->getSemanticsProvidingExpr());
      if (!CE || !CE->isImplicit())
        return false;

      // First case -- Optional.none
      if (auto DRE = dyn_cast<DeclRefExpr>(CE->getSemanticFn()))
        return DRE->getDecl() == TC.Context.getOptionalNoneDecl();

      // Second case -- init(nilLiteral:)
      auto CRCE = dyn_cast<ConstructorRefCallExpr>(CE->getSemanticFn());
      if (!CRCE || !CRCE->isImplicit()) return false;

      if (auto DRE = dyn_cast<DeclRefExpr>(CRCE->getSemanticFn())) {
        SmallString<32> NameBuffer;
        auto name = DRE->getDecl()->getFullName().getString(NameBuffer);
        return name == "init(nilLiteral:)";
      }

      return false;
    }


    /// Warn about surprising implicit optional promotions involving operands to
    /// calls.  Specifically, we warn about these expressions when the 'x'
    /// operand is implicitly promoted to optional:
    ///
    ///       x ?? y
    ///       x == nil    // also !=
    ///
    void checkOptionalPromotions(ApplyExpr *call) {
      // We only care about binary expressions.
      if (!isa<BinaryExpr>(call)) return;

      // Dig out the function we're calling.
      auto fnExpr = call->getSemanticFn();
      if (auto dotSyntax = dyn_cast<DotSyntaxCallExpr>(fnExpr))
        fnExpr = dotSyntax->getSemanticFn();

      auto DRE = dyn_cast<DeclRefExpr>(fnExpr);
      auto args = dyn_cast<TupleExpr>(call->getArg());
      if (!DRE || !DRE->getDecl()->isOperator() ||
          !args || args->getNumElements() != 2)
        return;
      
      auto lhs = args->getElement(0);
      auto rhs = args->getElement(1);
      auto calleeName = DRE->getDecl()->getBaseName();

      Expr *subExpr = nullptr;
      if (calleeName == "??" &&
          (subExpr = isImplicitPromotionToOptional(lhs))) {
        TC.diagnose(DRE->getLoc(), diag::use_of_qq_on_non_optional_value,
                    subExpr->getType())
          .highlight(lhs->getSourceRange())
          .fixItRemove(SourceRange(DRE->getLoc(), rhs->getEndLoc()));
        return;
      }
      
      if (calleeName == "==" || calleeName == "!=" ||
          calleeName == "===" || calleeName == "!==") {
        if (((subExpr = isImplicitPromotionToOptional(lhs)) &&
             isTypeCheckedOptionalNil(rhs)) ||
            (isTypeCheckedOptionalNil(lhs) &&
             (subExpr = isImplicitPromotionToOptional(rhs)))) {
          bool isTrue = calleeName == "!=" || calleeName == "!==";
              
          TC.diagnose(DRE->getLoc(), diag::nonoptional_compare_to_nil,
                      subExpr->getType(), isTrue)
            .highlight(lhs->getSourceRange())
            .highlight(rhs->getSourceRange());
          return;
        }
      }
    }
  };

  DiagnoseWalker Walker(TC, DC, isExprStmt);
  const_cast<Expr *>(E)->walk(Walker);

  // Diagnose uses of collection literals with defaulted types at the top
  // level.
  if (auto collection
        = dyn_cast<CollectionExpr>(E->getSemanticsProvidingExpr())) {
    if (collection->isTypeDefaulted()) {
      Walker.checkTypeDefaultedCollectionExpr(
        const_cast<CollectionExpr *>(collection));
    }
  }
}


/// Diagnose recursive use of properties within their own accessors
static void diagRecursivePropertyAccess(TypeChecker &TC, const Expr *E,
                                        const DeclContext *DC) {
  auto fn = dyn_cast<AccessorDecl>(DC);
  if (!fn)
    return;

  auto var = dyn_cast<VarDecl>(fn->getStorage());
  if (!var)  // Ignore subscripts
    return;

  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;
    VarDecl *Var;
    const AccessorDecl *Accessor;

  public:
    explicit DiagnoseWalker(TypeChecker &TC, VarDecl *var,
                            const AccessorDecl *Accessor)
      : TC(TC), Var(var), Accessor(Accessor) {}

    /// Return true if this is an implicit reference to self.
    static bool isImplicitSelfUse(Expr *E) {
      auto *DRE = dyn_cast<DeclRefExpr>(E);
      return DRE && DRE->isImplicit() && isa<VarDecl>(DRE->getDecl()) &&
             cast<VarDecl>(DRE->getDecl())->isSelfParameter();
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      Expr *subExpr;
      bool isStore = false;

      if (auto *AE = dyn_cast<AssignExpr>(E)) {
        subExpr = AE->getDest();
        
        // If we couldn't flatten this expression, don't explode.
        if (!subExpr)
          return { true, E };

        isStore = true;
      } else if (auto *IOE = dyn_cast<InOutExpr>(E)) {
        subExpr = IOE->getSubExpr();
        isStore = true;
      } else {
        subExpr = E;
      }

      if (auto *BOE = dyn_cast<BindOptionalExpr>(subExpr))
        subExpr = BOE;

      if (auto *DRE = dyn_cast<DeclRefExpr>(subExpr)) {
        if (DRE->getDecl() == Var) {
          // Handle local and top-level computed variables.
          if (DRE->getAccessSemantics() == AccessSemantics::Ordinary) {
            bool shouldDiagnose = false;
            // Warn about any property access in the getter.
            if (Accessor->isGetter())
              shouldDiagnose = !isStore;
            // Warn about stores in the setter, but allow loads.
            if (Accessor->isSetter())
              shouldDiagnose = isStore;

            // But silence the warning if the base was explicitly qualified.
            auto parentAsExpr = Parent.getAsExpr();
            if (parentAsExpr && isa<DotSyntaxBaseIgnoredExpr>(parentAsExpr))
              shouldDiagnose = false;

            if (shouldDiagnose) {
              TC.diagnose(subExpr->getLoc(), diag::recursive_accessor_reference,
                          Var->getName(), Accessor->isSetter());
            }
          }
          
          // If this is a direct store in a "willSet", we reject this because
          // it is about to get overwritten.
          if (isStore &&
              DRE->getAccessSemantics() == AccessSemantics::DirectToStorage &&
              Accessor->getAccessorKind() == AccessorKind::WillSet) {
            TC.diagnose(E->getLoc(), diag::store_in_willset, Var->getName());
          }
        }


      } else if (auto *MRE = dyn_cast<MemberRefExpr>(subExpr)) {
        // Handle instance and type computed variables.
        // Find MemberRefExprs that have an implicit "self" base.
        if (MRE->getMember().getDecl() == Var &&
            isa<DeclRefExpr>(MRE->getBase()) &&
            isImplicitSelfUse(MRE->getBase())) {
          
          if (MRE->getAccessSemantics() == AccessSemantics::Ordinary) {
            bool shouldDiagnose = false;
            // Warn about any property access in the getter.
            if (Accessor->isGetter())
              shouldDiagnose = !isStore;
            // Warn about stores in the setter, but allow loads.
            if (Accessor->isSetter())
              shouldDiagnose = isStore;

            if (shouldDiagnose) {
              TC.diagnose(subExpr->getLoc(), diag::recursive_accessor_reference,
                          Var->getName(), Accessor->isSetter());
              TC.diagnose(subExpr->getLoc(),
                          diag::recursive_accessor_reference_silence)
              .fixItInsert(subExpr->getStartLoc(), "self.");
            }
          }

          // If this is a direct store in a "willSet", we reject this because
          // it is about to get overwritten.
          if (isStore &&
              MRE->getAccessSemantics() == AccessSemantics::DirectToStorage &&
              Accessor->getAccessorKind() == AccessorKind::WillSet) {
              TC.diagnose(subExpr->getLoc(), diag::store_in_willset,
                          Var->getName());
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
      return DRE && DRE->isImplicit() && isa<VarDecl>(DRE->getDecl()) &&
             cast<VarDecl>(DRE->getDecl())->isSelfParameter() &&
             // Metatype self captures don't extend the lifetime of an object.
             !DRE->getType()->is<MetatypeType>();
    }

    /// Return true if this is a closure expression that will require "self."
    /// qualification of member references.
    static bool isClosureRequiringSelfQualification(
                  const AbstractClosureExpr *CE) {
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
                      MRE->getMember().getDecl()->getBaseName().getIdentifier())
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
                      MethodExpr->getDecl()->getBaseName().getIdentifier())
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

bool TypeChecker::getDefaultGenericArgumentsString(
    SmallVectorImpl<char> &buf,
    const swift::GenericTypeDecl *typeDecl,
    llvm::function_ref<Type(const GenericTypeParamDecl *)> getPreferredType) {
  llvm::raw_svector_ostream genericParamText{buf};
  genericParamText << "<";

  auto printGenericParamSummary =
      [&](GenericTypeParamType *genericParamTy) {
    const GenericTypeParamDecl *genericParam = genericParamTy->getDecl();
    if (Type result = getPreferredType(genericParam)) {
      result.print(genericParamText);
      return;
    }

    auto contextTy = typeDecl->mapTypeIntoContext(genericParamTy);
    if (auto archetypeTy = contextTy->getAs<ArchetypeType>()) {
      SmallVector<Type, 2> members;

      bool hasExplicitAnyObject = archetypeTy->requiresClass();
      if (auto superclass = archetypeTy->getSuperclass()) {
        hasExplicitAnyObject = false;
        members.push_back(superclass);
      }

      for (auto proto : archetypeTy->getConformsTo()) {
        members.push_back(proto->getDeclaredType());
        if (proto->requiresClass())
          hasExplicitAnyObject = false;
      }

      if (hasExplicitAnyObject)
        members.push_back(typeDecl->getASTContext().getAnyObjectType());

      auto type = ProtocolCompositionType::get(typeDecl->getASTContext(),
                                               members, hasExplicitAnyObject);

      if (type->isObjCExistentialType() || type->isAny()) {
        genericParamText << type;
        return;
      }

      genericParamText << "<#" << genericParam->getName() << ": ";
      genericParamText << type << "#>";
      return;
    }

    genericParamText << contextTy;
  };

  interleave(typeDecl->getInnermostGenericParamTypes(),
             printGenericParamSummary, [&]{ genericParamText << ", "; });

  genericParamText << ">";
  return true;
}

/// Diagnose an argument labeling issue, returning true if we successfully
/// diagnosed the issue.
bool swift::diagnoseArgumentLabelError(ASTContext &ctx,
                                       const Expr *expr,
                                       ArrayRef<Identifier> newNames,
                                       bool isSubscript,
                                       InFlightDiagnostic *existingDiag) {
  Optional<InFlightDiagnostic> diagOpt;
  auto getDiag = [&]() -> InFlightDiagnostic & {
    if (existingDiag)
      return *existingDiag;
    return *diagOpt;
  };

  auto &diags = ctx.Diags;
  auto tuple = dyn_cast<TupleExpr>(expr);
  if (!tuple) {
    if (newNames[0].empty()) {
      // We don't know what to do with this.
      return false;
    }

    llvm::SmallString<16> str;
    // If the diagnostic is local, flush it before returning.
    // This makes sure it's emitted before 'str' is destroyed.
    SWIFT_DEFER { diagOpt.reset(); };

    // This is a scalar-to-tuple conversion. Add the name. We "know"
    // that we're inside a ParenExpr, because ParenExprs are required
    // by the syntax and locator resolution looks through on level of
    // them.

    // Look through the paren expression, if there is one.
    if (auto parenExpr = dyn_cast<ParenExpr>(expr))
      expr = parenExpr->getSubExpr();

    str += newNames[0].str();
    str += ": ";
    if (!existingDiag) {
      diagOpt.emplace(diags.diagnose(expr->getStartLoc(),
                                     diag::missing_argument_labels,
                                     false, str.str().drop_back(),
                                     isSubscript));
    }
    getDiag().fixItInsert(expr->getStartLoc(), str);
    return true;
  }

  // Figure out how many extraneous, missing, and wrong labels are in
  // the call.
  unsigned numExtra = 0, numMissing = 0, numWrong = 0;
  unsigned n = std::max(tuple->getNumElements(), (unsigned)newNames.size());

  llvm::SmallString<16> missingBuffer;
  llvm::SmallString<16> extraBuffer;
  for (unsigned i = 0; i != n; ++i) {
    Identifier oldName;
    if (i < tuple->getNumElements())
      oldName = tuple->getElementName(i);
    Identifier newName;
    if (i < newNames.size())
      newName = newNames[i];

    if (oldName == newName ||
        (tuple->hasTrailingClosure() && i == tuple->getNumElements()-1))
      continue;

    if (oldName.empty()) {
      ++numMissing;
      missingBuffer += newName.str();
      missingBuffer += ":";
    } else if (newName.empty()) {
      ++numExtra;
      extraBuffer += oldName.str();
      extraBuffer += ':';
    } else
      ++numWrong;
  }

  // Emit the diagnostic.
  assert(numMissing > 0 || numExtra > 0 || numWrong > 0);
  llvm::SmallString<16> haveBuffer; // note: diagOpt has references to this
  llvm::SmallString<16> expectedBuffer; // note: diagOpt has references to this

  // If we had any wrong labels, or we have both missing and extra labels,
  // emit the catch-all "wrong labels" diagnostic.
  if (!existingDiag) {
    bool plural = (numMissing + numExtra + numWrong) > 1;
    if (numWrong > 0 || (numMissing > 0 && numExtra > 0)) {
      for (unsigned i = 0, n = tuple->getNumElements(); i != n; ++i) {
        auto haveName = tuple->getElementName(i);
        if (haveName.empty())
          haveBuffer += '_';
        else
          haveBuffer += haveName.str();
        haveBuffer += ':';
      }

      for (auto expected : newNames) {
        if (expected.empty())
          expectedBuffer += '_';
        else
          expectedBuffer += expected.str();
        expectedBuffer += ':';
      }

      StringRef haveStr = haveBuffer;
      StringRef expectedStr = expectedBuffer;
      diagOpt.emplace(diags.diagnose(expr->getLoc(),
                                     diag::wrong_argument_labels,
                                     plural, haveStr, expectedStr,
                                     isSubscript));
    } else if (numMissing > 0) {
      StringRef missingStr = missingBuffer;
      diagOpt.emplace(diags.diagnose(expr->getLoc(),
                                     diag::missing_argument_labels,
                                     plural, missingStr, isSubscript));
    } else {
      assert(numExtra > 0);
      StringRef extraStr = extraBuffer;
      diagOpt.emplace(diags.diagnose(expr->getLoc(),
                                     diag::extra_argument_labels,
                                     plural, extraStr, isSubscript));
    }
  }

  // Emit Fix-Its to correct the names.
  auto &diag = getDiag();
  for (unsigned i = 0, n = tuple->getNumElements(); i != n; ++i) {
    Identifier oldName = tuple->getElementName(i);
    Identifier newName;
    if (i < newNames.size())
      newName = newNames[i];

    if (oldName == newName || (i == n-1 && tuple->hasTrailingClosure()))
      continue;

    if (newName.empty()) {
      // Delete the old name.
      diag.fixItRemoveChars(tuple->getElementNameLocs()[i],
                            tuple->getElement(i)->getStartLoc());
      continue;
    }

    bool newNameIsReserved = !canBeArgumentLabel(newName.str());
    llvm::SmallString<16> newStr;
    if (newNameIsReserved)
      newStr += "`";
    newStr += newName.str();
    if (newNameIsReserved)
      newStr += "`";

    if (oldName.empty()) {
      // Insert the name.
      newStr += ": ";
      diag.fixItInsert(tuple->getElement(i)->getStartLoc(), newStr);
      continue;
    }

    // Change the name.
    diag.fixItReplace(tuple->getElementNameLocs()[i], newStr);
  }

  // If the diagnostic is local, flush it before returning.
  // This makes sure it's emitted before the message text buffers are destroyed.
  diagOpt.reset();
  return true;
}

static const Expr *lookThroughExprsToImmediateDeallocation(const Expr *E) {
  // Look through various expressions that don't affect the fact that the user
  // will be assigning a class instance that will be immediately deallocated.
  while (true) {
    E = E->getValueProvidingExpr();

    // We don't currently deal with tuple shuffles.
    if (isa<TupleShuffleExpr>(E))
      return E;

    // If we have a TupleElementExpr with a child TupleExpr, dig into that
    // element.
    if (auto *TEE = dyn_cast<TupleElementExpr>(E)) {
      auto *subExpr = lookThroughExprsToImmediateDeallocation(TEE->getBase());
      if (auto *TE = dyn_cast<TupleExpr>(subExpr)) {
        auto *element = TE->getElements()[TEE->getFieldNumber()];
        return lookThroughExprsToImmediateDeallocation(element);
      }
      return subExpr;
    }

    if (auto *ICE = dyn_cast<ImplicitConversionExpr>(E)) {
      E = ICE->getSubExpr();
      continue;
    }
    if (auto *CE = dyn_cast<CoerceExpr>(E)) {
      E = CE->getSubExpr();
      continue;
    }
    if (auto *OEE = dyn_cast<OpenExistentialExpr>(E)) {
      E = OEE->getSubExpr();
      continue;
    }

    // Look through optional evaluations, we still want to diagnose on
    // things like initializers called through optional chaining and the
    // unwrapping of failable initializers.
    if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(E)) {
      E = OEE->getSubExpr();
      continue;
    }
    if (auto *OBE = dyn_cast<BindOptionalExpr>(E)) {
      E = OBE->getSubExpr();
      continue;
    }
    if (auto *FOE = dyn_cast<ForceValueExpr>(E)) {
      E = FOE->getSubExpr();
      continue;
    }

    if (auto *ATE = dyn_cast<AnyTryExpr>(E)) {
      E = ATE->getSubExpr();
      continue;
    }
    if (auto *DSBIE = dyn_cast<DotSyntaxBaseIgnoredExpr>(E)) {
      E = DSBIE->getRHS();
      continue;
    }
    return E;
  }
}

static void diagnoseUnownedImmediateDeallocationImpl(TypeChecker &TC,
                                                     const VarDecl *varDecl,
                                                     const Expr *initExpr,
                                                     SourceLoc diagLoc,
                                                     SourceRange diagRange) {
  auto *ownershipAttr =
      varDecl->getAttrs().getAttribute<ReferenceOwnershipAttr>();
  if (!ownershipAttr || ownershipAttr->isInvalid())
    return;

  // Only diagnose for non-owning ownerships such as 'weak' and 'unowned'.
  // Zero is the default/strong ownership strength.
  if (ReferenceOwnership::Strong == ownershipAttr->get() ||
      isLessStrongThan(ReferenceOwnership::Strong, ownershipAttr->get()))
    return;

  // Try to find a call to a constructor.
  initExpr = lookThroughExprsToImmediateDeallocation(initExpr);
  auto *CE = dyn_cast<CallExpr>(initExpr);
  if (!CE)
    return;

  auto *CRCE = dyn_cast<ConstructorRefCallExpr>(CE->getFn());
  if (!CRCE)
    return;

  auto *DRE = dyn_cast<DeclRefExpr>(CRCE->getFn());
  if (!DRE)
    return;

  auto *constructorDecl = dyn_cast<ConstructorDecl>(DRE->getDecl());
  if (!constructorDecl)
    return;

  // Make sure the constructor constructs an instance that allows ownership.
  // This is to ensure we don't diagnose on constructors such as
  // Optional.init(nilLiteral:).
  auto selfType = constructorDecl->getDeclContext()->getSelfTypeInContext();
  if (!selfType->allowsOwnership())
    return;

  // This must stay in sync with
  // diag::unowned_assignment_immediate_deallocation.
  enum {
    SK_Variable = 0,
    SK_Property
  } storageKind = SK_Variable;

  if (varDecl->getDeclContext()->isTypeContext())
    storageKind = SK_Property;

  TC.diagnose(diagLoc, diag::unowned_assignment_immediate_deallocation,
              varDecl->getName(), ownershipAttr->get(), unsigned(storageKind))
    .highlight(diagRange);

  TC.diagnose(diagLoc, diag::unowned_assignment_requires_strong)
    .highlight(diagRange);

  TC.diagnose(varDecl, diag::decl_declared_here, varDecl->getFullName());
}

void swift::diagnoseUnownedImmediateDeallocation(TypeChecker &TC,
                                                 const AssignExpr *assignExpr) {
  auto *destExpr = assignExpr->getDest()->getValueProvidingExpr();
  auto *initExpr = assignExpr->getSrc();

  // Try to find a referenced VarDecl.
  const VarDecl *VD = nullptr;
  if (auto *DRE = dyn_cast<DeclRefExpr>(destExpr)) {
    VD = dyn_cast<VarDecl>(DRE->getDecl());
  } else if (auto *MRE = dyn_cast<MemberRefExpr>(destExpr)) {
    VD = dyn_cast<VarDecl>(MRE->getMember().getDecl());
  }

  if (VD)
    diagnoseUnownedImmediateDeallocationImpl(TC, VD, initExpr,
                                             assignExpr->getLoc(),
                                             initExpr->getSourceRange());
}

void swift::diagnoseUnownedImmediateDeallocation(TypeChecker &TC,
                                                 const Pattern *pattern,
                                                 SourceLoc equalLoc,
                                                 const Expr *initExpr) {
  pattern = pattern->getSemanticsProvidingPattern();

  if (auto *TP = dyn_cast<TuplePattern>(pattern)) {
    initExpr = lookThroughExprsToImmediateDeallocation(initExpr);

    // If we've found a matching tuple initializer with the same number of
    // elements as our pattern, diagnose each element individually.
    auto TE = dyn_cast<TupleExpr>(initExpr);
    if (TE && TE->getNumElements() == TP->getNumElements()) {
      for (unsigned i = 0, e = TP->getNumElements(); i != e; ++i) {
        const TuplePatternElt &elt = TP->getElement(i);
        const Pattern *subPattern = elt.getPattern();
        Expr *subInitExpr = TE->getElement(i);

        diagnoseUnownedImmediateDeallocation(TC, subPattern, equalLoc,
                                             subInitExpr);
      }
    }
  } else if (auto *NP = dyn_cast<NamedPattern>(pattern)) {
    diagnoseUnownedImmediateDeallocationImpl(TC, NP->getDecl(), initExpr,
                                             equalLoc,
                                             initExpr->getSourceRange());
  }
}

bool swift::fixItOverrideDeclarationTypes(InFlightDiagnostic &diag,
                                          ValueDecl *decl,
                                          const ValueDecl *base) {
  // For now, just rewrite cases where the base uses a value type and the
  // override uses a reference type, and the value type is bridged to the
  // reference type. This is a way to migrate code that makes use of types
  // that previously were not bridged to value types.
  auto checkValueReferenceType =
      [&](Type overrideTy, VarDecl::Specifier overrideSpec,
          Type baseTy, VarDecl::Specifier baseSpec,
          SourceRange typeRange) -> bool {
    if (typeRange.isInvalid())
      return false;

    auto normalizeType = [](Type &ty, VarDecl::Specifier spec) -> Type {
      Type normalizedTy = ty;
      if (Type unwrappedTy = normalizedTy->getOptionalObjectType())
        normalizedTy = unwrappedTy;
      if (spec == VarDecl::Specifier::InOut)
        ty = InOutType::get(ty);
      return normalizedTy;
    };

    // Is the base type bridged?
    Type normalizedBaseTy = normalizeType(baseTy, baseSpec);
    const DeclContext *DC = decl->getDeclContext();

    ASTContext &ctx = decl->getASTContext();

    // ...and just knowing that it's bridged isn't good enough if we don't
    // know what it's bridged /to/. Also, don't do this check for trivial
    // bridging---that doesn't count.
    Type bridged;
    if (normalizedBaseTy->isAny()) {
      bridged = ctx.getAnyObjectType();
    } else {
      bridged = ctx.getBridgedToObjC(DC, normalizedBaseTy);
    }
    if (!bridged || bridged->isEqual(normalizedBaseTy))
      return false;

    // ...and is it bridged to the overridden type?
    Type normalizedOverrideTy = normalizeType(overrideTy, overrideSpec);
    if (!bridged->isEqual(normalizedOverrideTy)) {
      // If both are nominal types, check again, ignoring generic arguments.
      auto *overrideNominal = normalizedOverrideTy->getAnyNominal();
      if (!overrideNominal || bridged->getAnyNominal() != overrideNominal) {
        return false;
      }
    }

    Type newOverrideTy = baseTy;

    // Preserve optionality if we're dealing with a simple type.
    if (Type unwrappedTy = newOverrideTy->getOptionalObjectType())
      newOverrideTy = unwrappedTy;
    if (overrideTy->getOptionalObjectType())
      newOverrideTy = OptionalType::get(newOverrideTy);

    SmallString<32> baseTypeBuf;
    llvm::raw_svector_ostream baseTypeStr(baseTypeBuf);
    PrintOptions options;
    options.SynthesizeSugarOnTypes = true;

    newOverrideTy->print(baseTypeStr, options);
    diag.fixItReplace(typeRange, baseTypeStr.str());
    return true;
  };

  // Check if overriding fails because we lack @escaping attribute on the function
  // type repr.
  auto checkTypeMissingEscaping = [&](Type overrideTy, Type baseTy,
                                      SourceRange typeRange) -> bool {
    // Fix-it needs position to apply.
    if (typeRange.isInvalid())
      return false;
    auto overrideFnTy = overrideTy->getAs<AnyFunctionType>();
    auto baseFnTy = baseTy->getAs<AnyFunctionType>();

    // Both types should be function.
    if (overrideFnTy && baseFnTy &&
        // The overriding function type should be no escaping.
        overrideFnTy->getExtInfo().isNoEscape() &&
        // The overridden function type should be escaping.
        !baseFnTy->getExtInfo().isNoEscape()) {
      diag.fixItInsert(typeRange.Start, "@escaping ");
      return true;
    }
    return false;
  };

  auto checkType = [&](Type overrideTy, VarDecl::Specifier overrideSpec,
                       Type baseTy, VarDecl::Specifier baseSpec,
                       SourceRange typeRange) -> bool {
    return checkValueReferenceType(overrideTy, overrideSpec,
                                   baseTy, baseSpec, typeRange) ||
      checkTypeMissingEscaping(overrideTy, baseTy, typeRange);
  };

  if (auto *var = dyn_cast<VarDecl>(decl)) {
    SourceRange typeRange = var->getTypeSourceRangeForDiagnostics();
    auto *baseVar = cast<VarDecl>(base);
    return checkType(var->getInterfaceType(), var->getSpecifier(),
                     baseVar->getInterfaceType(), var->getSpecifier(),
                     typeRange);
  }

  if (auto *fn = dyn_cast<AbstractFunctionDecl>(decl)) {
    auto *baseFn = cast<AbstractFunctionDecl>(base);
    bool fixedAny = false;
    if (fn->getParameters()->size() ==
        baseFn->getParameters()->size()) {
      for_each(*fn->getParameters(),
               *baseFn->getParameters(),
               [&](ParamDecl *param, const ParamDecl *baseParam) {
        fixedAny |= fixItOverrideDeclarationTypes(diag, param, baseParam);
      });
    }
    if (auto *method = dyn_cast<FuncDecl>(decl)) {
      auto resultType = method->mapTypeIntoContext(
          method->getResultInterfaceType());

      auto *baseMethod = cast<FuncDecl>(base);
      auto baseResultType = baseMethod->mapTypeIntoContext(
          baseMethod->getResultInterfaceType());

      fixedAny |= checkType(resultType, VarDecl::Specifier::Default,
                            baseResultType, VarDecl::Specifier::Default,
                            method->getBodyResultTypeLoc().getSourceRange());
    }
    return fixedAny;
  }

  if (auto *subscript = dyn_cast<SubscriptDecl>(decl)) {
    auto *baseSubscript = cast<SubscriptDecl>(base);
    bool fixedAny = false;
    if (subscript->getIndices()->size() ==
        baseSubscript->getIndices()->size()) {
      for_each(*subscript->getIndices(),
               *baseSubscript->getIndices(),
               [&](ParamDecl *param, const ParamDecl *baseParam) {
        fixedAny |= fixItOverrideDeclarationTypes(diag, param, baseParam);
      });
    }

    auto resultType = subscript->getDeclContext()->mapTypeIntoContext(
        subscript->getElementInterfaceType());
    auto baseResultType = baseSubscript->getDeclContext()->mapTypeIntoContext(
        baseSubscript->getElementInterfaceType());
    fixedAny |= checkType(resultType, VarDecl::Specifier::Default,
                          baseResultType, VarDecl::Specifier::Default,
                          subscript->getElementTypeLoc().getSourceRange());
    return fixedAny;
  }

  llvm_unreachable("unknown overridable member");
}

//===----------------------------------------------------------------------===//
// Per func/init diagnostics
//===----------------------------------------------------------------------===//

namespace {
class VarDeclUsageChecker : public ASTWalker {
  DiagnosticEngine &Diags;
  // Keep track of some information about a variable.
  enum {
    RK_Read        = 1,      ///< Whether it was ever read.
    RK_Written     = 2,      ///< Whether it was ever written or passed inout.
    
    RK_CaptureList = 4       ///< Var is an entry in a capture list.
  };
  
  /// These are all of the variables that we are tracking.  VarDecls get added
  /// to this when the declaration is seen.  We use a MapVector to keep the
  /// diagnostics emission in deterministic order.
  llvm::SmallMapVector<VarDecl*, unsigned, 32> VarDecls;

  /// This is a mapping from an OpaqueValue to the expression that initialized
  /// it.
  llvm::SmallDenseMap<OpaqueValueExpr*, Expr*> OpaqueValueMap;

  /// The getter associated with a setter function declaration.
  const VarDecl *AssociatedGetter = nullptr;

  /// The first reference to the associated getter.
  const DeclRefExpr *AssociatedGetterDeclRef = nullptr;

  /// This is a mapping from VarDecls to the if/while/guard statement that they
  /// occur in, when they are in a pattern in a StmtCondition.
  llvm::SmallDenseMap<VarDecl*, LabeledConditionalStmt*> StmtConditionForVD;
  
  bool sawError = false;
  
  VarDeclUsageChecker(const VarDeclUsageChecker &) = delete;
  void operator=(const VarDeclUsageChecker &) = delete;

public:
  VarDeclUsageChecker(TypeChecker &TC, AbstractFunctionDecl *AFD) : Diags(TC.Diags) {
    // If this AFD is a setter, track the parameter and the getter for
    // the containing property so if newValue isn't used but the getter is used
    // an error can be reported.
    if (auto FD = dyn_cast<AccessorDecl>(AFD)) {
      if (FD->getAccessorKind() == AccessorKind::Set) {
        if (auto getter = dyn_cast<VarDecl>(FD->getStorage())) {
          auto arguments = FD->getParameters();
          VarDecls[arguments->get(0)] = 0;
          AssociatedGetter = getter;
        }
      }
    }
  }

  VarDeclUsageChecker(DiagnosticEngine &Diags) : Diags(Diags) {}

  VarDeclUsageChecker(TypeChecker &TC, VarDecl *VD) : Diags(TC.Diags) {
    // Track a specific VarDecl
    VarDecls[VD] = 0;
  }

  void suppressDiagnostics() {
    sawError = true; // set this flag so that no diagnostics will be emitted on delete.
  }
    
  // After we have scanned the entire region, diagnose variables that could be
  // declared with a narrower usage kind.
  ~VarDeclUsageChecker() override;

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
      PBE.getPattern()->forEachVariable([&](VarDecl *VD) {
        auto it = VarDecls.find(VD);
        sawMutation |= it != VarDecls.end() && (it->second & RK_Written);
      });
    }
    return sawMutation;
  }
    
  bool isVarDeclEverWritten(VarDecl *VD) {
    return (VarDecls[VD] & RK_Written) != 0;
  }

  bool shouldTrackVarDecl(VarDecl *VD) {
    // If the variable is implicit, ignore it.
    if (VD->isImplicit() || VD->getLoc().isInvalid())
      return false;
    
    // If the variable is computed, ignore it.
    if (!VD->hasStorage())
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
      
    // The body of #if clauses are not walked into, we need custom processing
    // for them.
    if (auto *ICD = dyn_cast<IfConfigDecl>(D))
      handleIfConfig(ICD);
      
    // If this is a VarDecl, then add it to our list of things to track.
    if (auto *vd = dyn_cast<VarDecl>(D))
      if (shouldTrackVarDecl(vd)) {
        unsigned defaultFlags = 0;
        // If this VarDecl is nested inside of a CaptureListExpr, remember that
        // fact for better diagnostics.
        auto parentAsExpr = Parent.getAsExpr();
        if (parentAsExpr && isa<CaptureListExpr>(parentAsExpr))
          defaultFlags = RK_CaptureList;
        VarDecls[vd] |= defaultFlags;
      }

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
    if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      // If this is a TopLevelCodeDecl, scan for global variables
      auto *body = TLCD->getBody();
      for (auto node : body->getElements()) {
        if (node.is<Decl *>()) {
          // Flag all variables in a PatternBindingDecl
          Decl *D = node.get<Decl *>();
          auto *PBD = dyn_cast<PatternBindingDecl>(D);
          if (!PBD) continue;
          for (PatternBindingEntry PBE : PBD->getPatternList()) {
            PBE.getPattern()->forEachVariable([&](VarDecl *VD) {
              VarDecls[VD] = RK_Read|RK_Written;
            });
          }
        } else if (node.is<Stmt *>()) {
          // Flag all variables in guard statements
          Stmt *S = node.get<Stmt *>();
          auto *GS = dyn_cast<GuardStmt>(S);
          if (!GS) continue;
          for (StmtConditionElement SCE : GS->getCond()) {
            if (auto pattern = SCE.getPatternOrNull()) {
              pattern->forEachVariable([&](VarDecl *VD) {
                VarDecls[VD] = RK_Read|RK_Written;
              });
            }
          }
        }
      }
    }

    // Note that we ignore the initialization behavior of PatternBindingDecls,
    // but we do want to walk into them, because we want to see any uses or
    // other things going on in the initializer expressions.
    return true;
  }
  
  /// The heavy lifting happens when visiting expressions.
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;

  /// handle #if directives.
  void handleIfConfig(IfConfigDecl *ICD);

  /// Custom handling for statements.
  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    // Keep track of an association between vardecls and the StmtCondition that
    // they are bound in for IfStmt, GuardStmt, WhileStmt, etc.
    if (auto LCS = dyn_cast<LabeledConditionalStmt>(S)) {
      for (auto &cond : LCS->getCond())
        if (auto pat = cond.getPatternOrNull()) {
          pat->forEachVariable([&](VarDecl *VD) {
            StmtConditionForVD[VD] = LCS;
          });
        }
    }
    
    // A fallthrough dest case's bound variable means the source case's
    // var of the same name is read.
    if (auto *fallthroughStmt = dyn_cast<FallthroughStmt>(S)) {
      if (auto *sourceCase = fallthroughStmt->getFallthroughSource()) {
        SmallVector<VarDecl *, 4> sourceVars;
        auto sourcePattern = sourceCase->getCaseLabelItems()[0].getPattern();
        sourcePattern->collectVariables(sourceVars);
        
        auto destCase = fallthroughStmt->getFallthroughDest();
        auto destPattern = destCase->getCaseLabelItems()[0].getPattern();
        destPattern->forEachVariable([&](VarDecl *V) {
          if (!V->hasName())
            return;
          for (auto *var : sourceVars) {
            if (var->hasName() && var->getName() == V->getName()) {
              VarDecls[var] |= RK_Read;
              break;
            }
          }
        });
      }
    }
      
    return { true, S };
  }

};
} // end anonymous namespace


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
    auto isWrittenLet = false;
    if (var->isImmutable()) {
      isWrittenLet = (access & RK_Written) != 0;
      access &= ~RK_Written;
    }
    
    // If this variable has WeakStorageType, then it can be mutated in ways we
    // don't know.
    if (var->getType()->is<WeakStorageType>())
      access |= RK_Written;
    
    // If this is a vardecl with 'inout' type, then it is an inout argument to a
    // function, never diagnose anything related to it.
    if (var->isInOut())
      continue;    
    
    // If the setter parameter is not used, but the property is read, report
    // a warning. Otherwise, parameters should not generate usage warnings. It
    // is common to name a parameter and not use it (e.g. because you are an
    // override or want the named keyword, etc).  Warning to rewrite it to _ is
    // more annoying than it is useful.
    if (auto param = dyn_cast<ParamDecl>(var)) {
      auto FD = dyn_cast<AccessorDecl>(param->getDeclContext());
      if (FD && FD->getAccessorKind() == AccessorKind::Set) {
        auto getter = dyn_cast<VarDecl>(FD->getStorage());
        if ((access & RK_Read) == 0 && AssociatedGetter == getter) {
          if (auto DRE = AssociatedGetterDeclRef) {
            Diags.diagnose(DRE->getLoc(), diag::unused_setter_parameter,
                           var->getName());
            Diags.diagnose(DRE->getLoc(), diag::fixit_for_unused_setter_parameter,
                           var->getName())
              .fixItReplace(DRE->getSourceRange(), var->getName().str());
          }
        }
      }
      continue;
    }
    
    // Diagnose variables that were never used (other than their
    // initialization).
    //
    if ((access & (RK_Read|RK_Written)) == 0) {
      // If this is a member in a capture list, just say it is unused.  We could
      // produce a fixit hint with a parent map, but this is a lot of effort for
      // a narrow case.
      if (access & RK_CaptureList) {
        Diags.diagnose(var->getLoc(), diag::capture_never_used,
                       var->getName());
        continue;
      }
      
      // If the source of the VarDecl is a trivial PatternBinding with only a
      // single binding, rewrite the whole thing into an assignment.
      //    let x = foo()
      //  ->
      //    _ = foo()
      if (auto *pbd = var->getParentPatternBinding())
        if (pbd->getSingleVar() == var && pbd->getInit(0) != nullptr &&
            !isa<TypedPattern>(pbd->getPatternList()[0].getPattern())) {
          unsigned varKind = var->isLet();
          SourceRange replaceRange(
              pbd->getStartLoc(),
              pbd->getPatternList()[0].getPattern()->getEndLoc());
          Diags.diagnose(var->getLoc(), diag::pbd_never_used,
                         var->getName(), varKind)
            .fixItReplace(replaceRange, "_");
          continue;
        }
      
      // If the variable is defined in a pattern in an if/while/guard statement,
      // see if we can produce a tuned fixit.  When we have something like:
      //
      //    if let x = <expr> {
      //
      // we prefer to rewrite it to:
      //
      //    if <expr> != nil {
      //
      if (auto SC = StmtConditionForVD[var]) {
        // We only handle the "if let" case right now, since it is vastly the
        // most common situation that people run into.
        if (SC->getCond().size() == 1) {
          auto pattern = SC->getCond()[0].getPattern();
          if (auto OSP = dyn_cast<OptionalSomePattern>(pattern))
            if (auto LP = dyn_cast<VarPattern>(OSP->getSubPattern()))
              if (isa<NamedPattern>(LP->getSubPattern())) {
                auto initExpr = SC->getCond()[0].getInitializer();
                if (initExpr->getStartLoc().isValid()) {
                  unsigned noParens = initExpr->canAppendPostfixExpression();
                  
                  // If the subexpr is an "as?" cast, we can rewrite it to
                  // be an "is" test.
                  bool isIsTest = false;
                  if (isa<ConditionalCheckedCastExpr>(initExpr) &&
                      !initExpr->isImplicit()) {
                    noParens = isIsTest = true;
                  }
                  
                  auto diagIF = Diags.diagnose(var->getLoc(),
                                               diag::pbd_never_used_stmtcond,
                                            var->getName());
                  auto introducerLoc = SC->getCond()[0].getIntroducerLoc();
                  diagIF.fixItReplaceChars(introducerLoc,
                                           initExpr->getStartLoc(),
                                           &"("[noParens]);
                  
                  if (isIsTest) {
                    // If this was an "x as? T" check, rewrite it to "x is T".
                    auto CCE = cast<ConditionalCheckedCastExpr>(initExpr);
                    diagIF.fixItReplace(SourceRange(CCE->getLoc(),
                                                    CCE->getQuestionLoc()),
                                        "is");
                  } else {
                    diagIF.fixItInsertAfter(initExpr->getEndLoc(),
                                            &") != nil"[noParens]);
                  }
                  continue;
                }
              }
        }
      }
      
      // Otherwise, this is something more complex, perhaps
      //    let (a,b) = foo()
      if (isWrittenLet) {
        Diags.diagnose(var->getLoc(),
                       diag::immutable_value_never_used_but_assigned,
                       var->getName());
      } else {
        unsigned varKind = var->isLet();
        // Just rewrite the one variable with a _.
        Diags.diagnose(var->getLoc(), diag::variable_never_used,
                       var->getName(), varKind)
          .fixItReplace(var->getLoc(), "_");
      }
      continue;
    }
    
    // If this is a mutable 'var', and it was never written to, suggest
    // upgrading to 'let'.  We do this even for a parameter.
    if (!var->isImmutable() && (access & RK_Written) == 0 &&
        // Don't warn if we have something like "let (x,y) = ..." and 'y' was
        // never mutated, but 'x' was.
        !isVarDeclPartOfPBDThatHadSomeMutation(var)) {
      SourceLoc FixItLoc;

      // Try to find the location of the 'var' so we can produce a fixit.  If
      // this is a simple PatternBinding, use its location.
      if (auto *PBD = var->getParentPatternBinding()) {
        if (PBD->getSingleVar() == var)
          FixItLoc = PBD->getLoc();
      } else if (auto *pattern = var->getParentPattern()) {
        VarPattern *foundVP = nullptr;
        pattern->forEachNode([&](Pattern *P) {
          if (auto *VP = dyn_cast<VarPattern>(P))
            if (VP->getSingleVar() == var)
              foundVP = VP;
        });
        
        if (foundVP && !foundVP->isLet())
          FixItLoc = foundVP->getLoc();
      }

      // If this is a parameter explicitly marked 'var', remove it.
      unsigned varKind = isa<ParamDecl>(var);
      if (FixItLoc.isInvalid())
        Diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                       var->getName(), varKind);
      else {
        bool suggestLet = true;
        if (auto *stmt = var->getParentPatternStmt()) {
          // Don't try to suggest 'var' -> 'let' conversion
          // in case of 'for' loop because it's an implicitly
          // immutable context.
          suggestLet = !isa<ForEachStmt>(stmt);
        }

        auto diag = Diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                                   var->getName(), varKind);

        if (suggestLet)
          diag.fixItReplace(FixItLoc, "let");
        else
          diag.fixItRemove(FixItLoc);

        continue;
      }
    }
    
    // If this is a variable that was only written to, emit a warning.
    if ((access & RK_Read) == 0) {
      Diags.diagnose(var->getLoc(), diag::variable_never_read, var->getName(),
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
      !(base->getType()->hasLValueType() || base->isSemanticallyInOutExpr())) {
    base->walk(*this);
    return;
  }

  // If the store is to a non-mutating member, then this is just a load, even
  // if the base is an inout expr.
  auto *ASD = cast<AbstractStorageDecl>(decl.getDecl());
  if (ASD->isSettable(nullptr) && !ASD->isSetterMutating()) {
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
  if (E == nullptr || !E->getType() || E->getType()->hasError()) {
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
    // The index of the subscript is evaluated as an rvalue.
    SE->getIndex()->walk(*this);
    if (SE->hasDecl())
      markBaseOfAbstractStorageDeclStore(SE->getBase(), SE->getDecl());
    else  // FIXME: Should not be needed!
      markStoredOrInOutExpr(SE->getBase(), RK_Written|RK_Read);
    
    return;
  }
  
  // Likewise for key path applications. An application of a WritableKeyPath
  // reads and writes its base.
  if (auto *KPA = dyn_cast<KeyPathApplicationExpr>(E)) {
    auto &C = KPA->getType()->getASTContext();
    KPA->getKeyPath()->walk(*this);
    if (KPA->getKeyPath()->getType()->getAnyNominal()
          == C.getWritableKeyPathDecl())
      markStoredOrInOutExpr(KPA->getBase(), RK_Written|RK_Read);
    if (KPA->getKeyPath()->getType()->getAnyNominal()
          == C.getReferenceWritableKeyPathDecl())
      markStoredOrInOutExpr(KPA->getBase(), RK_Read);
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

  if (auto *BOE = dyn_cast<BindOptionalExpr>(E))
    return markStoredOrInOutExpr(BOE->getSubExpr(), Flags);
  
  // If this is an OpaqueValueExpr that we've seen a mapping for, jump to the
  // mapped value.
  if (auto *OVE = dyn_cast<OpaqueValueExpr>(E))
    if (auto *expr = OpaqueValueMap[OVE])
      return markStoredOrInOutExpr(expr, Flags);

  // If we don't know what kind of expression this is, assume it's a reference
  // and mark it as a read.
  E->walk(*this);
}

   

/// The heavy lifting happens when visiting expressions.
std::pair<bool, Expr *> VarDeclUsageChecker::walkToExprPre(Expr *E) {
  // Sema leaves some subexpressions null, which seems really unfortunate.  It
  // should replace them with ErrorExpr.
  if (E == nullptr || !E->getType() || E->getType()->hasError()) {
    sawError = true;
    return { false, E };
  }

  // If this is a DeclRefExpr found in a random place, it is a load of the
  // vardecl.
  if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
    addMark(DRE->getDecl(), RK_Read);

    // If the Decl is a read of a getter, track the first DRE for diagnostics
    if (auto VD = dyn_cast<VarDecl>(DRE->getDecl())) {
      if (AssociatedGetter == VD && AssociatedGetterDeclRef == nullptr)
        AssociatedGetterDeclRef = DRE;
    }
  }
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
  
  // If we see an OpenExistentialExpr, remember the mapping for its OpaqueValue.
  if (auto *oee = dyn_cast<OpenExistentialExpr>(E))
    OpaqueValueMap[oee->getOpaqueValue()] = oee->getExistentialValue();
  
  // If we saw an ErrorExpr, take note of this.
  if (isa<ErrorExpr>(E))
    sawError = true;
  
  return { true, E };
}

/// handle #if directives.  All of the active clauses are already walked by the
/// AST walker, but we also want to handle the inactive ones to avoid false
/// positives.
void VarDeclUsageChecker::handleIfConfig(IfConfigDecl *ICD) {
  struct ConservativeDeclMarker : public ASTWalker {
    VarDeclUsageChecker &VDUC;
    ConservativeDeclMarker(VarDeclUsageChecker &VDUC) : VDUC(VDUC) {}

    Expr *walkToExprPost(Expr *E) override {
      // If we see a bound reference to a decl in an inactive #if block, then
      // conservatively mark it read and written.  This will silence "variable
      // unused" and "could be marked let" warnings for it.
      if (auto *DRE = dyn_cast<DeclRefExpr>(E))
        VDUC.addMark(DRE->getDecl(), RK_Read|RK_Written);
      return E;
    }
  };

  for (auto &clause : ICD->getClauses()) {
    // Active clauses are handled by the normal AST walk.
    if (clause.isActive) continue;

    for (auto elt : clause.Elements)
      elt.walk(ConservativeDeclMarker(*this));
  }
}

/// Apply the warnings managed by VarDeclUsageChecker to the top level
/// code declarations that haven't been checked yet.
void swift::
performTopLevelDeclDiagnostics(TypeChecker &TC, TopLevelCodeDecl *TLCD) {
  VarDeclUsageChecker checker(TC.Diags);
  TLCD->walk(checker);
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

// Perform MiscDiagnostics on Switch Statements.
static void checkSwitch(TypeChecker &TC, const SwitchStmt *stmt) {
  // We want to warn about "case .Foo, .Bar where 1 != 100:" since the where
  // clause only applies to the second case, and this is surprising.
  for (auto cs : stmt->getCases()) {
    TC.checkUnsupportedProtocolType(cs);

    // The case statement can have multiple case items, each can have a where.
    // If we find a "where", and there is a preceding item without a where, and
    // if they are on the same source line, then warn.
    auto items = cs->getCaseLabelItems();
    
    // Don't do any work for the vastly most common case.
    if (items.size() == 1) continue;
    
    // Ignore the first item, since it can't have preceding ones.
    for (unsigned i = 1, e = items.size(); i != e; ++i) {
      // Must have a where clause.
      auto where = items[i].getGuardExpr();
      if (!where)
        continue;
      
      // Preceding item must not.
      if (items[i-1].getGuardExpr())
        continue;
      
      // Must be on the same source line.
      auto prevLoc = items[i-1].getStartLoc();
      auto thisLoc = items[i].getStartLoc();
      if (prevLoc.isInvalid() || thisLoc.isInvalid())
        continue;
      
      auto &SM = TC.Context.SourceMgr;
      auto prevLineCol = SM.getLineAndColumn(prevLoc);
      if (SM.getLineNumber(thisLoc) != prevLineCol.first)
        continue;
      
      TC.diagnose(items[i].getWhereLoc(), diag::where_on_one_item)
        .highlight(items[i].getPattern()->getSourceRange())
        .highlight(where->getSourceRange());
      
      // Whitespace it out to the same column as the previous item.
      std::string whitespace(prevLineCol.second-1, ' ');
      TC.diagnose(thisLoc, diag::add_where_newline)
        .fixItInsert(thisLoc, "\n"+whitespace);

      auto whereRange = SourceRange(items[i].getWhereLoc(),
                                    where->getEndLoc());
      auto charRange = Lexer::getCharSourceRangeFromSourceRange(SM, whereRange);
      auto whereText = SM.extractText(charRange);
      TC.diagnose(prevLoc, diag::duplicate_where)
        .fixItInsertAfter(items[i-1].getEndLoc(), " " + whereText.str())
        .highlight(items[i-1].getSourceRange());
    }
  }
}

void swift::fixItEncloseTrailingClosure(TypeChecker &TC,
                                        InFlightDiagnostic &diag,
                                        const CallExpr *call,
                                        Identifier closureLabel) {
  auto argsExpr = call->getArg();
  if (auto TSE = dyn_cast<TupleShuffleExpr>(argsExpr))
    argsExpr = TSE->getSubExpr();

  SmallString<32> replacement;
  SourceLoc lastLoc;
  SourceRange closureRange;
  if (auto PE = dyn_cast<ParenExpr>(argsExpr)) {
    assert(PE->hasTrailingClosure() && "must have trailing closure");
    closureRange = PE->getSubExpr()->getSourceRange();
    lastLoc = PE->getLParenLoc(); // e.g funcName() { 1 }
    if (!lastLoc.isValid()) {
      // Bare trailing closure: e.g. funcName { 1 }
      replacement = "(";
      lastLoc = call->getFn()->getEndLoc();
    }
  } else if (auto TE = dyn_cast<TupleExpr>(argsExpr)) {
    // Tuple + trailing closure: e.g. funcName(x: 1) { 1 }
    assert(TE->hasTrailingClosure() && "must have trailing closure");
    auto numElements = TE->getNumElements();
    assert(numElements >= 2 && "Unexpected num of elements in TupleExpr");
    closureRange = TE->getElement(numElements - 1)->getSourceRange();
    lastLoc = TE->getElement(numElements - 2)->getEndLoc();
    replacement = ", ";
  } else {
    // Can't be here.
    return;
  }

  // Add argument label of the closure.
  if (!closureLabel.empty()) {
    replacement += closureLabel.str();
    replacement += ": ";
  }

  lastLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr, lastLoc);
  diag
    .fixItReplaceChars(lastLoc, closureRange.Start, replacement)
    .fixItInsertAfter(closureRange.End, ")");
}

// Perform checkStmtConditionTrailingClosure for single expression.
static void checkStmtConditionTrailingClosure(TypeChecker &TC, const Expr *E) {
  if (E == nullptr || isa<ErrorExpr>(E)) return;

  // Shallow walker. just dig into implicit expression.
  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;

    void diagnoseIt(const CallExpr *E) {
      if (!E->hasTrailingClosure()) return;

      auto argsExpr = E->getArg();
      auto argsTy = argsExpr->getType();
      // Ignore invalid argument type. Some diagnostics are already emitted.
      if (!argsTy || argsTy->hasError()) return;

      if (auto TSE = dyn_cast<TupleShuffleExpr>(argsExpr))
        argsExpr = TSE->getSubExpr();

      SourceLoc closureLoc;
      if (auto PE = dyn_cast<ParenExpr>(argsExpr))
        closureLoc = PE->getSubExpr()->getStartLoc();
      else if (auto TE = dyn_cast<TupleExpr>(argsExpr))
        closureLoc = TE->getElements().back()->getStartLoc();

      Identifier closureLabel;
      if (auto TT = argsTy->getAs<TupleType>()) {
        assert(TT->getNumElements() != 0 && "Unexpected empty TupleType");
        closureLabel = TT->getElement(TT->getNumElements() - 1).getName();
      }

      auto diag = TC.diagnose(closureLoc,
                              diag::trailing_closure_requires_parens);
      fixItEncloseTrailingClosure(TC, diag, E, closureLabel);
    }

  public:
    DiagnoseWalker(TypeChecker &tc) : TC(tc) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // Dig into implicit expression.
      if (E->isImplicit()) return { true, E };
      // Diagnose call expression.
      if (auto CE = dyn_cast<CallExpr>(E))
        diagnoseIt(CE);
      // Don't dig any further.
      return { false, E };
    }
  };

  DiagnoseWalker Walker(TC);
  const_cast<Expr *>(E)->walk(Walker);
}

/// \brief Diagnose trailing closure in statement-conditions.
///
/// Conditional statements, including 'for' or `switch` doesn't allow ambiguous
/// trailing closures in these conditions part. Even if the parser can recover
/// them, we force them to disambiguate.
//
/// E.g.:
///   if let _ = arr?.map {$0+1} { ... }
///   for _ in numbers.filter {$0 > 4} { ... }
static void checkStmtConditionTrailingClosure(TypeChecker &TC, const Stmt *S) {
  if (auto LCS = dyn_cast<LabeledConditionalStmt>(S)) {
    for (auto elt : LCS->getCond()) {
      if (elt.getKind() == StmtConditionElement::CK_PatternBinding)
        checkStmtConditionTrailingClosure(TC, elt.getInitializer());
      else if (elt.getKind() == StmtConditionElement::CK_Boolean)
        checkStmtConditionTrailingClosure(TC, elt.getBoolean());
      // No trailing closure for CK_Availability: e.g. `if #available() {}`.
    }
  } else if (auto SS = dyn_cast<SwitchStmt>(S)) {
    checkStmtConditionTrailingClosure(TC, SS->getSubjectExpr());
  } else if (auto FES = dyn_cast<ForEachStmt>(S)) {
    checkStmtConditionTrailingClosure(TC, FES->getSequence());
    checkStmtConditionTrailingClosure(TC, FES->getWhere());
  } else if (auto DCS = dyn_cast<DoCatchStmt>(S)) {
    for (auto CS : DCS->getCatches())
      checkStmtConditionTrailingClosure(TC, CS->getGuardExpr());
  }
}

static Optional<ObjCSelector>
parseObjCSelector(ASTContext &ctx, StringRef string) {
  // Find the first colon.
  auto colonPos = string.find(':');

  // If there is no colon, we have a nullary selector.
  if (colonPos == StringRef::npos) {
    if (string.empty() || !Lexer::isIdentifier(string)) return None;
    return ObjCSelector(ctx, 0, { ctx.getIdentifier(string) });
  }

  SmallVector<Identifier, 2> pieces;
  do {
    // Check whether we have a valid selector piece.
    auto piece = string.substr(0, colonPos);
    if (piece.empty()) {
      pieces.push_back(Identifier());
    } else {
      if (!Lexer::isIdentifier(piece)) return None;
      pieces.push_back(ctx.getIdentifier(piece));
    }

    // Move to the next piece.
    string = string.substr(colonPos+1);
    colonPos = string.find(':');
  } while (colonPos != StringRef::npos);

  // If anything remains of the string, it's not a selector.
  if (!string.empty()) return None;

  return ObjCSelector(ctx, pieces.size(), pieces);
}


namespace {

class ObjCSelectorWalker : public ASTWalker {
  TypeChecker &TC;
  const DeclContext *DC;
  Type SelectorTy;

  /// Determine whether a reference to the given method via its
  /// enclosing class/protocol is ambiguous (and, therefore, needs to
  /// be disambiguated with a coercion).
  bool isSelectorReferenceAmbiguous(AbstractFunctionDecl *method) {
    // Determine the name we would search for. If there are no
    // argument names, our lookup will be based solely on the base
    // name.
    DeclName lookupName = method->getFullName();
    if (lookupName.getArgumentNames().empty())
      lookupName = lookupName.getBaseName();

    // Look for members with the given name.
    auto nominal = method->getDeclContext()->getSelfNominalTypeDecl();
    auto result = TC.lookupMember(const_cast<DeclContext *>(DC),
                                  nominal->getDeclaredInterfaceType(),
                                  lookupName,
                                  (defaultMemberLookupOptions |
                                   NameLookupFlags::KnownPrivate));

    // If we didn't find multiple methods, there is no ambiguity.
    if (result.size() < 2) return false;

    // If we found more than two methods, it's ambiguous.
    if (result.size() > 2) return true;

    // Dig out the methods.
    auto firstMethod = dyn_cast<FuncDecl>(result[0].getValueDecl());
    auto secondMethod = dyn_cast<FuncDecl>(result[1].getValueDecl());
    if (!firstMethod || !secondMethod) return true;

    // If one is a static/class method and the other is not...
    if (firstMethod->isStatic() == secondMethod->isStatic()) return true;

    // ... overload resolution will prefer the static method. Check
    // that it has the correct selector. We don't even care that it's
    // the same method we're asking for, just that it has the right
    // selector.
    FuncDecl *staticMethod =
      firstMethod->isStatic() ? firstMethod : secondMethod;
    return staticMethod->getObjCSelector() != method->getObjCSelector();
  }

public:
  ObjCSelectorWalker(TypeChecker &tc, const DeclContext *dc, Type selectorTy)
    : TC(tc), DC(dc), SelectorTy(selectorTy) { }

  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    auto *stringLiteral = dyn_cast<StringLiteralExpr>(expr);
    bool fromStringLiteral = false;
    bool hadParens = false;
    if (stringLiteral) {
      // Is this a string literal that has type 'Selector'.
      if (!stringLiteral->getType() ||
          !stringLiteral->getType()->isEqual(SelectorTy))
        return { true, expr };

      fromStringLiteral = true;

      // FIXME: hadParens
    } else {
      // Is this an initialization of 'Selector'?
      auto call = dyn_cast<CallExpr>(expr);
      if (!call) return { true, expr };

      // That produce Selectors.
      if (!call->getType() || !call->getType()->isEqual(SelectorTy))
        return { true, expr };

      // Via a constructor.
      ConstructorDecl *ctor = nullptr;
      if (auto ctorRefCall = dyn_cast<ConstructorRefCallExpr>(call->getFn())) {
        if (auto ctorRef = dyn_cast<DeclRefExpr>(ctorRefCall->getFn()))
          ctor = dyn_cast<ConstructorDecl>(ctorRef->getDecl());
        else if (auto otherCtorRef =
                   dyn_cast<OtherConstructorDeclRefExpr>(ctorRefCall->getFn()))
          ctor = otherCtorRef->getDecl();
      }

      if (!ctor) return { true, expr };

      // Make sure the constructor is within Selector.
      auto ctorContextType = ctor->getDeclContext()
          ->getSelfNominalTypeDecl()
          ->getDeclaredType();
      if (!ctorContextType || !ctorContextType->isEqual(SelectorTy))
        return { true, expr };

      auto argNames = ctor->getFullName().getArgumentNames();
      if (argNames.size() != 1) return { true, expr };

      // Is this the init(stringLiteral:) initializer or init(_:) initializer?
      if (argNames[0] == TC.Context.Id_stringLiteral)
        fromStringLiteral = true;
      else if (!argNames[0].empty())
        return { true, expr };

      Expr *arg = call->getArg();

      if (auto paren = dyn_cast<ParenExpr>(arg))
        arg = paren->getSubExpr();
      else if (auto tuple = dyn_cast<TupleExpr>(arg))
        arg = tuple->getElement(0);
      else
        return { true, expr };

      // Track whether we had parentheses around the string literal.
      if (auto paren = dyn_cast<ParenExpr>(arg)) {
        hadParens = true;
        arg = paren->getSubExpr();
      }

      // Check whether we have a string literal.
      stringLiteral = dyn_cast<StringLiteralExpr>(arg);
      if (!stringLiteral) return { true, expr };
    }

    /// Retrieve the parent expression that coerces to Selector, if
    /// there is one.
    auto getParentCoercion = [&]() -> CoerceExpr * {
      auto parentExpr = Parent.getAsExpr();
      if (!parentExpr) return nullptr;

      auto coerce = dyn_cast<CoerceExpr>(parentExpr);
      if (!coerce) return nullptr;

      if (coerce->getType() && coerce->getType()->isEqual(SelectorTy))
        return coerce;

      return nullptr;
    };

    // Local function that adds the constructor syntax around string
    // literals implicitly treated as a Selector.
    auto addSelectorConstruction = [&](InFlightDiagnostic &diag) {
      if (!fromStringLiteral) return;

      // Introduce the beginning part of the Selector construction.
      diag.fixItInsert(stringLiteral->getLoc(), "Selector(");

      if (auto coerce = getParentCoercion()) {
        // If the string literal was coerced to Selector, replace the
        // coercion with the ")".
        SourceLoc endLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                                                      expr->getEndLoc());
        diag.fixItReplace(SourceRange(endLoc, coerce->getEndLoc()), ")");
      } else {
        // Otherwise, just insert the closing ")".
        diag.fixItInsertAfter(stringLiteral->getEndLoc(), ")");
      }
    };

    // Try to parse the string literal as an Objective-C selector, and complain
    // if it isn't one.
    auto selector = parseObjCSelector(TC.Context, stringLiteral->getValue());
    if (!selector) {
      auto diag = TC.diagnose(stringLiteral->getLoc(),
                              diag::selector_literal_invalid);
      diag.highlight(stringLiteral->getSourceRange());
      addSelectorConstruction(diag);
      return { true, expr };
    }

    // Look for methods with this selector.
    SmallVector<AbstractFunctionDecl *, 8> allMethods;
    DC->lookupAllObjCMethods(*selector, allMethods);

    // If we didn't find any methods, complain.
    if (allMethods.empty()) {
      // If this was Selector(("selector-name")), suppress, the
      // diagnostic.
      if (!fromStringLiteral && hadParens)
        return { true, expr };

      {
        auto diag = TC.diagnose(stringLiteral->getLoc(),
                                diag::selector_literal_undeclared,
                                *selector);
        addSelectorConstruction(diag);
      }

      // If the result was from a Selector("selector-name"), add a
      // separate note that suggests wrapping the selector in
      // parentheses to silence the warning.
      if (!fromStringLiteral) {
        TC.diagnose(stringLiteral->getLoc(),
                    diag::selector_construction_suppress_warning)
          .fixItInsert(stringLiteral->getStartLoc(), "(")
          .fixItInsertAfter(stringLiteral->getEndLoc(), ")");
      }

      return { true, expr };
    }

    // Find the "best" method that has this selector, so we can report
    // that.
    AbstractFunctionDecl *bestMethod = nullptr;
    for (auto method : allMethods) {
      // If this is the first method, use it.
      if (!bestMethod) {
        bestMethod = method;
        continue;
      }

      // If referencing the best method would produce an ambiguity and
      // referencing the new method would not, we have a new "best".
      if (isSelectorReferenceAmbiguous(bestMethod) &&
          !isSelectorReferenceAmbiguous(method)) {
        bestMethod = method;
        continue;
      }

      // If this method is within a protocol...
      if (auto proto = method->getDeclContext()->getSelfProtocolDecl()) {
        // If the best so far is not from a protocol, or is from a
        // protocol that inherits this protocol, we have a new best.
        auto bestProto = bestMethod->getDeclContext()->getSelfProtocolDecl();
        if (!bestProto || bestProto->inheritsFrom(proto))
          bestMethod = method;
        continue;
      }

      // This method is from a class.
      auto classDecl = method->getDeclContext()->getSelfClassDecl();

      // If the best method was from a protocol, keep it.
      auto bestClassDecl = bestMethod->getDeclContext()->getSelfClassDecl();
      if (!bestClassDecl) continue;

      // If the best method was from a subclass of the place where
      // this method was declared, we have a new best.
      while (auto superclassDecl = bestClassDecl->getSuperclassDecl()) {
        if (classDecl == superclassDecl) {
          bestMethod = method;
          break;
        }

        bestClassDecl = superclassDecl;
      }
    }

    // If we have a best method, reference it.
    if (bestMethod) {
      // Form the replacement #selector expression.
      SmallString<32> replacement;
      {
        llvm::raw_svector_ostream out(replacement);
        auto nominal = bestMethod->getDeclContext()->getSelfNominalTypeDecl();
        out << "#selector(";

        DeclName name;
        auto bestAccessor = dyn_cast<AccessorDecl>(bestMethod);
        if (bestAccessor) {
          switch (bestAccessor->getAccessorKind()) {
          case AccessorKind::Get:
            out << "getter: ";
            name = bestAccessor->getStorage()->getFullName();
            break;

          case AccessorKind::Set:
          case AccessorKind::WillSet:
          case AccessorKind::DidSet:
            out << "setter: ";
            name = bestAccessor->getStorage()->getFullName();
            break;

          case AccessorKind::Address:
          case AccessorKind::MutableAddress:
          case AccessorKind::Read:
          case AccessorKind::Modify:
            llvm_unreachable("cannot be @objc");
          }
        } else {
          name = bestMethod->getFullName();
        }

        out << nominal->getName().str() << "." << name.getBaseName();
        auto argNames = name.getArgumentNames();

        // Only print the parentheses if there are some argument
        // names, because "()" would indicate a call.
        if (!argNames.empty()) {
          out << "(";
          for (auto argName : argNames) {
            if (argName.empty()) out << "_";
            else out << argName.str();
            out << ":";
          }
          out << ")";
        }

        // If there will be an ambiguity when referring to the method,
        // introduce a coercion to resolve it to the method we found.
        if (!bestAccessor && isSelectorReferenceAmbiguous(bestMethod)) {
          if (auto fnType =
                bestMethod->getInterfaceType()->getAs<FunctionType>()) {
            // For static/class members, drop the metatype argument.
            if (bestMethod->isStatic())
              fnType = fnType->getResult()->getAs<FunctionType>();

            // Drop the argument labels.
            // FIXME: They never should have been in the type anyway.
            Type type = fnType->getUnlabeledType(TC.Context);

            // Coerce to this type.
            out << " as ";
            type.print(out);
          }
        }

        out << ")";
      }

      // Emit the diagnostic.
      SourceRange replacementRange = expr->getSourceRange();
      if (auto coerce = getParentCoercion())
        replacementRange.End = coerce->getEndLoc();

      TC.diagnose(expr->getLoc(),
                  fromStringLiteral ? diag::selector_literal_deprecated_suggest
                                    : diag::selector_construction_suggest)
        .fixItReplace(replacementRange, replacement);
      return { true, expr };
    }

    // If we couldn't pick a method to use for #selector, just wrap
    // the string literal in Selector(...).
    if (fromStringLiteral) {
      auto diag = TC.diagnose(stringLiteral->getLoc(),
                              diag::selector_literal_deprecated);
      addSelectorConstruction(diag);
      return { true, expr };
    }

    return { true, expr };
  }

};
} // end anonymous namespace

static void diagDeprecatedObjCSelectors(TypeChecker &tc, const DeclContext *dc,
                                        const Expr *expr) {
  auto selectorTy = tc.getObjCSelectorType(const_cast<DeclContext *>(dc));
  if (!selectorTy) return;

  const_cast<Expr *>(expr)->walk(ObjCSelectorWalker(tc, dc, selectorTy));
}

        
        
/// Diagnose things like this, where 'i' is an Int, not an Int?
///     if let x: Int = i {
static void
checkImplicitPromotionsInCondition(const StmtConditionElement &cond,
                                   TypeChecker &TC) {
  auto *p = cond.getPatternOrNull();
  if (!p) return;
  
  if (auto *subExpr = isImplicitPromotionToOptional(cond.getInitializer())) {
    // If the subexpression was actually optional, then the pattern must be
    // checking for a type, which forced it to be promoted to a double optional
    // type.
    if (auto ooType = subExpr->getType()->getOptionalObjectType()) {
      if (auto TP = dyn_cast<TypedPattern>(p))
        // Check for 'if let' to produce a tuned diagnostic.
        if (isa<OptionalSomePattern>(TP->getSubPattern()) &&
            TP->getSubPattern()->isImplicit()) {
          TC.diagnose(cond.getIntroducerLoc(), diag::optional_check_promotion,
                      subExpr->getType())
            .highlight(subExpr->getSourceRange())
            .fixItReplace(TP->getTypeLoc().getSourceRange(),
                          ooType->getString());
          return;
        }
      TC.diagnose(cond.getIntroducerLoc(),
                  diag::optional_pattern_match_promotion,
                  subExpr->getType(), cond.getInitializer()->getType())
        .highlight(subExpr->getSourceRange());
      return;
    }
    
    TC.diagnose(cond.getIntroducerLoc(), diag::optional_check_nonoptional,
                subExpr->getType())
      .highlight(subExpr->getSourceRange());
  }
}

static void diagnoseUnintendedOptionalBehavior(TypeChecker &TC, const Expr *E,
                                               const DeclContext *DC) {
  if (!E || isa<ErrorExpr>(E) || !E->getType())
    return;

  class UnintendedOptionalBehaviorWalker : public ASTWalker {
    TypeChecker &TC;
    SmallPtrSet<Expr *, 16> IgnoredExprs;

    class OptionalToAnyCoercion {
    public:
      Type DestType;
      CoerceExpr *ParentCoercion;

      bool shouldSuppressDiagnostic() {
        // If we have a parent CoerceExpr that has the same type as our
        // Optional-to-Any coercion, don't emit a diagnostic.
        return ParentCoercion && ParentCoercion->getType()->isEqual(DestType);
      }
    };

    /// Returns true iff a coercion from srcType to destType is an
    /// Optional-to-Any coercion.
    bool isOptionalToAnyCoercion(Type srcType, Type destType) {
      size_t difference = 0;
      return isOptionalToAnyCoercion(srcType, destType, difference);
    }

    /// Returns true iff a coercion from srcType to destType is an
    /// Optional-to-Any coercion. On returning true, the value of 'difference'
    /// will be the difference in the levels of optionality.
    bool isOptionalToAnyCoercion(Type srcType, Type destType,
                                 size_t &difference) {
      SmallVector<Type, 4> destOptionals;
      auto destValueType =
        destType->lookThroughAllOptionalTypes(destOptionals);

      if (!destValueType->isAny())
        return false;

      SmallVector<Type, 4> srcOptionals;
      srcType->lookThroughAllOptionalTypes(srcOptionals);

      if (srcOptionals.size() > destOptionals.size()) {
        difference = srcOptionals.size() - destOptionals.size();
        return true;
      } else {
        return false;
      }
    }

    /// Returns true iff the collection upcast coercion is an Optional-to-Any
    /// coercion.
    bool isOptionalToAnyCoercion(CollectionUpcastConversionExpr::ConversionPair
                                   conversion) {
      if (!conversion.OrigValue || !conversion.Conversion)
        return false;

      auto srcType = conversion.OrigValue->getType();
      auto destType = conversion.Conversion->getType();
      return isOptionalToAnyCoercion(srcType, destType);
    }

    /// Looks through OptionalEvaluationExprs and InjectIntoOptionalExprs to
    /// find a child ErasureExpr, returning nullptr if no such child is found.
    /// Any intermediate OptionalEvaluationExprs will be marked as ignored.
    ErasureExpr *findErasureExprThroughOptionalInjections(Expr *E) {
      while (true) {
        if (auto *next = dyn_cast<OptionalEvaluationExpr>(E)) {
          // We don't want to re-visit any intermediate optional evaluations.
          IgnoredExprs.insert(next);
          E = next->getSubExpr();
        } else if (auto *next = dyn_cast<InjectIntoOptionalExpr>(E)) {
          E = next->getSubExpr();
        } else {
          break;
        }
      }
      return dyn_cast<ErasureExpr>(E);
    }

    void emitSilenceOptionalAnyWarningWithCoercion(Expr *E, Type destType) {
      SmallString<16> coercionString;
      coercionString += " as ";
      coercionString += destType->getWithoutParens()->getString();

      TC.diagnose(E->getLoc(), diag::silence_optional_to_any,
                  destType, coercionString.substr(1))
        .highlight(E->getSourceRange())
        .fixItInsertAfter(E->getEndLoc(), coercionString);
    }

    static bool hasImplicitlyUnwrappedResult(Expr *E) {
      auto getDeclForExpr = [&](Expr *E) -> ValueDecl * {
        if (auto *call = dyn_cast<CallExpr>(E))
          E = call->getDirectCallee();

        if (auto *subscript = dyn_cast<SubscriptExpr>(E)) {
          if (subscript->hasDecl())
            return subscript->getDecl().getDecl();

          return nullptr;
        }

        if (auto *memberRef = dyn_cast<MemberRefExpr>(E))
          return memberRef->getMember().getDecl();
        if (auto *declRef = dyn_cast<DeclRefExpr>(E))
          return declRef->getDecl();
        if (auto *apply = dyn_cast<ApplyExpr>(E))
          return apply->getCalledValue();

        return nullptr;
      };

      // Look through implicit conversions like loads, derived-to-base
      // conversion, etc.
      if (auto *ICE = dyn_cast<ImplicitConversionExpr>(E))
        E = ICE->getSubExpr();

      auto *decl = getDeclForExpr(E);

      return decl
        && decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
    }

    void visitErasureExpr(ErasureExpr *E, OptionalToAnyCoercion coercion) {
      if (coercion.shouldSuppressDiagnostic())
        return;

      auto subExpr = E->getSubExpr();

      // Look through any BindOptionalExprs, as the coercion may have started
      // from a higher level of optionality.
      while (auto *bindExpr = dyn_cast<BindOptionalExpr>(subExpr))
        subExpr = bindExpr->getSubExpr();

      // Do not warn on coercions from implicitly unwrapped optionals
      // for Swift versions less than 5.
      if (!TC.Context.isSwiftVersionAtLeast(5) &&
          hasImplicitlyUnwrappedResult(subExpr))
        return;

      // We're taking the source type from the child of any BindOptionalExprs,
      // and the destination from the parent of any
      // (InjectIntoOptional/OptionalEvaluation)Exprs in order to take into
      // account any bindings that need to be done for nested Optional-to-Any
      // coercions, e.g Int??? to Any?.
      auto srcType = subExpr->getType();
      auto destType = coercion.DestType;

      size_t optionalityDifference = 0;
      if (!isOptionalToAnyCoercion(srcType, destType, optionalityDifference))
        return;

      TC.diagnose(subExpr->getStartLoc(), diag::optional_to_any_coercion,
                  /* from */ srcType, /* to */ destType)
        .highlight(subExpr->getSourceRange());

      if (optionalityDifference == 1) {
        TC.diagnose(subExpr->getLoc(), diag::default_optional_to_any)
          .highlight(subExpr->getSourceRange())
          .fixItInsertAfter(subExpr->getEndLoc(), " ?? <#default value#>");
      }

      SmallString<4> forceUnwrapString;
      for (size_t i = 0; i < optionalityDifference; i++)
        forceUnwrapString += "!";

      TC.diagnose(subExpr->getLoc(), diag::force_optional_to_any)
        .highlight(subExpr->getSourceRange())
        .fixItInsertAfter(subExpr->getEndLoc(), forceUnwrapString);

      emitSilenceOptionalAnyWarningWithCoercion(subExpr, destType);
    }

    void visitCollectionUpcastExpr(CollectionUpcastConversionExpr *E,
                                   OptionalToAnyCoercion coercion) {
      // We only need to consider the valueConversion, as the Key type of a
      // Dictionary cannot be implicitly coerced to Any.
      auto valueConversion = E->getValueConversion();

      // We're handling the coercion of the entire collection, so we don't need
      // to re-visit a nested ErasureExpr for the value.
      if (auto conversionExpr = valueConversion.Conversion)
        if (auto *erasureExpr =
              findErasureExprThroughOptionalInjections(conversionExpr))
          IgnoredExprs.insert(erasureExpr);

      if (coercion.shouldSuppressDiagnostic() ||
          !isOptionalToAnyCoercion(valueConversion))
        return;

      auto subExpr = E->getSubExpr();

      TC.diagnose(subExpr->getStartLoc(), diag::optional_to_any_coercion,
                  /* from */ subExpr->getType(), /* to */ E->getType())
        .highlight(subExpr->getSourceRange());

      emitSilenceOptionalAnyWarningWithCoercion(subExpr, E->getType());
    }

    void visitPossibleOptionalToAnyExpr(Expr *E,
                                        OptionalToAnyCoercion coercion) {
      if (auto *upcastExpr =
          dyn_cast<CollectionUpcastConversionExpr>(E)) {
        visitCollectionUpcastExpr(upcastExpr, coercion);
      } else if (auto *erasureExpr = dyn_cast<ErasureExpr>(E)) {
        visitErasureExpr(erasureExpr, coercion);
      } else if (auto *optionalEvalExpr = dyn_cast<OptionalEvaluationExpr>(E)) {
        // The ErasureExpr could be nested within optional injections and
        // bindings, such as is the case for e.g Int??? to Any?. Try and find
        // and visit it directly, making sure we don't re-visit it later.
        auto subExpr = optionalEvalExpr->getSubExpr();
        if (auto *erasureExpr =
              findErasureExprThroughOptionalInjections(subExpr)) {
          visitErasureExpr(erasureExpr, coercion);
          IgnoredExprs.insert(erasureExpr);
        }
      }
    }

    void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
      // Warn about interpolated segments that contain optionals.
      for (auto &segment : E->getSegments()) {
        // Allow explicit casts.
        if (auto paren = dyn_cast<ParenExpr>(segment))
          if (isa<ExplicitCastExpr>(paren->getSubExpr()))
            continue;

        // Bail out if we don't have an optional.
        if (!segment->getType()->getRValueType()->getOptionalObjectType())
          continue;

        TC.diagnose(segment->getStartLoc(),
                    diag::optional_in_string_interpolation_segment)
          .highlight(segment->getSourceRange());

        // Suggest 'String(describing: <expr>)'.
        auto segmentStart = segment->getStartLoc().getAdvancedLoc(1);
        TC.diagnose(segment->getLoc(),
                    diag::silence_optional_in_interpolation_segment_call)
          .highlight(segment->getSourceRange())
          .fixItInsert(segmentStart, "String(describing: ")
          .fixItInsert(segment->getEndLoc(), ")");

        // Suggest inserting a default value.
        TC.diagnose(segment->getLoc(), diag::default_optional_to_any)
          .highlight(segment->getSourceRange())
          .fixItInsert(segment->getEndLoc(), " ?? <#default value#>");
      }
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (!E || isa<ErrorExpr>(E) || !E->getType())
        return { false, E };

      if (auto *CE = dyn_cast<AbstractClosureExpr>(E))
        if (!CE->hasSingleExpressionBody())
          return { false, E };

      if (IgnoredExprs.count(E))
        return { true, E };

      if (auto *literal = dyn_cast<InterpolatedStringLiteralExpr>(E)) {
        visitInterpolatedStringLiteralExpr(literal);
      } else if (auto *coercion = dyn_cast<CoerceExpr>(E)) {
        // If we come across a CoerceExpr, visit its subExpr with the coercion
        // as the parent, making sure we don't re-visit the subExpr later.
        auto subExpr = coercion->getSubExpr();
        visitPossibleOptionalToAnyExpr(subExpr,
                                       { subExpr->getType(), coercion });
        IgnoredExprs.insert(subExpr);
      } else {
        visitPossibleOptionalToAnyExpr(E, { E->getType(), nullptr });
      }
      return { true, E };
    }

  public:
    UnintendedOptionalBehaviorWalker(TypeChecker &tc) : TC(tc) { }
  };

  UnintendedOptionalBehaviorWalker Walker(TC);
  const_cast<Expr *>(E)->walk(Walker);
}

static void diagnoseDeprecatedWritableKeyPath(TypeChecker &TC, const Expr *E,
                                              const DeclContext *DC) {
  if (!E || isa<ErrorExpr>(E) || !E->getType())
    return;

  class DeprecatedWritableKeyPathWalker : public ASTWalker {
    TypeChecker &TC;
    const DeclContext *DC;

    void visitKeyPathApplicationExpr(KeyPathApplicationExpr *E) {
      bool isWrite = false;
      if (auto *P = Parent.getAsExpr())
        if (auto *AE = dyn_cast<AssignExpr>(P))
          if (AE->getDest() == E)
            isWrite = true;

      if (!isWrite)
        return;

      if (auto *keyPathExpr = dyn_cast<KeyPathExpr>(E->getKeyPath())) {
        auto *decl = keyPathExpr->getType()->getNominalOrBoundGenericNominal();
        if (decl != TC.Context.getWritableKeyPathDecl() &&
            decl != TC.Context.getReferenceWritableKeyPathDecl())
          return;

        assert(keyPathExpr->getComponents().size() > 0);
        auto &component = keyPathExpr->getComponents().back();
        if (component.getKind() == KeyPathExpr::Component::Kind::Property) {
          auto *storage =
            cast<AbstractStorageDecl>(component.getDeclRef().getDecl());
          if (!storage->isSettable(nullptr) ||
              !storage->isSetterAccessibleFrom(DC)) {
            TC.diagnose(keyPathExpr->getLoc(),
                        swift::diag::expr_deprecated_writable_keypath,
                        storage->getFullName());
          }
        }
      }
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (!E || isa<ErrorExpr>(E) || !E->getType())
        return {false, E};

      if (auto *KPAE = dyn_cast<KeyPathApplicationExpr>(E)) {
        visitKeyPathApplicationExpr(KPAE);
        return {true, E};
      }

      return {true, E};
    }

  public:
    DeprecatedWritableKeyPathWalker(TypeChecker &TC, const DeclContext *DC)
        : TC(TC), DC(DC) {}
  };

  DeprecatedWritableKeyPathWalker Walker(TC, DC);
  const_cast<Expr *>(E)->walk(Walker);
}

//===----------------------------------------------------------------------===//
// High-level entry points.
//===----------------------------------------------------------------------===//

/// \brief Emit diagnostics for syntactic restrictions on a given expression.
void swift::performSyntacticExprDiagnostics(TypeChecker &TC, const Expr *E,
                                            const DeclContext *DC,
                                            bool isExprStmt) {
  TC.diagnoseSelfAssignment(E);
  diagSyntacticUseRestrictions(TC, E, DC, isExprStmt);
  diagRecursivePropertyAccess(TC, E, DC);
  diagnoseImplicitSelfUseInClosure(TC, E, DC);
  diagnoseUnintendedOptionalBehavior(TC, E, DC);
  if (!TC.Context.isSwiftVersionAtLeast(5))
    diagnoseDeprecatedWritableKeyPath(TC, E, DC);
  if (!TC.getLangOpts().DisableAvailabilityChecking)
    diagAvailability(TC, E, const_cast<DeclContext*>(DC));
  if (TC.Context.LangOpts.EnableObjCInterop)
    diagDeprecatedObjCSelectors(TC, DC, E);
}

void swift::performStmtDiagnostics(TypeChecker &TC, const Stmt *S) {
  TC.checkUnsupportedProtocolType(const_cast<Stmt *>(S));
    
  if (auto switchStmt = dyn_cast<SwitchStmt>(S))
    checkSwitch(TC, switchStmt);

  checkStmtConditionTrailingClosure(TC, S);
  
  // Check for implicit optional promotions in stmt-condition patterns.
  if (auto *lcs = dyn_cast<LabeledConditionalStmt>(S))
    for (const auto &elt : lcs->getCond())
      checkImplicitPromotionsInCondition(elt, TC);
}

//===----------------------------------------------------------------------===//
// Utility functions
//===----------------------------------------------------------------------===//

void swift::fixItAccess(InFlightDiagnostic &diag, ValueDecl *VD,
                        AccessLevel desiredAccess, bool isForSetter) {
  StringRef fixItString;
  switch (desiredAccess) {
  case AccessLevel::Private:      fixItString = "private ";      break;
  case AccessLevel::FilePrivate:  fixItString = "fileprivate ";  break;
  case AccessLevel::Internal:     fixItString = "internal ";     break;
  case AccessLevel::Public:       fixItString = "public ";       break;
  case AccessLevel::Open:         fixItString = "open ";         break;
  }

  DeclAttributes &attrs = VD->getAttrs();
  AbstractAccessControlAttr *attr;
  if (isForSetter) {
    attr = attrs.getAttribute<SetterAccessAttr>();
    cast<AbstractStorageDecl>(VD)->overwriteSetterAccess(desiredAccess);
  } else {
    attr = attrs.getAttribute<AccessControlAttr>();
    VD->overwriteAccess(desiredAccess);

    if (auto *ASD = dyn_cast<AbstractStorageDecl>(VD)) {
      if (auto *getter = ASD->getGetter())
        getter->overwriteAccess(desiredAccess);

      if (auto *setterAttr = attrs.getAttribute<SetterAccessAttr>()) {
        if (setterAttr->getAccess() > desiredAccess)
          fixItAccess(diag, VD, desiredAccess, true);
      } else {
        ASD->overwriteSetterAccess(desiredAccess);
      }
    }
  }

  if (isForSetter && VD->getFormalAccess() == desiredAccess) {
    assert(attr);
    attr->setInvalid();
    // Remove the setter attribute.
    diag.fixItRemove(attr->Range);

  } else if (attr) {
    // If the formal access already matches the desired access, the problem
    // must be in a parent scope. Don't emit a fix-it.
    // FIXME: It's also possible for access to already be /broader/ than what's
    // desired, in which case the problem is also in a parent scope. However,
    // this function is sometimes called to make access narrower, so assuming
    // that a broader scope is acceptable breaks some diagnostics.
    if (attr->getAccess() != desiredAccess) {
      // This uses getLocation() instead of getRange() because we don't want to
      // replace the "(set)" part of a setter attribute.
      diag.fixItReplace(attr->getLocation(), fixItString.drop_back());
      attr->setInvalid();
    }

  } else if (auto var = dyn_cast<VarDecl>(VD)) {
    if (auto PBD = var->getParentPatternBinding())
      diag.fixItInsert(PBD->getStartLoc(), fixItString);

  } else {
    diag.fixItInsert(VD->getStartLoc(), fixItString);
  }
}

/// Retrieve the type name to be used for determining whether we can
/// omit needless words.
static OmissionTypeName getTypeNameForOmission(Type type) {
  if (!type)
    return "";

  ASTContext &ctx = type->getASTContext();
  Type boolType;
  if (auto boolDecl = ctx.getBoolDecl())
    boolType = boolDecl->getDeclaredInterfaceType();
  Type objcBoolType;
  if (auto objcBoolDecl = ctx.getObjCBoolDecl())
    objcBoolType = objcBoolDecl->getDeclaredInterfaceType();

  /// Determine the options associated with the given type.
  auto getOptions = [&](Type type) {
    // Look for Boolean types.
    OmissionTypeOptions options;

    // Look for Boolean types.
    if (boolType && type->isEqual(boolType)) {
      // Swift.Bool
      options |= OmissionTypeFlags::Boolean;
    } else if (objcBoolType && type->isEqual(objcBoolType)) {
      // ObjectiveC.ObjCBool
      options |= OmissionTypeFlags::Boolean;
    }

    return options;
  };

  do {
    // Look through typealiases.
    if (auto aliasTy = dyn_cast<NameAliasType>(type.getPointer())) {
      type = aliasTy->getSinglyDesugaredType();
      continue;
    }

    // Strip off lvalue/inout types.
    Type newType = type->getWithoutSpecifierType();
    if (newType.getPointer() != type.getPointer()) {
      type = newType;
      continue;
    }

    // Look through reference-storage types.
    newType = type->getReferenceStorageReferent();
    if (newType.getPointer() != type.getPointer()) {
      type = newType;
      continue;
    }

    // Look through parentheses.
    type = type->getWithoutParens();

    // Look through optionals.
    if (auto optObjectTy = type->getOptionalObjectType()) {
      type = optObjectTy;
      continue;
    }

    break;
  } while (true);

  // Nominal types.
  if (auto nominal = type->getAnyNominal()) {
    // If we have a collection, get the element type.
    if (auto bound = type->getAs<BoundGenericType>()) {
      ASTContext &ctx = nominal->getASTContext();
      auto args = bound->getGenericArgs();
      if (!args.empty() &&
          (bound->getDecl() == ctx.getArrayDecl() ||
           bound->getDecl() == ctx.getSetDecl())) {
        return OmissionTypeName(nominal->getName().str(),
                                getOptions(bound),
                                getTypeNameForOmission(args[0]).Name);
      }
    }

    // AnyObject -> "Object".
    if (type->isAnyObject())
      return "Object";

    return OmissionTypeName(nominal->getName().str(), getOptions(type));
  }

  // Generic type parameters.
  if (auto genericParamTy = type->getAs<GenericTypeParamType>()) {
    if (auto genericParam = genericParamTy->getDecl())
      return genericParam->getName().str();

    return "";
  }

  // Dependent members.
  if (auto dependentMemberTy = type->getAs<DependentMemberType>()) {
    return dependentMemberTy->getName().str();
  }

  // Archetypes.
  if (auto archetypeTy = type->getAs<ArchetypeType>()) {
    return archetypeTy->getName().str();
  }

  // Function types.
  if (auto funcTy = type->getAs<AnyFunctionType>()) {
    if (funcTy->getRepresentation() == AnyFunctionType::Representation::Block)
      return "Block";

    return "Function";
  }
  return "";
}

Optional<DeclName> TypeChecker::omitNeedlessWords(AbstractFunctionDecl *afd) {
  auto &Context = afd->getASTContext();

  if (!afd->hasInterfaceType())
    validateDecl(afd);

  if (afd->isInvalid() || isa<DestructorDecl>(afd))
    return None;

  DeclName name = afd->getFullName();
  if (!name)
    return None;

  // String'ify the arguments.
  StringRef baseNameStr = name.getBaseName().userFacingName();
  SmallVector<StringRef, 4> argNameStrs;
  for (auto arg : name.getArgumentNames()) {
    if (arg.empty())
      argNameStrs.push_back("");
    else
      argNameStrs.push_back(arg.str());
  }

  // String'ify the parameter types.
  SmallVector<OmissionTypeName, 4> paramTypes;

  // Always look at the parameters in the last parameter list.
  for (auto param : *afd->getParameters()) {
    paramTypes.push_back(getTypeNameForOmission(param->getInterfaceType())
                         .withDefaultArgument(param->isDefaultArgument()));
  }
  
  // Handle contextual type, result type, and returnsSelf.
  Type contextType = afd->getDeclContext()->getDeclaredInterfaceType();
  Type resultType;
  bool returnsSelf = false;

  if (auto func = dyn_cast<FuncDecl>(afd)) {
    resultType = func->getResultInterfaceType();
    resultType = func->mapTypeIntoContext(resultType);
    returnsSelf = func->hasDynamicSelf();
  } else if (isa<ConstructorDecl>(afd)) {
    resultType = contextType;
    returnsSelf = true;
  }

  // Figure out the first parameter name.
  StringRef firstParamName;
  auto params = afd->getParameters();
  if (params->size() != 0 && !params->get(0)->getName().empty())
    firstParamName = params->get(0)->getName().str();

  StringScratchSpace scratch;
  if (!swift::omitNeedlessWords(baseNameStr, argNameStrs, firstParamName,
                                getTypeNameForOmission(resultType),
                                getTypeNameForOmission(contextType),
                                paramTypes, returnsSelf, false,
                                /*allPropertyNames=*/nullptr, scratch))
    return None;

  /// Retrieve a replacement identifier.
  auto getReplacementIdentifier = [&](StringRef name,
                                      DeclBaseName old) -> DeclBaseName{
    if (name.empty())
      return Identifier();

    if (!old.empty() && name == old.userFacingName())
      return old;

    return Context.getIdentifier(name);
  };

  auto newBaseName = getReplacementIdentifier(
      baseNameStr, name.getBaseName());
  SmallVector<Identifier, 4> newArgNames;
  auto oldArgNames = name.getArgumentNames();
  for (unsigned i = 0, n = argNameStrs.size(); i != n; ++i) {
    auto argBaseName = getReplacementIdentifier(argNameStrs[i],
                                                oldArgNames[i]);
    newArgNames.push_back(argBaseName.getIdentifier());
  }

  return DeclName(Context, newBaseName, newArgNames);
}

Optional<Identifier> TypeChecker::omitNeedlessWords(VarDecl *var) {
  auto &Context = var->getASTContext();

  if (!var->hasInterfaceType())
    validateDecl(var);

  if (var->isInvalid() || !var->hasInterfaceType())
    return None;

  if (var->getName().empty())
    return None;

  auto name = var->getName().str();

  // Dig out the context type.
  Type contextType = var->getDeclContext()->getDeclaredInterfaceType();
  if (!contextType)
    return None;

  // Dig out the type of the variable.
  Type type = var->getInterfaceType()->getReferenceStorageReferent()
                ->getWithoutSpecifierType();
  while (auto optObjectTy = type->getOptionalObjectType())
    type = optObjectTy;

  // Omit needless words.
  StringScratchSpace scratch;
  OmissionTypeName typeName = getTypeNameForOmission(var->getInterfaceType());
  OmissionTypeName contextTypeName = getTypeNameForOmission(contextType);
  if (::omitNeedlessWords(name, { }, "", typeName, contextTypeName, { },
                          /*returnsSelf=*/false, true,
                          /*allPropertyNames=*/nullptr, scratch)) {
    return Context.getIdentifier(name);
  }

  return None;
}
