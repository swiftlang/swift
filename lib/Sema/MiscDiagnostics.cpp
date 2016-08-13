//===--- MiscDiagnostics.cpp - AST-Level Diagnostics ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

//===----------------------------------------------------------------------===//
// Diagnose assigning variable to itself.
//===----------------------------------------------------------------------===//

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
      unsigned level : 29;
      enum : unsigned {
        Function,
        MutatingMethod,
        SuperInit,
        SelfInit,
      };
      unsigned kind : 3;
    };

    // Partial applications of functions that are not permitted.  This is
    // tracked in post-order and unravelled as subsequent applications complete
    // the call (or not).
    llvm::SmallDenseMap<Expr*, PartialApplication,2> InvalidPartialApplications;

    ~DiagnoseWalker() {
      for (auto &unapplied : InvalidPartialApplications) {
        unsigned kind = unapplied.second.kind;
        TC.diagnose(unapplied.first->getLoc(),
                    diag::partial_application_of_function_invalid,
                    kind);
      }
    }

    /// If this is an application of a function that cannot be partially
    /// applied, arrange for us to check that it gets fully applied.
    void recordUnsupportedPartialApply(ApplyExpr *expr, Expr *fnExpr) {

      if (isa<OtherConstructorDeclRefExpr>(fnExpr)) {
        auto kind = expr->getArg()->isSuperExpr()
                  ? PartialApplication::SuperInit
                  : PartialApplication::SelfInit;

        // Partial applications of delegated initializers aren't allowed, and
        // don't really make sense to begin with.
        InvalidPartialApplications.insert({ expr, {1, kind} });
        return;
      }

      auto fnDeclRef = dyn_cast<DeclRefExpr>(fnExpr);
      if (!fnDeclRef)
        return;

      auto fn = dyn_cast<FuncDecl>(fnDeclRef->getDecl());
      if (!fn)
        return;

      unsigned kind =
        fn->isInstanceMember() ? PartialApplication::MutatingMethod
                               : PartialApplication::Function;

      // Functions with inout parameters cannot be partially applied.
      if (expr->getArg()->getType()->hasInOut()) {
        // We need to apply all argument clauses.
        InvalidPartialApplications.insert({
          fnExpr, {fn->getNumParameterLists(), kind}
        });
      }
    }

    /// This method is called in post-order over the AST to validate that
    /// methods are fully applied when they can't support partial application.
    void checkInvalidPartialApplication(Expr *E) {
      if (auto AE = dyn_cast<ApplyExpr>(E)) {
        Expr *fnExpr = AE->getSemanticFn();
        if (auto forceExpr = dyn_cast<ForceValueExpr>(fnExpr))
          fnExpr = forceExpr->getSubExpr()->getSemanticsProvidingExpr();
        if (auto dotSyntaxExpr = dyn_cast<DotSyntaxBaseIgnoredExpr>(fnExpr))
          fnExpr = dotSyntaxExpr->getRHS();

        // Check to see if this is a potentially unsupported partial
        // application.
        recordUnsupportedPartialApply(AE, fnExpr);

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
          InvalidPartialApplications.insert({ AE, {level - 1, kind} });
        }
        return;
      }

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
        checkNoEscapeParameterUse(DRE, nullptr);

        // Verify warn_unqualified_access uses.
        checkUnqualifiedAccessUse(DRE);
      }
      if (auto *MRE = dyn_cast<MemberRefExpr>(Base)) {
        if (isa<TypeDecl>(MRE->getMember().getDecl()))
          checkUseOfMetaTypeName(Base);

        // Check whether there are needless words that could be omitted.
        TC.checkOmitNeedlessWords(MRE);
      }
      if (isa<TypeExpr>(Base))
        checkUseOfMetaTypeName(Base);

      if (auto *SE = dyn_cast<SubscriptExpr>(E)) {
        // Implicit InOutExpr's are allowed in the base of a subscript expr.
        if (auto *IOE = dyn_cast<InOutExpr>(SE->getBase()))
          if (IOE->isImplicit())
            AcceptableInOutExprs.insert(IOE);
      }


      // Check function calls, looking through implicit conversions on the
      // function and inspecting the arguments directly.
      if (auto *Call = dyn_cast<ApplyExpr>(E)) {
        // Warn about surprising implicit optional promotions.
        checkOptionalPromotions(Call);
        
        // Check for tuple splat.
        checkTupleSplat(Call);

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
        else if (auto *PE = dyn_cast<ParenExpr>(Arg))
          arguments = PE->getSubExpr();
        else
          arguments = Call->getArg();

        // Check each argument.
        for (auto arg : arguments) {
          // InOutExpr's are allowed in argument lists directly.
          if (auto *IOE = dyn_cast<InOutExpr>(arg)) {
            if (isa<CallExpr>(Call))
              AcceptableInOutExprs.insert(IOE);
          }
          // InOutExprs can be wrapped in some implicit casts.
          Expr *unwrapped = arg;
          if (auto *IIO = dyn_cast<InjectIntoOptionalExpr>(arg))
            unwrapped = IIO->getSubExpr();
          if (auto *ICO = dyn_cast<ImplicitConversionExpr>(unwrapped)) {
            if (isa<InOutToPointerExpr>(ICO) ||
                isa<ArrayToPointerExpr>(ICO) ||
                isa<ErasureExpr>(ICO))
              if (auto *IOE = dyn_cast<InOutExpr>(ICO->getSubExpr()))
                AcceptableInOutExprs.insert(IOE);
          }

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

        // Check whether there are needless words that could be omitted.
        TC.checkOmitNeedlessWords(Call);
      }
      
      // If we have an assignment expression, scout ahead for acceptable _'s.
      if (auto *AE = dyn_cast<AssignExpr>(E))
        markAcceptableDiscardExprs(AE->getDest());

      /// Diagnose a '_' that isn't on the immediate LHS of an assignment.
      if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(E)) {
        if (!CorrectDiscardAssignmentExprs.count(DAE) &&
            !DAE->getType()->is<ErrorType>())
          TC.diagnose(DAE->getLoc(), diag::discard_expr_outside_of_assignment);
      }

      // Diagnose an '&' that isn't in an argument lists.
      if (auto *IOE = dyn_cast<InOutExpr>(E)) {
        if (!IOE->isImplicit() && !AcceptableInOutExprs.count(IOE) &&
            !IOE->getType()->is<ErrorType>())
          TC.diagnose(IOE->getLoc(), diag::inout_expr_outside_of_call)
            .highlight(IOE->getSubExpr()->getSourceRange());
      }

      // Diagnose 'self.init' or 'super.init' nested in another expression.
      if (auto *rebindSelfExpr = dyn_cast<RebindSelfInConstructorExpr>(E)) {
        if (!Parent.isNull() || !IsExprStmt) {
          bool isChainToSuper;
          (void)rebindSelfExpr->getCalledConstructor(isChainToSuper);
          TC.diagnose(E->getLoc(), diag::init_delegation_nested,
                      isChainToSuper, !IsExprStmt);
        }
      }

      return { true, E };
    }

    Expr *walkToExprPost(Expr *E) override {
      checkInvalidPartialApplication(E);
      return E;
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
        TC.diagnose(c->getLoc(), diag::collection_literal_heterogenous,
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


    /// Warn on tuple splat, which is deprecated.  For example:
    ///
    ///     func f(a : Int, _ b : Int) {}
    ///     let x = (1,2)
    ///     f(x)
    ///
    void checkTupleSplat(ApplyExpr *Call) {
      auto FT = Call->getFn()->getType()->getAs<AnyFunctionType>();
      // If this wasn't type checked correctly then don't worry about it.
      if (!FT) return;

      // If we're passing multiple parameters, then this isn't a tuple splat.
      auto arg = Call->getArg()->getSemanticsProvidingExpr();
      if (isa<TupleExpr>(arg) || isa<TupleShuffleExpr>(arg))
        return;

      // We care about whether the parameter list of the callee syntactically
      // has more than one argument.  It has to *syntactically* have a tuple
      // type as its argument.  A ParenType wrapping a TupleType is a single
      // parameter. 
      if (isa<TupleType>(FT->getInput().getPointer())) {
        auto TT = FT->getInput()->getAs<TupleType>();
        if (TT->getNumElements() > 1) {
          TC.diagnose(Call->getLoc(), diag::tuple_splat_use,
                      TT->getNumElements())
            .highlight(Call->getArg()->getSourceRange());
        }
      }
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
      // This only cares about declarations of noescape function type.
      auto AFT = DRE->getDecl()->getType()->getAs<AnyFunctionType>();
      if (!AFT || !AFT->isNoEscape())
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
                  DRE->getDecl()->getName(), isa<ParamDecl>(DRE->getDecl()));

      // If we're a parameter, emit a helpful fixit to add @escaping
      auto paramDecl = dyn_cast<ParamDecl>(DRE->getDecl());
      auto isAutoClosure = AFT->isAutoClosure();
      if (paramDecl && !isAutoClosure) {
        TC.diagnose(paramDecl->getStartLoc(), diag::noescape_parameter,
                    paramDecl->getName())
            .fixItInsert(paramDecl->getTypeLoc().getSourceRange().Start,
                         "@escaping ");
      } else if (isAutoClosure)
        // TODO: add in a fixit for autoclosure
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
      // - member references T.foo, T.Type, T.self, etc.
      // - constructor calls T()
      if (auto *ParentExpr = Parent.getAsExpr()) {
        // This is the white-list of accepted syntactic forms.
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

        // Note: as a specific hack, produce a warning + Fix-It for
        // the missing ".self" as the subexpression of a parenthesized
        // expression, which is a historical bug.
        if (isa<ParenExpr>(ParentExpr) && CallArgs.count(ParentExpr) > 0) {
          TC.diagnose(E->getEndLoc(), diag::warn_value_of_metatype_missing_self,
                      E->getType()->getRValueInstanceType())
            .fixItInsertAfter(E->getEndLoc(), ".self");
          return;
        }
      }

      // Is this a protocol metatype?

      TC.diagnose(E->getStartLoc(), diag::value_of_metatype_type);

      // Add fix-t to insert '()', unless this is a protocol metatype.
      bool isProtocolMetatype = false;
      if (auto metaTy = E->getType()->getAs<MetatypeType>())
        isProtocolMetatype = metaTy->getInstanceType()->is<ProtocolType>();
      if (!isProtocolMetatype) {
        TC.diagnose(E->getEndLoc(), diag::add_parens_to_type)
          .fixItInsertAfter(E->getEndLoc(), "()");
      }

      // Add fix-it to insert ".self".
      TC.diagnose(E->getEndLoc(), diag::add_self_to_type)
        .fixItInsertAfter(E->getEndLoc(), ".self");
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
          VD->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
      if (!declParent) {
        assert(VD->getDeclContext()->isModuleScopeContext());
        declParent = VD->getDeclContext()->getParentModule();
      }

      TC.diagnose(DRE->getLoc(), diag::warn_unqualified_access,
                  VD->getName(), VD->getDescriptiveKind(),
                  declParent->getDescriptiveKind(), declParent->getFullName());
      TC.diagnose(VD, diag::decl_declared_here, VD->getFullName());

      if (VD->getDeclContext()->isTypeContext()) {
        TC.diagnose(DRE->getLoc(), diag::fix_unqualified_access_member)
          .fixItInsert(DRE->getStartLoc(), "self.");
      }

      DeclContext *topLevelContext = DC->getModuleScopeContext();
      UnqualifiedLookup lookup(VD->getBaseName(), topLevelContext, &TC,
                               /*knownPrivate*/true);

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
    
    /// Return true if this is 'nil' type checked as an Optional.  This looks
    /// like this:
    ///   (call_expr implicit type='Int?'
    ///     (constructor_ref_call_expr implicit
    ///       (declref_expr implicit decl=Optional.init(nilLiteral:)
    static bool isTypeCheckedOptionalNil(Expr *E) {
      auto CE = dyn_cast<CallExpr>(E->getSemanticsProvidingExpr());
      if (!CE || !CE->isImplicit())
        return false;
      
      auto CRCE = dyn_cast<ConstructorRefCallExpr>(CE->getSemanticFn());
      if (!CRCE || !CRCE->isImplicit()) return false;
      
      auto DRE = dyn_cast<DeclRefExpr>(CRCE->getSemanticFn());
      
      SmallString<32> NameBuffer;
      auto name = DRE->getDecl()->getFullName().getString(NameBuffer);
      return name == "init(nilLiteral:)";
    }
    
    
    /// Warn about surprising implicit optional promotions involving operands to
    /// calls.  Specifically, we warn about these expressions when the 'x'
    /// operand is implicitly promoted to optional:
    ///
    ///       x ?? y
    ///       x == nil    // also !=
    ///
    void checkOptionalPromotions(ApplyExpr *call) {
      auto DRE = dyn_cast<DeclRefExpr>(call->getSemanticFn());
      auto args = dyn_cast<TupleExpr>(call->getArg());
      if (!DRE || !DRE->getDecl()->isOperator() ||
          !args || args->getNumElements() != 2)
        return;
      
      auto lhs = args->getElement(0);
      auto rhs = args->getElement(1);
      auto calleeName = DRE->getDecl()->getName().str();
      
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
          if (DRE->getAccessSemantics() != AccessSemantics::DirectToStorage) {
            bool shouldDiagnose = false;
            // Warn about any property access in the getter.
            if (Accessor->isGetter())
              shouldDiagnose = !isStore;
            // Warn about stores in the setter, but allow loads.
            if (Accessor->isSetter())
              shouldDiagnose = isStore;

            // But silence the warning if the base was explicitly qualified.
            if (dyn_cast_or_null<DotSyntaxBaseIgnoredExpr>(Parent.getAsExpr()))
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
              Accessor->getAccessorKind() == AccessorKind::IsWillSet) {
            TC.diagnose(E->getLoc(), diag::store_in_willset, Var->getName());
          }
        }


      } else if (auto *MRE = dyn_cast<MemberRefExpr>(subExpr)) {
        // Handle instance and type computed variables.
        // Find MemberRefExprs that have an implicit "self" base.
        if (MRE->getMember().getDecl() == Var &&
            isa<DeclRefExpr>(MRE->getBase()) &&
            MRE->getBase()->isImplicit()) {
          
          if (MRE->getAccessSemantics() != AccessSemantics::DirectToStorage) {
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
              Accessor->getAccessorKind() == AccessorKind::IsWillSet) {
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

/// Diagnose an argument labeling issue, returning true if we successfully
/// diagnosed the issue.
bool swift::diagnoseArgumentLabelError(TypeChecker &TC, const Expr *expr,
                                       ArrayRef<Identifier> newNames,
                                       bool isSubscript,
                                       InFlightDiagnostic *existingDiag) {
  Optional<InFlightDiagnostic> diagOpt;
  auto getDiag = [&]() -> InFlightDiagnostic & {
    if (existingDiag)
      return *existingDiag;
    return *diagOpt;
  };

  auto tuple = dyn_cast<TupleExpr>(expr);
  if (!tuple) {
    llvm::SmallString<16> str;
    // If the diagnostic is local, flush it before returning.
    // This makes sure it's emitted before 'str' is destroyed.
    SWIFT_DEFER { diagOpt.reset(); };

    if (newNames[0].empty()) {
      // This is probably a conversion from a value of labeled tuple type to
      // a scalar.
      // FIXME: We want this issue to disappear completely when single-element
      // labelled tuples go away.
      if (auto tupleTy = expr->getType()->getRValueType()->getAs<TupleType>()) {
        int scalarFieldIdx = tupleTy->getElementForScalarInit();
        if (scalarFieldIdx >= 0) {
          auto &field = tupleTy->getElement(scalarFieldIdx);
          if (field.hasName()) {
            str = ".";
            str += field.getName().str();
            if (!existingDiag) {
              diagOpt.emplace(TC.diagnose(expr->getStartLoc(),
                                          diag::extra_named_single_element_tuple,
                                          field.getName().str()));
            }
            getDiag().fixItInsertAfter(expr->getEndLoc(), str);
            return true;
          }
        }
      }

      // We don't know what to do with this.
      return false;
    }

    // This is a scalar-to-tuple conversion. Add the  name.  We "know"
    // that we're inside a ParenExpr, because ParenExprs are required
    // by the syntax and locator resolution looks through on level of
    // them.

    // Look through the paren expression, if there is one.
    if (auto parenExpr = dyn_cast<ParenExpr>(expr))
      expr = parenExpr->getSubExpr();

    str += newNames[0].str();
    str += ": ";
    if (!existingDiag) {
      diagOpt.emplace(TC.diagnose(expr->getStartLoc(),
                                  diag::missing_argument_labels,
                                  false, str.str().drop_back(), isSubscript));
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
      diagOpt.emplace(TC.diagnose(expr->getLoc(), diag::wrong_argument_labels,
                                  plural, haveStr, expectedStr, isSubscript));
    } else if (numMissing > 0) {
      StringRef missingStr = missingBuffer;
      diagOpt.emplace(TC.diagnose(expr->getLoc(), diag::missing_argument_labels,
                                  plural, missingStr, isSubscript));
    } else {
      assert(numExtra > 0);
      StringRef extraStr = extraBuffer;
      diagOpt.emplace(TC.diagnose(expr->getLoc(), diag::extra_argument_labels,
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

bool swift::fixItOverrideDeclarationTypes(TypeChecker &TC,
                                          InFlightDiagnostic &diag,
                                          ValueDecl *decl,
                                          const ValueDecl *base) {
  // For now, just rewrite cases where the base uses a value type and the
  // override uses a reference type, and the value type is bridged to the
  // reference type. This is a way to migrate code that makes use of types
  // that previously were not bridged to value types.
  auto checkValueReferenceType = [&](Type overrideTy, Type baseTy,
                                     SourceRange typeRange) -> bool {
    if (typeRange.isInvalid())
      return false;

    auto normalizeType = [](Type ty) -> Type {
      ty = ty->getInOutObjectType();
      if (Type unwrappedTy = ty->getAnyOptionalObjectType())
        ty = unwrappedTy;
      return ty;
    };

    // Is the base type bridged?
    Type normalizedBaseTy = normalizeType(baseTy);
    const DeclContext *DC = decl->getDeclContext();
    Optional<Type> maybeBridged =
        TC.Context.getBridgedToObjC(DC, normalizedBaseTy, &TC);

    // ...and just knowing that it's bridged isn't good enough if we don't
    // know what it's bridged /to/. Also, don't do this check for trivial
    // bridging---that doesn't count.
    Type bridged = maybeBridged.getValueOr(Type());
    if (!bridged || bridged->isEqual(normalizedBaseTy))
      return false;

    // ...and is it bridged to the overridden type?
    Type normalizedOverrideTy = normalizeType(overrideTy);
    if (!bridged->isEqual(normalizedOverrideTy)) {
      // If both are nominal types, check again, ignoring generic arguments.
      auto *overrideNominal = normalizedOverrideTy->getAnyNominal();
      if (!overrideNominal || bridged->getAnyNominal() != overrideNominal) {
        return false;
      }
    }

    Type newOverrideTy = baseTy;

    // Preserve optionality if we're dealing with a simple type.
    OptionalTypeKind OTK;
    if (Type unwrappedTy = newOverrideTy->getAnyOptionalObjectType())
      newOverrideTy = unwrappedTy;
    if (overrideTy->getAnyOptionalObjectType(OTK))
      newOverrideTy = OptionalType::get(OTK, newOverrideTy);

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
        // The overriden function type should be escaping.
        !baseFnTy->getExtInfo().isNoEscape()) {
      diag.fixItInsert(typeRange.Start, "@escaping ");
      return true;
    }
    return false;
  };

  auto checkType = [&](Type overrideTy, Type baseTy,
                       SourceRange typeRange) -> bool {
    return checkValueReferenceType(overrideTy, baseTy, typeRange) ||
      checkTypeMissingEscaping(overrideTy, baseTy, typeRange);
  };

  if (auto *var = dyn_cast<VarDecl>(decl)) {
    SourceRange typeRange = var->getTypeSourceRangeForDiagnostics();
    return checkType(var->getType(), base->getType(), typeRange);
  }

  if (auto *fn = dyn_cast<AbstractFunctionDecl>(decl)) {
    auto *baseFn = cast<AbstractFunctionDecl>(base);
    bool fixedAny = false;
    if (fn->getParameterLists().back()->size() ==
        baseFn->getParameterLists().back()->size()) {
      for_each(*fn->getParameterLists().back(),
               *baseFn->getParameterLists().back(),
               [&](ParamDecl *param, const ParamDecl *baseParam) {
        fixedAny |= fixItOverrideDeclarationTypes(TC, diag, param, baseParam);
      });
    }
    if (auto *method = dyn_cast<FuncDecl>(decl)) {
      auto *baseMethod = cast<FuncDecl>(base);
      fixedAny |= checkType(method->getBodyResultType(),
                            baseMethod->getResultType(),
                            method->getBodyResultTypeLoc().getSourceRange());
    }
    return fixedAny;
  }

  if (auto *subscript = dyn_cast<SubscriptDecl>(decl)) {
    auto *baseSubscript = cast<SubscriptDecl>(base);
    bool fixedAny = false;
    for_each(*subscript->getIndices(),
             *baseSubscript->getIndices(),
             [&](ParamDecl *param, const ParamDecl *baseParam) {
      fixedAny |= fixItOverrideDeclarationTypes(TC, diag, param, baseParam);
    });
    fixedAny |= checkType(subscript->getElementType(),
                          baseSubscript->getElementType(),
                          subscript->getElementTypeLoc().getSourceRange());
    return fixedAny;
  }

  llvm_unreachable("unknown overridable member");
}



//===----------------------------------------------------------------------===//
// Diagnose availability.
//===----------------------------------------------------------------------===//

void swift::fixItAvailableAttrRename(TypeChecker &TC,
                                     InFlightDiagnostic &diag,
                                     SourceRange referenceRange,
                                     const AvailableAttr *attr,
                                     const ApplyExpr *call) {
  ParsedDeclName parsed = swift::parseDeclName(attr->Rename);
  if (!parsed)
    return;

  bool originallyWasKnownOperatorExpr = false;
  if (call) {
    originallyWasKnownOperatorExpr =
        isa<BinaryExpr>(call) ||
        isa<PrefixUnaryExpr>(call) ||
        isa<PostfixUnaryExpr>(call);
  }
  if (parsed.isOperator() != originallyWasKnownOperatorExpr)
    return;

  SourceManager &sourceMgr = TC.Context.SourceMgr;

  if (parsed.isInstanceMember()) {
    // Replace the base of the call with the "self argument".
    // We can only do a good job with the fix-it if we have the whole call
    // expression.
    // FIXME: Should we be validating the ContextName in some way?
    if (!dyn_cast_or_null<CallExpr>(call))
      return;

    unsigned selfIndex = parsed.SelfIndex.getValue();
    const Expr *selfExpr = nullptr;
    SourceLoc removeRangeStart;
    SourceLoc removeRangeEnd;

    const Expr *argExpr = call->getArg();
    if (auto args = dyn_cast<TupleExpr>(argExpr)) {
      size_t numElementsWithinParens = args->getNumElements();
      numElementsWithinParens -= args->hasTrailingClosure();
      if (selfIndex >= numElementsWithinParens)
        return;

      if (parsed.IsGetter) {
        if (numElementsWithinParens != 1)
          return;
      } else if (parsed.IsSetter) {
        if (numElementsWithinParens != 2)
          return;
      } else {
        if (parsed.ArgumentLabels.size() != args->getNumElements() - 1)
          return;
      }

      selfExpr = args->getElement(selfIndex);

      if (selfIndex + 1 == numElementsWithinParens) {
        if (selfIndex > 0) {
          // Remove from the previous comma to the close-paren (half-open).
          removeRangeStart = args->getElement(selfIndex-1)->getEndLoc();
          removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                        removeRangeStart);
        } else {
          // Remove from after the open paren to the close paren (half-open).
          removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                        argExpr->getStartLoc());
        }

        // Prefer the r-paren location, so that we get the right behavior when
        // there's a trailing closure, but handle some implicit cases too.
        removeRangeEnd = args->getRParenLoc();
        if (removeRangeEnd.isInvalid())
          removeRangeEnd = args->getEndLoc();

      } else {
        // Remove from the label to the start of the next argument (half-open).
        SourceLoc labelLoc = args->getElementNameLoc(selfIndex);
        if (labelLoc.isValid())
          removeRangeStart = labelLoc;
        else
          removeRangeStart = selfExpr->getStartLoc();

        SourceLoc nextLabelLoc = args->getElementNameLoc(selfIndex + 1);
        if (nextLabelLoc.isValid())
          removeRangeEnd = nextLabelLoc;
        else
          removeRangeEnd = args->getElement(selfIndex + 1)->getStartLoc();
      }

      // Avoid later argument label fix-its for this argument.
      if (!parsed.isPropertyAccessor()) {
        Identifier oldLabel = args->getElementName(selfIndex);
        StringRef oldLabelStr;
        if (!oldLabel.empty())
          oldLabelStr = oldLabel.str();
        parsed.ArgumentLabels.insert(parsed.ArgumentLabels.begin() + selfIndex,
                                     oldLabelStr);
      }

    } else {
      if (selfIndex != 0 || !parsed.ArgumentLabels.empty())
        return;
      selfExpr = cast<ParenExpr>(argExpr)->getSubExpr();
      // Remove from after the open paren to the close paren (half-open).
      removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                    argExpr->getStartLoc());
      removeRangeEnd = argExpr->getEndLoc();
    }

    if (auto *inoutSelf = dyn_cast<InOutExpr>(selfExpr))
      selfExpr = inoutSelf->getSubExpr();

    CharSourceRange selfExprRange =
        Lexer::getCharSourceRangeFromSourceRange(sourceMgr,
                                                 selfExpr->getSourceRange());
    bool needsParens = !selfExpr->canAppendCallParentheses();

    SmallString<64> selfReplace;
    if (needsParens)
      selfReplace.push_back('(');
    selfReplace += sourceMgr.extractText(selfExprRange);
    if (needsParens)
      selfReplace.push_back(')');
    selfReplace.push_back('.');
    selfReplace += parsed.BaseName;
    diag.fixItReplace(call->getFn()->getSourceRange(), selfReplace);

    if (!parsed.isPropertyAccessor())
      diag.fixItRemoveChars(removeRangeStart, removeRangeEnd);

    // Continue on to diagnose any argument label renames.

  } else if (parsed.BaseName == TC.Context.Id_init.str() &&
             dyn_cast_or_null<CallExpr>(call)) {
    // For initializers, replace with a "call" of the context type...but only
    // if we know we're doing a call (rather than a first-class reference).
    if (parsed.isMember()) {
      diag.fixItReplace(call->getFn()->getSourceRange(), parsed.ContextName);

    } else if (auto *dotCall = dyn_cast<DotSyntaxCallExpr>(call->getFn())) {
      SourceLoc removeLoc = dotCall->getDotLoc();
      if (removeLoc.isInvalid())
        return;

      diag.fixItRemove(SourceRange(removeLoc, dotCall->getFn()->getEndLoc()));
    } else if (!isa<ConstructorRefCallExpr>(call->getFn())) {
      return;
    }

    // Continue on to diagnose any constructor argument label renames.
    
  } else {
    // Just replace the base name.
    SmallString<64> baseReplace;
    if (!parsed.ContextName.empty()) {
      baseReplace += parsed.ContextName;
      baseReplace += '.';
    }
    baseReplace += parsed.BaseName;
    if (parsed.IsFunctionName && parsed.ArgumentLabels.empty() && !call) {
      // If we're going from a var to a function with no arguments, emit an
      // empty parameter list.
      baseReplace += "()";
    }
    diag.fixItReplace(referenceRange, baseReplace);
  }

  if (!dyn_cast_or_null<CallExpr>(call))
    return;

  const Expr *argExpr = call->getArg();
  if (parsed.IsGetter) {
    diag.fixItRemove(argExpr->getSourceRange());
    return;
  }

  if (parsed.IsSetter) {
    const Expr *newValueExpr = nullptr;

    if (auto args = dyn_cast<TupleExpr>(argExpr)) {
      size_t newValueIndex = 0;
      if (parsed.isInstanceMember()) {
        assert(parsed.SelfIndex.getValue() == 0 ||
               parsed.SelfIndex.getValue() == 1);
        newValueIndex = !parsed.SelfIndex.getValue();
      }
      newValueExpr = args->getElement(newValueIndex);
    } else {
      newValueExpr = cast<ParenExpr>(argExpr)->getSubExpr();
    }

    diag.fixItReplaceChars(argExpr->getStartLoc(), newValueExpr->getStartLoc(),
                           " = ");
    diag.fixItRemoveChars(Lexer::getLocForEndOfToken(sourceMgr,
                                                     newValueExpr->getEndLoc()),
                          Lexer::getLocForEndOfToken(sourceMgr,
                                                     argExpr->getEndLoc()));
    return;
  }

  if (!parsed.IsFunctionName)
    return;

  SmallVector<Identifier, 4> argumentLabelIDs;
  std::transform(parsed.ArgumentLabels.begin(), parsed.ArgumentLabels.end(),
                 std::back_inserter(argumentLabelIDs),
                 [&TC](StringRef labelStr) -> Identifier {
    return labelStr.empty() ? Identifier() : TC.Context.getIdentifier(labelStr);
  });

  if (auto args = dyn_cast<TupleShuffleExpr>(argExpr)) {
    argExpr = args->getSubExpr();

    // Coerce the `argumentLabelIDs` to the user supplied arguments.
    // e.g:
    //   @available(.., renamed: "new(w:x:y:z:)")
    //   func old(a: Int, b: Int..., c: String="", d: Int=0){}
    //   old(a: 1, b: 2, 3, 4, d: 5)
    // coerce
    //   argumentLabelIDs = {"w", "x", "y", "z"}
    // to
    //   argumentLabelIDs = {"w", "x", "", "", "z"}
    auto elementMap = args->getElementMapping();
    if (elementMap.size() != argumentLabelIDs.size()) {
      // Mismatched lengths; give up.
      return;
    }
    auto I = argumentLabelIDs.begin();
    for (auto shuffleIdx : elementMap) {
      switch (shuffleIdx) {
      case TupleShuffleExpr::DefaultInitialize:
      case TupleShuffleExpr::CallerDefaultInitialize:
        // Defaulted: remove param label of it.
        I = argumentLabelIDs.erase(I);
        break;
      case TupleShuffleExpr::Variadic: {
        auto variadicArgsNum = args->getVariadicArgs().size();
        if (variadicArgsNum == 0) {
          // No arguments: Remove param label of it.
          I = argumentLabelIDs.erase(I);
        } else if (variadicArgsNum == 1) {
          // One argument: Just advance.
          ++I;
        } else {
          // Two or more arguments: Insert empty labels after the first one.
          I = argumentLabelIDs.insert(++I, --variadicArgsNum, Identifier());
          I += variadicArgsNum;
        }
        break;
      }
      default:
        // Normal: Just advance.
        assert(shuffleIdx == (I - argumentLabelIDs.begin()) &&
               "SE-0060 guarantee");
        ++I;
        break;
      }
    }
  }

  if (auto args = dyn_cast<TupleExpr>(argExpr)) {
    if (argumentLabelIDs.size() != args->getNumElements()) {
      // Mismatched lengths; give up.
      return;
    }

    auto argumentLabelsToCheck = llvm::makeArrayRef(argumentLabelIDs);
    // The argument label for a trailing closure is ignored.
    if (args->hasTrailingClosure())
      argumentLabelsToCheck = argumentLabelsToCheck.drop_back();

    if (args->hasElementNames()) {
      if (std::equal(argumentLabelsToCheck.begin(), argumentLabelsToCheck.end(),
                     args->getElementNames().begin())) {
        // Already matching.
        return;
      }

    } else {
      if (std::all_of(argumentLabelsToCheck.begin(),argumentLabelsToCheck.end(),
                      std::mem_fn(&Identifier::empty))) {
        // Already matching (as in, there are no labels).
        return;
      }
    }

  } else if (auto args = dyn_cast<ParenExpr>(argExpr)) {
    if (args->hasTrailingClosure()) {
      // The argument label for a trailing closure is ignored.
      return;
    }

    if (argumentLabelIDs.size() != 1) {
      // Mismatched lengths; give up.
      return;
    }

    if (argumentLabelIDs.front().empty()) {
      // Already matching (no labels).
      return;
    }
  } else {
    llvm_unreachable("Unexpected arg expression");
  }

  diagnoseArgumentLabelError(TC, argExpr, argumentLabelIDs, false, &diag);
}

// Must be kept in sync with diag::availability_decl_unavailable_rename and
// others.
namespace {
  enum class ReplacementDeclKind : unsigned {
    None,
    InstanceMethod,
    Property,
  };
}

static Optional<ReplacementDeclKind>
describeRename(ASTContext &ctx, const AvailableAttr *attr, const ValueDecl *D,
               SmallVectorImpl<char> &nameBuf) {
  ParsedDeclName parsed = swift::parseDeclName(attr->Rename);
  if (!parsed)
    return None;

  // Only produce special descriptions for renames to
  // - instance members
  // - properties (or global bindings)
  // - class/static methods
  // - initializers, unless the original was known to be an initializer
  // Leave non-member renames alone, as well as renames from top-level types
  // and bindings to member types and class/static properties.
  if (!(parsed.isInstanceMember() || parsed.isPropertyAccessor() ||
        (parsed.isMember() && parsed.IsFunctionName) ||
        (parsed.BaseName == ctx.Id_init.str() &&
         !dyn_cast_or_null<ConstructorDecl>(D)))) {
    return None;
  }

  llvm::raw_svector_ostream name(nameBuf);

  if (!parsed.ContextName.empty())
    name << parsed.ContextName << '.';

  if (parsed.IsFunctionName) {
    // FIXME: duplicated from above.
    SmallVector<Identifier, 4> argumentLabelIDs;
    std::transform(parsed.ArgumentLabels.begin(), parsed.ArgumentLabels.end(),
                   std::back_inserter(argumentLabelIDs),
                   [&ctx](StringRef labelStr) -> Identifier {
      return labelStr.empty() ? Identifier() : ctx.getIdentifier(labelStr);
    });
    name << DeclName(ctx, ctx.getIdentifier(parsed.BaseName), argumentLabelIDs);
  } else {
    name << parsed.BaseName;
  }

  if (parsed.isMember() && parsed.isPropertyAccessor())
    return ReplacementDeclKind::Property;
  if (parsed.isInstanceMember() && parsed.IsFunctionName)
    return ReplacementDeclKind::InstanceMethod;

  // We don't have enough information.
  return ReplacementDeclKind::None;
}

void TypeChecker::diagnoseDeprecated(SourceRange ReferenceRange,
                                     const DeclContext *ReferenceDC,
                                     const AvailableAttr *Attr,
                                     DeclName Name,
                                     const ApplyExpr *Call) {
  // We match the behavior of clang to not report deprecation warnings
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (isInsideDeprecatedDeclaration(ReferenceRange, ReferenceDC)) {
    return;
  }

  if (!Context.LangOpts.DisableAvailabilityChecking) {
    AvailabilityContext RunningOSVersions =
        overApproximateAvailabilityAtLocation(ReferenceRange.Start,ReferenceDC);
    if (RunningOSVersions.isKnownUnreachable()) {
      // Suppress a deprecation warning if the availability checking machinery
      // thinks the reference program location will not execute on any
      // deployment target for the current platform.
      return;
    }
  }

  StringRef Platform = Attr->prettyPlatformString();
  clang::VersionTuple DeprecatedVersion;
  if (Attr->Deprecated)
    DeprecatedVersion = Attr->Deprecated.getValue();

  if (Attr->Message.empty() && Attr->Rename.empty()) {
    diagnose(ReferenceRange.Start, diag::availability_deprecated, Name,
             Attr->hasPlatform(), Platform, Attr->Deprecated.hasValue(),
             DeprecatedVersion)
      .highlight(Attr->getRange());
    return;
  }

  SmallString<32> newNameBuf;
  Optional<ReplacementDeclKind> replacementDeclKind =
    describeRename(Context, Attr, /*decl*/nullptr, newNameBuf);
  StringRef newName = replacementDeclKind ? newNameBuf.str() : Attr->Rename;

  if (!Attr->Message.empty()) {
    EncodedDiagnosticMessage EncodedMessage(Attr->Message);
    diagnose(ReferenceRange.Start, diag::availability_deprecated_msg, Name,
             Attr->hasPlatform(), Platform, Attr->Deprecated.hasValue(),
             DeprecatedVersion, EncodedMessage.Message)
      .highlight(Attr->getRange());
  } else {
    unsigned rawReplaceKind = static_cast<unsigned>(
        replacementDeclKind.getValueOr(ReplacementDeclKind::None));
    diagnose(ReferenceRange.Start, diag::availability_deprecated_rename, Name,
             Attr->hasPlatform(), Platform, Attr->Deprecated.hasValue(),
             DeprecatedVersion, replacementDeclKind.hasValue(), rawReplaceKind,
             newName)
      .highlight(Attr->getRange());
  }

  if (!Attr->Rename.empty()) {
    auto renameDiag = diagnose(ReferenceRange.Start,
                               diag::note_deprecated_rename,
                               newName);
    fixItAvailableAttrRename(*this, renameDiag, ReferenceRange, Attr, Call);
  }
}


void TypeChecker::diagnoseUnavailableOverride(ValueDecl *override,
                                              const ValueDecl *base,
                                              const AvailableAttr *attr) {
  if (attr->Rename.empty()) {
    if (attr->Message.empty())
      diagnose(override, diag::override_unavailable, override->getName());
    else
      diagnose(override, diag::override_unavailable_msg,
               override->getName(), attr->Message);
    diagnose(base, diag::availability_marked_unavailable,
             base->getFullName());
    return;
  }

  diagnoseExplicitUnavailability(base, override->getLoc(),
                                 override->getDeclContext(),
                                 [&](InFlightDiagnostic &diag) {
    ParsedDeclName parsedName = parseDeclName(attr->Rename);
    if (!parsedName || parsedName.isPropertyAccessor() ||
        parsedName.isMember() || parsedName.isOperator()) {
      return;
    }

    // Only initializers should be named 'init'.
    if (isa<ConstructorDecl>(override) ^
        (parsedName.BaseName == Context.Id_init.str())) {
      return;
    }

    if (!parsedName.IsFunctionName) {
      diag.fixItReplace(override->getNameLoc(), parsedName.BaseName);
      return;
    }

    DeclName newName = parsedName.formDeclName(Context);
    size_t numArgs = override->getFullName().getArgumentNames().size();
    if (!newName || newName.getArgumentNames().size() != numArgs)
      return;

    fixDeclarationName(diag, override, newName);
  });
}

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
bool TypeChecker::diagnoseExplicitUnavailability(const ValueDecl *D,
                                                 SourceRange R,
                                                 const DeclContext *DC,
                                                 const ApplyExpr *call) {
  return diagnoseExplicitUnavailability(D, R, DC,
                                        [=](InFlightDiagnostic &diag) {
    fixItAvailableAttrRename(*this, diag, R, AvailableAttr::isUnavailable(D),
                             call);
  });
}

bool TypeChecker::diagnoseExplicitUnavailability(
    const ValueDecl *D,
    SourceRange R,
    const DeclContext *DC,
    llvm::function_ref<void(InFlightDiagnostic &)> attachRenameFixIts) {
  auto *Attr = AvailableAttr::isUnavailable(D);
  if (!Attr)
    return false;

  // Suppress the diagnostic if we are in synthesized code inside
  // a synthesized function and the reference is lexically
  // contained in a declaration that is itself marked unavailable.
  // The right thing to do here is to not synthesize that code in the
  // first place. rdar://problem/20491640
  if (R.isInvalid() && isInsideImplicitFunction(R, DC) &&
      isInsideUnavailableDeclaration(R, DC)) {
    return false;
  }

  SourceLoc Loc = R.Start;
  auto Name = D->getFullName();

  switch (Attr->getUnconditionalAvailability()) {
  case UnconditionalAvailabilityKind::Deprecated:
    break;

  case UnconditionalAvailabilityKind::None:
  case UnconditionalAvailabilityKind::Unavailable:
  case UnconditionalAvailabilityKind::UnavailableInCurrentSwift:
  case UnconditionalAvailabilityKind::UnavailableInSwift: {
    bool inSwift = (Attr->getUnconditionalAvailability() ==
                    UnconditionalAvailabilityKind::UnavailableInSwift);

    if (!Attr->Rename.empty()) {
      SmallString<32> newNameBuf;
      Optional<ReplacementDeclKind> replaceKind =
          describeRename(Context, Attr, D, newNameBuf);
      unsigned rawReplaceKind = static_cast<unsigned>(
          replaceKind.getValueOr(ReplacementDeclKind::None));
      StringRef newName = replaceKind ? newNameBuf.str() : Attr->Rename;

      if (Attr->Message.empty()) {
        auto diag = diagnose(Loc, diag::availability_decl_unavailable_rename,
                             Name, replaceKind.hasValue(), rawReplaceKind,
                             newName);
        attachRenameFixIts(diag);
      } else {
        auto diag = diagnose(Loc, diag::availability_decl_unavailable_rename_msg,
                             Name, replaceKind.hasValue(), rawReplaceKind,
                             newName, Attr->Message);
        attachRenameFixIts(diag);
      }
    } else if (Attr->Message.empty()) {
      diagnose(Loc, inSwift ? diag::availability_decl_unavailable_in_swift
                            : diag::availability_decl_unavailable,
               Name).highlight(R);
    } else {
      EncodedDiagnosticMessage EncodedMessage(Attr->Message);
      diagnose(Loc, inSwift ? diag::availability_decl_unavailable_in_swift_msg
                            : diag::availability_decl_unavailable_msg,
               Name, EncodedMessage.Message)
        .highlight(R);
    }
    break;
  }
  }

  auto MinVersion = Context.LangOpts.getMinPlatformVersion();
  switch (Attr->getMinVersionAvailability(MinVersion)) {
  case MinVersionComparison::Available:
  case MinVersionComparison::PotentiallyUnavailable:
    llvm_unreachable("These aren't considered unavailable");

  case MinVersionComparison::Unavailable:
    diagnose(D, diag::availability_marked_unavailable, Name)
        .highlight(Attr->getRange());
    break;

  case MinVersionComparison::Obsoleted:
    // FIXME: Use of the platformString here is non-awesome for application
    // extensions.
    diagnose(D, diag::availability_obsoleted, Name,
             Attr->prettyPlatformString(),
             *Attr->Obsoleted).highlight(Attr->getRange());
    break;
  }
  return true;
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
  DeclContext *DC;
  MemberAccessContext AccessContext = MemberAccessContext::Getter;
  SmallVector<const Expr *, 16> ExprStack;

public:
  AvailabilityWalker(
      TypeChecker &TC, DeclContext *DC) : TC(TC), DC(DC) {}

  virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    ExprStack.push_back(E);

    auto visitChildren = [&]() { return std::make_pair(true, E); };
    auto skipChildren = [&]() {
      ExprStack.pop_back();
      return std::make_pair(false, E);
    };

    if (auto DR = dyn_cast<DeclRefExpr>(E))
      diagAvailability(DR->getDecl(), DR->getSourceRange(),
                       getEnclosingApplyExpr());
    if (auto MR = dyn_cast<MemberRefExpr>(E)) {
      walkMemberRef(MR);
      return skipChildren();
    }
    if (auto OCDR = dyn_cast<OtherConstructorDeclRefExpr>(E))
      diagAvailability(OCDR->getDecl(),
                       OCDR->getConstructorLoc().getSourceRange(),
                       getEnclosingApplyExpr());
    if (auto DMR = dyn_cast<DynamicMemberRefExpr>(E))
      diagAvailability(DMR->getMember().getDecl(),
                       DMR->getNameLoc().getSourceRange(),
                       getEnclosingApplyExpr());
    if (auto DS = dyn_cast<DynamicSubscriptExpr>(E))
      diagAvailability(DS->getMember().getDecl(), DS->getSourceRange());
    if (auto S = dyn_cast<SubscriptExpr>(E)) {
      if (S->hasDecl())
        diagAvailability(S->getDecl().getDecl(), S->getSourceRange());
    }
    if (auto A = dyn_cast<AssignExpr>(E)) {
      walkAssignExpr(A);
      return skipChildren();
    }
    if (auto IO = dyn_cast<InOutExpr>(E)) {
      walkInOutExpr(IO);
      return skipChildren();
    }
    
    return visitChildren();
  }

  virtual Expr *walkToExprPost(Expr *E) override {
    assert(ExprStack.back() == E);
    ExprStack.pop_back();

    return E;
  }

private:
  bool diagAvailability(const ValueDecl *D, SourceRange R,
                        const ApplyExpr *call = nullptr);
  bool diagnoseIncDecRemoval(const ValueDecl *D, SourceRange R,
                             const AvailableAttr *Attr);
  bool diagnoseMemoryLayoutMigration(const ValueDecl *D, SourceRange R,
                                     const AvailableAttr *Attr,
                                     const ApplyExpr *call);

  /// Walks up from a potential callee to the enclosing ApplyExpr.
  const ApplyExpr *getEnclosingApplyExpr() const {
    ArrayRef<const Expr *> parents = ExprStack;
    assert(!parents.empty() && "must be called while visiting an expression");
    size_t idx = parents.size() - 1;

    do {
      if (idx == 0)
        return nullptr;
      --idx;
    } while (isa<DotSyntaxBaseIgnoredExpr>(parents[idx]) || // Mod.f(a)
             isa<SelfApplyExpr>(parents[idx]) || // obj.f(a)
             isa<IdentityExpr>(parents[idx]) || // (f)(a)
             isa<ForceValueExpr>(parents[idx]) || // f!(a)
             isa<BindOptionalExpr>(parents[idx])); // f?(a)

    auto *call = dyn_cast<ApplyExpr>(parents[idx]);
    if (!call || call->getFn() != parents[idx+1])
      return nullptr;
    return call;
  }

  /// Walk an assignment expression, checking for availability.
  void walkAssignExpr(AssignExpr *E) {
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
    walkInContext(E, Dest, MemberAccessContext::Setter);

    // Check RHS in getter context
    Expr *Source = E->getSrc();
    if (!Source) {
      return;
    }
    walkInContext(E, Source, MemberAccessContext::Getter);
  }
  
  /// Walk a member reference expression, checking for availability.
  void walkMemberRef(MemberRefExpr *E) {
    // Walk the base in a getter context.
    walkInContext(E, E->getBase(), MemberAccessContext::Getter);

    ValueDecl *D = E->getMember().getDecl();
    // Diagnose for the member declaration itself.
    if (diagAvailability(D, E->getNameLoc().getSourceRange()))
      return;

    if (TC.getLangOpts().DisableAvailabilityChecking)
      return;

    if (auto *ASD = dyn_cast<AbstractStorageDecl>(D)) {
      // Diagnose for appropriate accessors, given the access context.
      diagStorageAccess(ASD, E->getSourceRange(), DC);
    }
  }
  
  /// Walk an inout expression, checking for availability.
  void walkInOutExpr(InOutExpr *E) {
    walkInContext(E, E->getSubExpr(), MemberAccessContext::InOut);
  }

  /// Walk the given expression in the member access context.
  void walkInContext(Expr *baseExpr, Expr *E,
                     MemberAccessContext AccessContext) {
    llvm::SaveAndRestore<MemberAccessContext>
      C(this->AccessContext, AccessContext);
    E->walk(*this);
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


/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool AvailabilityWalker::diagAvailability(const ValueDecl *D, SourceRange R,
                                          const ApplyExpr *call) {
  if (!D)
    return false;

  if (auto *attr = AvailableAttr::isUnavailable(D)) {
    if (diagnoseIncDecRemoval(D, R, attr))
      return true;
    if (call && diagnoseMemoryLayoutMigration(D, R, attr, call))
      return true;
  }

  if (TC.diagnoseExplicitUnavailability(D, R, DC, call))
    return true;

  // Diagnose for deprecation
  if (const AvailableAttr *Attr = TypeChecker::getDeprecated(D)) {
    TC.diagnoseDeprecated(R, DC, Attr, D->getFullName(), call);
  }

  // Diagnose for potential unavailability
  auto maybeUnavail = TC.checkDeclarationAvailability(D, R.Start, DC);
  if (maybeUnavail.hasValue()) {
    TC.diagnosePotentialUnavailability(D, R, DC, maybeUnavail.getValue());
    return true;
  }
  return false;
}


/// Return true if the specified type looks like an integer of floating point
/// type.
static bool isIntegerOrFloatingPointType(Type ty, DeclContext *DC,
                                         TypeChecker &TC) {
  auto integerType =
    TC.getProtocol(SourceLoc(),
                   KnownProtocolKind::ExpressibleByIntegerLiteral);
  auto floatingType =
    TC.getProtocol(SourceLoc(),
                   KnownProtocolKind::ExpressibleByFloatLiteral);
  if (!integerType || !floatingType) return false;

  return
    TC.conformsToProtocol(ty, integerType, DC,
                          ConformanceCheckFlags::InExpression) ||
    TC.conformsToProtocol(ty, floatingType, DC,
                          ConformanceCheckFlags::InExpression);
}


/// If this is a call to an unavailable ++ / -- operator, try to diagnose it
/// with a fixit hint and return true.  If not, or if we fail, return false.
bool AvailabilityWalker::diagnoseIncDecRemoval(const ValueDecl *D,
                                               SourceRange R,
                                               const AvailableAttr *Attr) {
  // We can only produce a fixit if we're talking about ++ or --.
  bool isInc = D->getNameStr() == "++";
  if (!isInc && D->getNameStr() != "--")
    return false;

  // We can only handle the simple cases of lvalue++ and ++lvalue.  This is
  // always modeled as:
  //   (postfix_unary_expr (declrefexpr ++), (inoutexpr (lvalue)))
  // if not, bail out.
  if (ExprStack.size() != 2 ||
      !isa<DeclRefExpr>(ExprStack[1]) ||
      !(isa<PostfixUnaryExpr>(ExprStack[0]) ||
        isa<PrefixUnaryExpr>(ExprStack[0])))
    return false;

  auto call = cast<ApplyExpr>(ExprStack[0]);

  // If the expression type is integer or floating point, then we can rewrite it
  // to "lvalue += 1".
  std::string replacement;
  if (isIntegerOrFloatingPointType(call->getType(), DC, TC))
    replacement = isInc ? " += 1" : " -= 1";
  else {
    // Otherwise, it must be an index type.  Rewrite to:
    // "lvalue = lvalue.successor()".
    auto &SM = TC.Context.SourceMgr;
    auto CSR = Lexer::getCharSourceRangeFromSourceRange(SM,
                                         call->getArg()->getSourceRange());
    replacement = " = " + SM.extractText(CSR).str();
    replacement += isInc ? ".successor()" : ".predecessor()";
  }
  
  if (!replacement.empty()) {
    // If we emit a deprecation diagnostic, produce a fixit hint as well.
    auto diag = TC.diagnose(R.Start, diag::availability_decl_unavailable_msg,
                            D->getFullName(), "it has been removed in Swift 3");
    if (isa<PrefixUnaryExpr>(call)) {
      // Prefix: remove the ++ or --.
      diag.fixItRemove(call->getFn()->getSourceRange());
      diag.fixItInsertAfter(call->getArg()->getEndLoc(), replacement);
    } else {
      // Postfix: replace the ++ or --.
      diag.fixItReplace(call->getFn()->getSourceRange(), replacement);
    }

    return true;
  }


  return false;
}

/// If this is a call to an unavailable sizeof family functions, diagnose it
/// with a fixit hint and return true. If not, or if we fail, return false.
bool AvailabilityWalker::diagnoseMemoryLayoutMigration(const ValueDecl *D,
                                               SourceRange R,
                                               const AvailableAttr *Attr,
                                               const ApplyExpr *call) {

  if (!D->getModuleContext()->isStdlibModule())
    return false;

  std::pair<StringRef, bool> KindValue
    = llvm::StringSwitch<std::pair<StringRef, bool>>(D->getNameStr())
      .Case("sizeof", {"size", false})
      .Case("alignof", {"alignment", false})
      .Case("strideof", {"stride", false})
      .Default({});

  if (KindValue.first.empty())
    return false;

  auto Kind = KindValue.first;
  auto isValue = KindValue.second;

  auto args = dyn_cast<ParenExpr>(call->getArg());
  if (!args)
    return false;

  auto subject = args->getSubExpr();
  if (!isValue) {
    // sizeof(type(of: x)) is equivalent to sizeofValue(x)
    if (auto DTE = dyn_cast<DynamicTypeExpr>(subject)) {
      subject = DTE->getBase();
      isValue = true;
    }
  }

  EncodedDiagnosticMessage EncodedMessage(Attr->Message);
  auto diag = TC.diagnose(R.Start, diag::availability_decl_unavailable_msg,
                          D->getFullName(), EncodedMessage.Message);
  diag.highlight(R);

  StringRef Prefix = "MemoryLayout<";
  StringRef Suffix = ">.";
  if (isValue) {
    auto valueType = subject->getType()->getRValueType();
    if (!valueType || valueType->is<ErrorType>()) {
      // If we don't have a suitable argument, we cannot emit a fix-it.
      return true;
    }

    // NOTE: We are destructively replacing the source text here.
    // `sizeof(type(of: doSomething()))` => `MemoryLayout<T>.size`, where T is
    // the return type of `doSomething()`. If that function has any side
    // effects, this replacement will break the source.
    diag.fixItReplace(call->getSourceRange(),
      (Prefix + valueType->getString() + Suffix + Kind).str());
  } else {
    SourceRange PrefixRange(call->getStartLoc(), args->getLParenLoc());
    SourceRange SuffixRange(args->getRParenLoc());

    // We must truncate `.self`.
    // E.g. sizeof(T.self) => MemoryLayout<T>.size
    if (auto *DSE = dyn_cast<DotSelfExpr>(subject))
      SuffixRange.Start = DSE->getDotLoc();

    diag
      .fixItReplace(PrefixRange, Prefix)
      .fixItReplace(SuffixRange, (Suffix + Kind).str());
  }

  return true;
}

/// Diagnose uses of unavailable declarations.
static void diagAvailability(TypeChecker &TC, const Expr *E,
                             DeclContext *DC) {
  AvailabilityWalker walker(TC, DC);
  const_cast<Expr*>(E)->walk(walker);
}

//===----------------------------------------------------------------------===//
// Per func/init diagnostics
//===----------------------------------------------------------------------===//

namespace {
class VarDeclUsageChecker : public ASTWalker {
  TypeChecker &TC;
  
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
  
  /// This is a mapping from VarDecls to the if/while/guard statement that they
  /// occur in, when they are in a pattern in a StmtCondition.
  llvm::SmallDenseMap<VarDecl*, LabeledConditionalStmt*> StmtConditionForVD;
  
  bool sawError = false;
  
  VarDeclUsageChecker(const VarDeclUsageChecker &) = delete;
  void operator=(const VarDeclUsageChecker &) = delete;

public:
  VarDeclUsageChecker(TypeChecker &TC, AbstractFunctionDecl *AFD) : TC(TC) {
    // Track the parameters of the function.
    for (auto PL : AFD->getParameterLists())
      for (auto param : *PL)
        if (shouldTrackVarDecl(param))
          VarDecls[param] = 0;
    
  }
    
  VarDeclUsageChecker(TypeChecker &TC, VarDecl *VD) : TC(TC) {
    // Track a specific VarDecl
    VarDecls[VD] = 0;
  }
    
  void suppressDiagnostics() {
    sawError = true; // set this flag so that no diagnostics will be emitted on delete.
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
      
    // If this is a VarDecl, then add it to our list of things to track.
    if (auto *vd = dyn_cast<VarDecl>(D))
      if (shouldTrackVarDecl(vd)) {
        unsigned defaultFlags = 0;
        // If this VarDecl is nested inside of a CaptureListExpr, remember that
        // fact for better diagnostics.
        if (dyn_cast_or_null<CaptureListExpr>(Parent.getAsExpr()))
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

    
    // Note that we ignore the initialization behavior of PatternBindingDecls,
    // but we do want to walk into them, because we want to see any uses or
    // other things going on in the initializer expressions.
    return true;
  }
  
  /// The heavy lifting happens when visiting expressions.
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;

  /// handle #if directives.
  void handleIfConfig(IfConfigStmt *ICS);

  /// Custom handling for statements.
  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    // The body of #if statements are not walked into, we need custom processing
    // for them.
    if (auto *ICS = dyn_cast<IfConfigStmt>(S))
      handleIfConfig(ICS);
      
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
      
    return { true, S };
  }

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
    if ((access & (RK_Read|RK_Written)) == 0) {
      // If this is a member in a capture list, just say it is unused.  We could
      // produce a fixit hint with a parent map, but this is a lot of effort for
      // a narrow case.
      if (access & RK_CaptureList) {
        TC.diagnose(var->getLoc(), diag::capture_never_used,
                    var->getName());
        continue;
      }
      
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
                  unsigned noParens = initExpr->canAppendCallParentheses();
                  
                  // If the subexpr is an "as?" cast, we can rewrite it to
                  // be an "is" test.
                  bool isIsTest = false;
                  if (isa<ConditionalCheckedCastExpr>(initExpr) &&
                      !initExpr->isImplicit()) {
                    noParens = isIsTest = true;
                  }
                  
                  auto diagIF = TC.diagnose(var->getLoc(),
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
  if (ASD->isSettable(nullptr) && ASD->isSetterNonMutating()) {
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
    // The index of the subscript is evaluated as an rvalue.
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
void VarDeclUsageChecker::handleIfConfig(IfConfigStmt *ICS) {
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

  for (auto &clause : ICS->getClauses()) {
    // Active clauses are handled by the normal AST walk.
    if (clause.isActive) continue;

    for (auto elt : clause.Elements)
      elt.walk(ConservativeDeclMarker(*this));
  }
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

/// Diagnose C style for loops.

namespace {
enum class OperatorKind : char {
  Greater,
  Smaller,
  NotEqual,
};

static Expr *endConditionValueForConvertingCStyleForLoop(const ForStmt *FS,
                                        VarDecl *loopVar, OperatorKind &OpKind) {
  auto *Cond = FS->getCond().getPtrOrNull();
  if (!Cond)
    return nullptr;
  auto callExpr = dyn_cast<CallExpr>(Cond);
  if (!callExpr)
    return nullptr;
  auto dotSyntaxExpr = dyn_cast<DotSyntaxCallExpr>(callExpr->getFn());
  if (!dotSyntaxExpr)
    return nullptr;
  auto binaryExpr = dyn_cast<BinaryExpr>(dotSyntaxExpr->getBase());
  if (!binaryExpr)
    return nullptr;
  auto binaryFuncExpr = dyn_cast<DeclRefExpr>(binaryExpr->getFn());
  if (!binaryFuncExpr)
    return nullptr;

  // Verify that the condition is a simple != or < comparison to the loop variable.
  auto comparisonOpName = binaryFuncExpr->getDecl()->getNameStr();
  if (comparisonOpName == "!=")
    OpKind = OperatorKind::NotEqual;
  else if (comparisonOpName == "<")
    OpKind = OperatorKind::Smaller;
  else if (comparisonOpName == ">")
    OpKind = OperatorKind::Greater;
  else
    return nullptr;

  auto args = binaryExpr->getArg()->getElements();
  auto loadExpr = dyn_cast<LoadExpr>(args[0]);
  if (!loadExpr)
    return nullptr;
  auto declRefExpr = dyn_cast<DeclRefExpr>(loadExpr->getSubExpr());
  if (!declRefExpr)
    return nullptr;
  if (declRefExpr->getDecl() != loopVar)
    return nullptr;
  return args[1];
}

static bool unaryOperatorCheckForConvertingCStyleForLoop(const ForStmt *FS,
                                                         VarDecl *loopVar,
                                                         StringRef OpName) {
  auto *Increment = FS->getIncrement().getPtrOrNull();
  if (!Increment)
    return false;
  ApplyExpr *unaryExpr = dyn_cast<PrefixUnaryExpr>(Increment);
  if (!unaryExpr)
    unaryExpr = dyn_cast<PostfixUnaryExpr>(Increment);
  if (!unaryExpr)
    return false;
  auto inoutExpr = dyn_cast<InOutExpr>(unaryExpr->getArg());
  if (!inoutExpr)
    return false;
  auto incrementDeclRefExpr = dyn_cast<DeclRefExpr>(inoutExpr->getSubExpr());
  if (!incrementDeclRefExpr)
    return false;
  auto unaryFuncExpr = dyn_cast<DeclRefExpr>(unaryExpr->getFn());
  if (!unaryFuncExpr)
    return false;
  if (unaryFuncExpr->getDecl()->getNameStr() != OpName)
    return false;
  return incrementDeclRefExpr->getDecl() == loopVar;
}


static bool unaryIncrementForConvertingCStyleForLoop(const ForStmt *FS,
                                                     VarDecl *loopVar) {
  return unaryOperatorCheckForConvertingCStyleForLoop(FS, loopVar, "++");
}

static bool unaryDecrementForConvertingCStyleForLoop(const ForStmt *FS,
                                                     VarDecl *loopVar) {
  return unaryOperatorCheckForConvertingCStyleForLoop(FS, loopVar, "--");
}

static bool binaryOperatorCheckForConvertingCStyleForLoop(TypeChecker &TC,
                                                            const ForStmt *FS,
                                                            VarDecl *loopVar,
                                                            StringRef OpName) {
  auto *Increment = FS->getIncrement().getPtrOrNull();
  if (!Increment)
    return false;
  ApplyExpr *binaryExpr = dyn_cast<BinaryExpr>(Increment);
  if (!binaryExpr)
    return false;
  auto binaryFuncExpr = dyn_cast<DeclRefExpr>(binaryExpr->getFn());
  if (!binaryFuncExpr)
    return false;
  if (binaryFuncExpr->getDecl()->getNameStr() != OpName)
    return false;
  auto argTupleExpr = dyn_cast<TupleExpr>(binaryExpr->getArg());
  if (!argTupleExpr)
    return false;
  auto addOneConstExpr = argTupleExpr->getElement(1);

  // Rather than unwrapping expressions all the way down implicit constructors, etc, just check that the
  // source text for the += argument is "1".
  SourceLoc constEndLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr, addOneConstExpr->getEndLoc());
  auto range = CharSourceRange(TC.Context.SourceMgr, addOneConstExpr->getStartLoc(), constEndLoc);
  if (range.str() != "1")
    return false;

  auto inoutExpr = dyn_cast<InOutExpr>(argTupleExpr->getElement(0));
  if (!inoutExpr)
    return false;
  auto declRefExpr = dyn_cast<DeclRefExpr>(inoutExpr->getSubExpr());
  if (!declRefExpr)
    return false;
  return declRefExpr->getDecl() == loopVar;

}

static bool plusEqualOneIncrementForConvertingCStyleForLoop(TypeChecker &TC,
                                                            const ForStmt *FS,
                                                            VarDecl *loopVar) {
  return binaryOperatorCheckForConvertingCStyleForLoop(TC, FS, loopVar, "+=");
}

static bool minusEqualOneDecrementForConvertingCStyleForLoop(TypeChecker &TC,
                                                             const ForStmt *FS,
                                                             VarDecl *loopVar) {
  return binaryOperatorCheckForConvertingCStyleForLoop(TC, FS, loopVar, "-=");
}

static void checkCStyleForLoop(TypeChecker &TC, const ForStmt *FS) {
  // If we're missing semi-colons we'll already be erroring out, and this may
  // not even have been intended as C-style.
  if (FS->getFirstSemicolonLoc().isInvalid() || FS->getSecondSemicolonLoc().isInvalid())
    return;
    
  InFlightDiagnostic diagnostic = TC.diagnose(FS->getStartLoc(), diag::deprecated_c_style_for_stmt);
    
  // Try to construct a fix it using for-each:

  // Verify that there is only one loop variable, and it is declared here.
  auto initializers = FS->getInitializerVarDecls();
  PatternBindingDecl *loopVarDecl = initializers.size() == 2 ?
    dyn_cast<PatternBindingDecl>(initializers[0]) : nullptr;
  if (!loopVarDecl || loopVarDecl->getNumPatternEntries() != 1)
    return;

  VarDecl *loopVar = dyn_cast<VarDecl>(initializers[1]);
  Expr *startValue = loopVarDecl->getInit(0);
  OperatorKind OpKind;
  Expr *endValue = endConditionValueForConvertingCStyleForLoop(FS, loopVar, OpKind);
  bool strideByOne = unaryIncrementForConvertingCStyleForLoop(FS, loopVar) ||
               plusEqualOneIncrementForConvertingCStyleForLoop(TC, FS, loopVar);
  bool strideBackByOne = unaryDecrementForConvertingCStyleForLoop(FS, loopVar) ||
               minusEqualOneDecrementForConvertingCStyleForLoop(TC, FS, loopVar);

  if (!loopVar || !startValue || !endValue || (!strideByOne && !strideBackByOne))
    return;

  assert(strideBackByOne != strideByOne && "cannot be both increment and decrement.");

  // Verify that the loop variable is invariant inside the body.
  VarDeclUsageChecker checker(TC, loopVar);
  checker.suppressDiagnostics();
  FS->getBody()->walk(checker);
    
  if (checker.isVarDeclEverWritten(loopVar)) {
    diagnostic.flush();
    return;
  }
    
  SourceLoc loopPatternEnd =
    Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                               loopVarDecl->getPattern(0)->getEndLoc());
  SourceLoc endOfIncrementLoc =
    Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                               FS->getIncrement().getPtrOrNull()->getEndLoc());

  if (strideByOne && OpKind != OperatorKind::Greater) {
    diagnostic
      .fixItRemoveChars(loopVarDecl->getLoc(), loopVar->getLoc())
      .fixItReplaceChars(loopPatternEnd, startValue->getStartLoc(), " in ")
      .fixItReplaceChars(FS->getFirstSemicolonLoc(), endValue->getStartLoc(),
                       " ..< ")
      .fixItRemoveChars(FS->getSecondSemicolonLoc(), endOfIncrementLoc);
    return;
  } else if (strideBackByOne && OpKind != OperatorKind::Smaller) {
    SourceLoc startValueEnd = Lexer::getLocForEndOfToken(TC.Context.SourceMgr,
                                                         startValue->getEndLoc());

    StringRef endValueStr = CharSourceRange(TC.Context.SourceMgr, endValue->getStartLoc(),
      Lexer::getLocForEndOfToken(TC.Context.SourceMgr, endValue->getEndLoc())).str();

    diagnostic
      .fixItRemoveChars(loopVarDecl->getLoc(), loopVar->getLoc())
      .fixItReplaceChars(loopPatternEnd, startValue->getStartLoc(), " in ")
      .fixItInsert(startValue->getStartLoc(), (llvm::Twine("((") + endValueStr + " + 1)...").str())
      .fixItInsert(startValueEnd, ").reversed()")
      .fixItRemoveChars(FS->getFirstSemicolonLoc(), endOfIncrementLoc);
  }
}
}// Anonymous namespace end.

// Perform MiscDiagnostics on Switch Statements.
static void checkSwitch(TypeChecker &TC, const SwitchStmt *stmt) {
  // We want to warn about "case .Foo, .Bar where 1 != 100:" since the where
  // clause only applies to the second case, and this is surprising.
  for (auto cs : stmt->getCases()) {
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

// Perform checkStmtConditionTrailingClosure for single expression.
static void checkStmtConditionTrailingClosure(TypeChecker &TC, const Expr *E) {
  if (E == nullptr || isa<ErrorExpr>(E)) return;

  // Shallow walker. just dig into implicit expression.
  class DiagnoseWalker : public ASTWalker {
    TypeChecker &TC;

    void diagnoseIt(const CallExpr *E) {
      auto argsExpr = E->getArg();
      auto argsTy = argsExpr->getType();

      // Ignore invalid argument type. Some diagnostics are already emitted.
      if (!argsTy || argsTy->is<ErrorType>()) return;

      if (auto TSE = dyn_cast<TupleShuffleExpr>(argsExpr))
        argsExpr = TSE->getSubExpr();

      SmallString<16> replacement;
      SourceLoc lastLoc;
      SourceRange closureRange;
      if (auto PE = dyn_cast<ParenExpr>(argsExpr)) {
        // Ignore non-trailing-closure.
        if (!PE->hasTrailingClosure()) return;

        closureRange = PE->getSubExpr()->getSourceRange();
        lastLoc = PE->getLParenLoc();
        if (lastLoc.isValid()) {
          // Empty paren: e.g. if funcName() { 1 } { ... }
          replacement = "";
        } else {
          // Bare trailing closure: e.g. if funcName { 1 } { ... }
          replacement = "(";
          lastLoc = E->getFn()->getEndLoc();
        }
      } else if (auto TE = dyn_cast<TupleExpr>(argsExpr)) {
        // Ignore non-trailing-closure.
        if (!TE->hasTrailingClosure()) return;

        // Tuple + trailing closure: e.g. if funcName(x: 1) { 1 } { ... }
        auto numElements = TE->getNumElements();
        assert(numElements >= 2 && "Unexpected num of elements in TupleExpr");
        closureRange = TE->getElement(numElements - 1)->getSourceRange();
        lastLoc = TE->getElement(numElements - 2)->getEndLoc();
        replacement = ", ";
      } else {
        // Can't be here.
        return;
      }

      // Add argument label of the closure that is going to be enclosed in
      // parens.
      if (auto TT = argsTy->getAs<TupleType>()) {
        assert(TT->getNumElements() != 0 && "Unexpected empty TupleType");
        auto closureLabel = TT->getElement(TT->getNumElements() - 1).getName();
        if (!closureLabel.empty()) {
          replacement += closureLabel.str();
          replacement += ": ";
        }
      }

      // Emit diagnostics.
      lastLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr, lastLoc);
      TC.diagnose(closureRange.Start, diag::trailing_closure_requires_parens)
        .fixItReplaceChars(lastLoc, closureRange.Start, replacement)
        .fixItInsertAfter(closureRange.End, ")");
    }

  public:
    DiagnoseWalker(TypeChecker &tc) : TC(tc) { }

    virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
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
    auto nominal =
      method->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
    auto result = TC.lookupMember(const_cast<DeclContext *>(DC),
                                  nominal->getInterfaceType(), lookupName,
                                  (defaultMemberLookupOptions |
                                   NameLookupFlags::KnownPrivate));

    // If we didn't find multiple methods, there is no ambiguity.
    if (result.size() < 2) return false;

    // If we found more than two methods, it's ambiguous.
    if (result.size() > 2) return true;

    // Dig out the methods.
    auto firstMethod = dyn_cast<FuncDecl>(result[0].Decl);
    auto secondMethod = dyn_cast<FuncDecl>(result[1].Decl);
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

  virtual std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    StringLiteralExpr *stringLiteral = dyn_cast<StringLiteralExpr>(expr);
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
      auto ctorContextType = ctor->getDeclContext()->getDeclaredTypeOfContext();
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
      if (auto proto =
            method->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
        // If the best so far is not from a protocol, or is from a
        // protocol that inherits this protocol, we have a new best.
        auto bestProto = bestMethod->getDeclContext()
          ->getAsProtocolOrProtocolExtensionContext();
        if (!bestProto || bestProto->inheritsFrom(proto))
          bestMethod = method;
        continue;
      }

      // This method is from a class.
      auto classDecl =
        method->getDeclContext()->getAsClassOrClassExtensionContext();

      // If the best method was from a protocol, keep it.
      auto bestClassDecl =
        bestMethod->getDeclContext()->getAsClassOrClassExtensionContext();
      if (!bestClassDecl) continue;

      // If the best method was from a subclass of the place where
      // this method was declared, we have a new best.
      while (auto superclassTy = bestClassDecl->getSuperclass()) {
        auto superclassDecl = superclassTy->getClassOrBoundGenericClass();
        if (!superclassDecl) break;

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
        auto nominal = bestMethod->getDeclContext()
                         ->getAsNominalTypeOrNominalTypeExtensionContext();
        out << "#selector(";

        DeclName name;
        auto bestFunc = dyn_cast<FuncDecl>(bestMethod);
        bool isAccessor = bestFunc && bestFunc->isAccessor();
        if (isAccessor) {
          switch (bestFunc->getAccessorKind()) {
          case AccessorKind::NotAccessor:
            llvm_unreachable("not an accessor");

          case AccessorKind::IsGetter:
            out << "getter: ";
            name = bestFunc->getAccessorStorageDecl()->getFullName();
            break;

          case AccessorKind::IsSetter:
          case AccessorKind::IsWillSet:
          case AccessorKind::IsDidSet:
            out << "setter: ";
            name = bestFunc->getAccessorStorageDecl()->getFullName();
            break;

          case AccessorKind::IsMaterializeForSet:
          case AccessorKind::IsAddressor:
          case AccessorKind::IsMutableAddressor:
            llvm_unreachable("cannot be @objc");
          }
        } else {
          name = bestMethod->getFullName();
        }

        out << nominal->getName().str() << "." << name.getBaseName().str();
        auto argNames = name.getArgumentNames();

        // Only print the parentheses if there are some argument
        // names, because "()" would indicate a call.
        if (argNames.size() > 0) {
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
        if (!isAccessor && isSelectorReferenceAmbiguous(bestMethod)) {
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
}

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
    if (auto ooType = subExpr->getType()->getAnyOptionalObjectType()) {
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
        
//===----------------------------------------------------------------------===//
// High-level entry points.
//===----------------------------------------------------------------------===//

/// \brief Emit diagnostics for syntactic restrictions on a given expression.
void swift::performSyntacticExprDiagnostics(TypeChecker &TC, const Expr *E,
                                            const DeclContext *DC,
                                            bool isExprStmt) {
  diagSelfAssignment(TC, E);
  diagSyntacticUseRestrictions(TC, E, DC, isExprStmt);
  diagRecursivePropertyAccess(TC, E, DC);
  diagnoseImplicitSelfUseInClosure(TC, E, DC);
  if (!TC.getLangOpts().DisableAvailabilityChecking)
    diagAvailability(TC, E, const_cast<DeclContext*>(DC));
  if (TC.Context.LangOpts.EnableObjCInterop)
    diagDeprecatedObjCSelectors(TC, DC, E);
}

void swift::performStmtDiagnostics(TypeChecker &TC, const Stmt *S) {
  TC.checkUnsupportedProtocolType(const_cast<Stmt *>(S));
    
  if (auto forStmt = dyn_cast<ForStmt>(S))
    checkCStyleForLoop(TC, forStmt);
  
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


Accessibility
swift::accessibilityFromScopeForDiagnostics(const DeclContext *accessScope) {
  if (!accessScope)
    return Accessibility::Public;
  if (isa<ModuleDecl>(accessScope))
    return Accessibility::Internal;
  if (accessScope->isModuleScopeContext() &&
      accessScope->getASTContext().LangOpts.EnableSwift3Private) {
    return Accessibility::FilePrivate;
  }
  return Accessibility::Private;
}

void swift::fixItAccessibility(InFlightDiagnostic &diag, ValueDecl *VD,
                               Accessibility desiredAccess, bool isForSetter) {
  StringRef fixItString;
  switch (desiredAccess) {
  case Accessibility::Private:      fixItString = "private ";      break;
  case Accessibility::FilePrivate:  fixItString = "fileprivate ";  break;
  case Accessibility::Internal:     fixItString = "internal ";     break;
  case Accessibility::Public:       fixItString = "public ";       break;
  case Accessibility::Open:         fixItString = "open ";         break;
  }

  DeclAttributes &attrs = VD->getAttrs();
  AbstractAccessibilityAttr *attr;
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
      type = aliasTy->getDecl()->getUnderlyingType();
      continue;
    }

    // Strip off lvalue/inout types.
    Type newType = type->getLValueOrInOutObjectType();
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
    if (auto parenTy = dyn_cast<ParenType>(type.getPointer())) {
      type = parenTy->getUnderlyingType();
      continue;
    }

    // Look through optionals.
    if (auto optObjectTy = type->getAnyOptionalObjectType()) {
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

  if (!afd->hasType())
    validateDecl(afd);

  if (afd->isInvalid() || isa<DestructorDecl>(afd))
    return None;

  DeclName name = afd->getFullName();
  if (!name)
    return None;

  // String'ify the arguments.
  StringRef baseNameStr = name.getBaseName().str();
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
  for (auto param : *afd->getParameterLists().back()) {
    paramTypes.push_back(getTypeNameForOmission(param->getType())
                         .withDefaultArgument(param->isDefaultArgument()));
  }
  
  // Handle contextual type, result type, and returnsSelf.
  Type contextType = afd->getDeclContext()->getDeclaredInterfaceType();
  Type resultType;
  bool returnsSelf = false;

  if (auto func = dyn_cast<FuncDecl>(afd)) {
    resultType = func->getResultType();
    returnsSelf = func->hasDynamicSelf();
  } else if (isa<ConstructorDecl>(afd)) {
    resultType = contextType;
    returnsSelf = true;
  }

  // Figure out the first parameter name.
  StringRef firstParamName;
  auto params = afd->getParameterList(afd->getImplicitSelfDecl() ? 1 : 0);
  if (params->size() != 0 && !params->get(0)->getName().empty())
    firstParamName = params->get(0)->getName().str();

  // Find the set of property names.
  const InheritedNameSet *allPropertyNames = nullptr;
  if (contextType) {
    if (auto classDecl = contextType->getClassOrBoundGenericClass()) {
      allPropertyNames = Context.getAllPropertyNames(classDecl,
                                                     afd->isInstanceMember());
    }
  }

  StringScratchSpace scratch;
  if (!swift::omitNeedlessWords(baseNameStr, argNameStrs, firstParamName,
                                getTypeNameForOmission(resultType),
                                getTypeNameForOmission(contextType),
                                paramTypes, returnsSelf, false,
                                allPropertyNames, scratch))
    return None;

  /// Retrieve a replacement identifier.
  auto getReplacementIdentifier = [&](StringRef name,
                                      Identifier old) -> Identifier{
    if (name.empty())
      return Identifier();

    if (!old.empty() && name == old.str())
      return old;

    return Context.getIdentifier(name);
  };

  Identifier newBaseName = getReplacementIdentifier(baseNameStr,
                                                    name.getBaseName());
  SmallVector<Identifier, 4> newArgNames;
  auto oldArgNames = name.getArgumentNames();
  for (unsigned i = 0, n = argNameStrs.size(); i != n; ++i) {
    newArgNames.push_back(getReplacementIdentifier(argNameStrs[i],
                                                   oldArgNames[i]));
  }

  return DeclName(Context, newBaseName, newArgNames);
}

Optional<Identifier> TypeChecker::omitNeedlessWords(VarDecl *var) {
  auto &Context = var->getASTContext();

  if (!var->hasType())
    validateDecl(var);

  if (var->isInvalid() || !var->hasType())
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
                ->getLValueOrInOutObjectType();
  while (auto optObjectTy = type->getAnyOptionalObjectType())
    type = optObjectTy;

  // Find the set of property names.
  const InheritedNameSet *allPropertyNames = nullptr;
  if (contextType) {
    if (auto classDecl = contextType->getClassOrBoundGenericClass()) {
      allPropertyNames = Context.getAllPropertyNames(classDecl,
                                                     var->isInstanceMember());
    }
  }


  // Omit needless words.
  StringScratchSpace scratch;
  OmissionTypeName typeName = getTypeNameForOmission(var->getType());
  OmissionTypeName contextTypeName = getTypeNameForOmission(contextType);
  if (::omitNeedlessWords(name, { }, "", typeName, contextTypeName, { },
                          /*returnsSelf=*/false, true, allPropertyNames,
                          scratch)) {
    return Context.getIdentifier(name);
  }

  return None;
}

void TypeChecker::checkOmitNeedlessWords(AbstractFunctionDecl *afd) {
  if (!Context.LangOpts.WarnOmitNeedlessWords)
    return;

  auto newName = omitNeedlessWords(afd);
  if (!newName)
    return;

  auto name = afd->getFullName();
  InFlightDiagnostic diag = diagnose(afd->getLoc(), diag::omit_needless_words,
                                     name, *newName);
  fixDeclarationName(diag, afd, *newName);
}

void TypeChecker::checkOmitNeedlessWords(VarDecl *var) {
  if (!Context.LangOpts.WarnOmitNeedlessWords)
    return;

  auto newName = omitNeedlessWords(var);
  if (!newName)
    return;

  auto name = var->getName();
  diagnose(var->getLoc(), diag::omit_needless_words, name, *newName)
    .fixItReplace(var->getLoc(), newName->str());
}

/// Find the source ranges of extraneous default arguments within a
/// call to the given function.
static bool hasExtraneousDefaultArguments(AbstractFunctionDecl *afd,
                                          Expr *arg, DeclName name,
                                          SmallVectorImpl<SourceRange> &ranges,
                                      SmallVectorImpl<unsigned> &removedArgs) {
  if (!afd->getClangDecl())
    return false;

  if (afd->isInvalid())
    return false;

  if (auto shuffle = dyn_cast<TupleShuffleExpr>(arg))
    arg = shuffle->getSubExpr();
    
  TupleExpr *argTuple = dyn_cast<TupleExpr>(arg);
  ParenExpr *argParen = dyn_cast<ParenExpr>(arg);
  
  ASTContext &ctx = afd->getASTContext();
  // Skip over the implicit 'self'.
  auto *bodyParams = afd->getParameterList(afd->getImplicitSelfDecl()?1:0);

  Optional<unsigned> firstRemoved;
  Optional<unsigned> lastRemoved;
  unsigned numElementsInParens;
  if (argTuple) {
    numElementsInParens = (argTuple->getNumElements() -
                           argTuple->hasTrailingClosure());
  } else if (argParen) {
    numElementsInParens = 1 - argParen->hasTrailingClosure();
  } else {
    numElementsInParens = 0;
  }

  for (unsigned i = 0; i != numElementsInParens; ++i) {
    auto param = bodyParams->get(i);
    if (!param->isDefaultArgument())
      continue;

    auto defaultArg = param->getDefaultArgumentKind();

    // Never consider removing the first argument for a "set" method
    // with an unnamed first argument.
    if (i == 0 &&
        !name.getBaseName().empty() &&
        camel_case::getFirstWord(name.getBaseName().str()) == "set" &&
        name.getArgumentNames().size() > 0 &&
        name.getArgumentNames()[0].empty())
      continue;

    SourceRange removalRange;
    if (argTuple && i < argTuple->getNumElements()) {
      // Check whether the supplied argument is the same as the
      // default argument.
      if (defaultArg != inferDefaultArgumentKind(argTuple->getElement(i)))
        continue;

      // Figure out where to start removing this argument.
      if (i == 0) {
        // Start removing right after the opening parenthesis.
        removalRange.Start = argTuple->getLParenLoc();
      } else {
        // Start removing right after the preceding argument, so we
        // consume the comma as well.
        removalRange.Start = argTuple->getElement(i-1)->getEndLoc();
      }

      // Adjust to the end of the starting token.
      removalRange.Start
        = Lexer::getLocForEndOfToken(ctx.SourceMgr, removalRange.Start);

      // Figure out where to finish removing this element.
      if (i == 0 && i < numElementsInParens - 1) {
        // We're the first of several arguments; consume the
        // following comma as well.
        removalRange.End = argTuple->getElementNameLoc(i+1);
        if (removalRange.End.isInvalid())
          removalRange.End = argTuple->getElement(i+1)->getStartLoc();
      } else if (i < numElementsInParens - 1) {
        // We're in the middle; consume through the end of this
        // element.
        removalRange.End
          = Lexer::getLocForEndOfToken(ctx.SourceMgr,
                                       argTuple->getElement(i)->getEndLoc());
      } else {
        // We're at the end; consume up to the closing parentheses.
        removalRange.End = argTuple->getRParenLoc();
      }
    } else if (argParen) {
      // Check whether we have a default argument.
      if (defaultArg != inferDefaultArgumentKind(argParen->getSubExpr()))
        continue;

      removalRange = SourceRange(argParen->getSubExpr()->getStartLoc(),
                                 argParen->getRParenLoc());
    } else {
      continue;
    }

    if (removalRange.isInvalid())
      continue;

    // Note that we're removing this argument.
    removedArgs.push_back(i);

    // If we hadn't removed anything before, this is the first
    // removal.
    if (!firstRemoved) {
      ranges.push_back(removalRange);
      firstRemoved = i;
      lastRemoved = i;
      continue;
    }

    // If the previous removal range was the previous argument,
    // combine the ranges.
    if (*lastRemoved == i - 1) {
      ranges.back().End = removalRange.End;
      lastRemoved = i;
      continue;
    }

    // Otherwise, add this new removal range.
    ranges.push_back(removalRange);
    lastRemoved = i;
  }

  // If there is a single removal range that covers everything but
  // the trailing closure at the end, also zap the parentheses.
  if (ranges.size() == 1 &&
      *firstRemoved == 0 && *lastRemoved == bodyParams->size() - 2 &&
      argTuple && argTuple->hasTrailingClosure()) {
    ranges.front().Start = argTuple->getLParenLoc();
    ranges.front().End
      = Lexer::getLocForEndOfToken(ctx.SourceMgr, argTuple->getRParenLoc());
  }

  return !ranges.empty();
}

void TypeChecker::checkOmitNeedlessWords(ApplyExpr *apply) {
  if (!Context.LangOpts.WarnOmitNeedlessWords)
    return;

  // Find the callee.
  ApplyExpr *innermostApply = apply;
  unsigned numApplications = 0;
  while (auto fnApply = dyn_cast<ApplyExpr>(
                          innermostApply->getFn()->getValueProvidingExpr())) {
    innermostApply = fnApply;
    ++numApplications;
  }
  if (numApplications != 1)
    return;

  DeclRefExpr *fnRef
    = dyn_cast<DeclRefExpr>(innermostApply->getFn()->getValueProvidingExpr());
  if (!fnRef)
    return;

  auto *afd = dyn_cast<AbstractFunctionDecl>(fnRef->getDecl());
  if (!afd)
    return;

  // Determine whether the callee has any needless words in it.
  auto newName = omitNeedlessWords(afd);

  bool renamed;
  if (!newName) {
    newName = afd->getFullName();
    renamed = false;
  } else {
    renamed = true;
  }

  // Determine whether there are any extraneous default arguments to be zapped.
  SmallVector<SourceRange, 2> removedDefaultArgRanges;
  SmallVector<unsigned, 2> removedArgs;
  bool anyExtraneousDefaultArgs
    = hasExtraneousDefaultArguments(afd, apply->getArg(), *newName,
                                    removedDefaultArgRanges,
                                    removedArgs);

  if (!renamed && !anyExtraneousDefaultArgs)
    return;

  // Make sure to apply the fix at the right application level.
  auto name = afd->getFullName();

  // Dig out the argument tuple.
  Expr *arg = apply->getArg();
  if (auto shuffle = dyn_cast<TupleShuffleExpr>(arg))
    arg = shuffle->getSubExpr();
  TupleExpr *argTuple = dyn_cast<TupleExpr>(arg);
  ParenExpr *argParen = dyn_cast<ParenExpr>(arg);

  if (argParen && !argTuple)
    arg = argParen->getSubExpr();

  InFlightDiagnostic diag
    = renamed ? diagnose(fnRef->getLoc(), diag::omit_needless_words,
                         name, *newName)
              : diagnose(fnRef->getLoc(), diag::extraneous_default_args_in_call,
                         name);

  // Fix the base name.
  if (newName->getBaseName() != name.getBaseName()) {
    diag.fixItReplace(fnRef->getLoc(), newName->getBaseName().str());
  }

  // Fix the argument names.
  auto oldArgNames = name.getArgumentNames();
  auto newArgNames = newName->getArgumentNames();
  unsigned currentRemovedArg = 0;
  if (argTuple) {
    for (unsigned i = 0, n = newArgNames.size(); i != n; ++i) {
      // If this argument was completely removed, don't emit any
      // Fix-Its for it.
      if (currentRemovedArg < removedArgs.size() &&
          removedArgs[currentRemovedArg] == i) {
        ++currentRemovedArg;
        continue;
      }

      // Check whether the name changed.
      auto newArgName = newArgNames[i];
      if (oldArgNames[i] == newArgName) continue;

      if (i >= argTuple->getNumElements()) break;
      if (argTuple->getElementName(i) != oldArgNames[i]) continue;

      auto nameLoc = argTuple->getElementNameLoc(i);
      if (nameLoc.isInvalid()) {
        // Add the argument label.
        diag.fixItInsert(argTuple->getElement(i)->getStartLoc(),
                         (newArgName.str() + ": ").str());
      } else if (newArgName.empty()) {
        // Delete the argument label.
        diag.fixItRemoveChars(nameLoc, argTuple->getElement(i)->getStartLoc());
      } else {
        // Fix the argument label.
        diag.fixItReplace(nameLoc, newArgName.str());
      }
    }
  } else if (newArgNames.size() > 0 && !newArgNames[0].empty() &&
             (!argParen || !argParen->hasTrailingClosure()) &&
             removedArgs.empty()) {
    // Add the argument label.
    auto newArgName = newArgNames[0];
    diag.fixItInsert(arg->getStartLoc(), (newArgName.str() + ": ").str());
  }

  // Remove all of the defaulted arguments.
  for (auto extraneous : removedDefaultArgRanges) {
    diag.fixItRemoveChars(extraneous.Start, extraneous.End);
  }
}

void TypeChecker::checkOmitNeedlessWords(MemberRefExpr *memberRef) {
  if (!Context.LangOpts.WarnOmitNeedlessWords)
    return;

  auto var = dyn_cast<VarDecl>(memberRef->getMember().getDecl());
  if (!var)
    return;

  // Check whether any needless words were omitted.
  auto newName = omitNeedlessWords(var);
  if (!newName)
    return;

  // Fix the name.
  auto name = var->getName();
  diagnose(memberRef->getNameLoc(), diag::omit_needless_words, name, *newName)
    .fixItReplace(memberRef->getNameLoc().getSourceRange(), newName->str());
}
