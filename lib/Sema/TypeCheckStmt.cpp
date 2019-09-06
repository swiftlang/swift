//===--- TypeCheckStmt.cpp - Type Checking for Statements -----------------===//
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
// This file implements semantic analysis for statements.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckType.h"
#include "MiscDiagnostics.h"
#include "ConstraintSystem.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/TopCollection.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/LocalContext.h"
#include "swift/Syntax/TokenKinds.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Timer.h"

using namespace swift;

#define DEBUG_TYPE "TypeCheckStmt"

#ifndef NDEBUG
/// Determine whether the given context is for the backing property of a
/// property wrapper.
static bool isPropertyWrapperBackingInitContext(DeclContext *dc) {
  auto initContext = dyn_cast<Initializer>(dc);
  if (!initContext) return false;

  auto patternInitContext = dyn_cast<PatternBindingInitializer>(initContext);
  if (!patternInitContext) return false;

  auto binding = patternInitContext->getBinding();
  if (!binding) return false;

  auto singleVar = binding->getSingleVar();
  if (!singleVar) return false;

  return singleVar->getOriginalWrappedProperty() != nullptr;
}
#endif

namespace {
  class ContextualizeClosures : public ASTWalker {
    DeclContext *ParentDC;
  public:
    unsigned NextDiscriminator = 0;

    ContextualizeClosures(DeclContext *parent,
                          unsigned nextDiscriminator = 0)
      : ParentDC(parent), NextDiscriminator(nextDiscriminator) {}

    bool hasAutoClosures() const {
      return NextDiscriminator != 0;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // Autoclosures need to be numbered and potentially reparented.
      // Reparenting is required with:
      //   - nested autoclosures, because the inner autoclosure will be
      //     parented to the outer context, not the outer autoclosure
      //   - non-local initializers
      if (auto CE = dyn_cast<AutoClosureExpr>(E)) {
        // FIXME: Work around an apparent reentrancy problem with the REPL.
        // I don't understand what's going on here well enough to fix the
        // underlying issue. -Joe
        if (CE->getParent() == ParentDC
            && CE->getDiscriminator() != AutoClosureExpr::InvalidDiscriminator)
          return { false, E };
        
        assert(CE->getDiscriminator() == AutoClosureExpr::InvalidDiscriminator);
        CE->setDiscriminator(NextDiscriminator++);
        CE->setParent(ParentDC);

        // Recurse into the autoclosure body using the same sequence,
        // but parenting to the autoclosure instead of the outer closure.
        auto oldParentDC = ParentDC;
        ParentDC = CE;
        CE->getBody()->walk(*this);
        ParentDC = oldParentDC;
        return { false, E };
      } 

      // Capture lists need to be reparented to enclosing autoclosures.
      if (auto CapE = dyn_cast<CaptureListExpr>(E)) {
        if (isa<AutoClosureExpr>(ParentDC)) {
          for (auto &Cap : CapE->getCaptureList()) {
            Cap.Init->setDeclContext(ParentDC);
            Cap.Var->setDeclContext(ParentDC);
          }
        }
      }

      // Explicit closures start their own sequence.
      if (auto CE = dyn_cast<ClosureExpr>(E)) {
        // In the repl, the parent top-level context may have been re-written.
        if (CE->getParent() != ParentDC) {
          if ((CE->getParent()->getContextKind() !=
                    ParentDC->getContextKind()) ||
              ParentDC->getContextKind() != DeclContextKind::TopLevelCodeDecl) {
            // If a closure is nested within an auto closure, we'll need to update
            // its parent to the auto closure parent.
            assert((ParentDC->getContextKind() ==
                      DeclContextKind::AbstractClosureExpr ||
                    isPropertyWrapperBackingInitContext(ParentDC)) &&
                   "Incorrect parent decl context for closure");
            CE->setParent(ParentDC);
          }
        }

        // If the closure has a single expression body, we need to walk into it
        // with a new sequence.  Otherwise, it'll have been separately
        // type-checked.
        if (CE->hasSingleExpressionBody())
          CE->getBody()->walk(ContextualizeClosures(CE));

        // In neither case do we need to continue the *current* walk.
        return { false, E };
      }

      return { true, E };
    }

    /// We don't want to recurse into most local declarations.
    bool walkToDeclPre(Decl *D) override {
      // But we do want to walk into the initializers of local
      // variables.
      return isa<PatternBindingDecl>(D);
    }
  };

  static DeclName getDescriptiveName(AbstractFunctionDecl *AFD) {
    DeclName name = AFD->getFullName();
    if (!name) {
      if (auto *accessor = dyn_cast<AccessorDecl>(AFD)) {
        name = accessor->getStorage()->getFullName();
      }
    }
    return name;
  }

  /// Used for debugging which parts of the code are taking a long time to
  /// compile.
  class FunctionBodyTimer {
    AnyFunctionRef Function;
    llvm::TimeRecord StartTime = llvm::TimeRecord::getCurrentTime();
    unsigned WarnLimit;
    bool ShouldDump;

  public:
    FunctionBodyTimer(AnyFunctionRef Fn, bool shouldDump,
                      unsigned warnLimit)
        : Function(Fn), WarnLimit(warnLimit), ShouldDump(shouldDump) {}

    ~FunctionBodyTimer() {
      llvm::TimeRecord endTime = llvm::TimeRecord::getCurrentTime(false);

      auto elapsed = endTime.getProcessTime() - StartTime.getProcessTime();
      unsigned elapsedMS = static_cast<unsigned>(elapsed * 1000);

      ASTContext &ctx = Function.getAsDeclContext()->getASTContext();
      auto *AFD = Function.getAbstractFunctionDecl();

      if (ShouldDump) {
        // Round up to the nearest 100th of a millisecond.
        llvm::errs() << llvm::format("%0.2f", ceil(elapsed * 100000) / 100) << "ms\t";
        Function.getLoc().print(llvm::errs(), ctx.SourceMgr);

        if (AFD) {
          llvm::errs()
            << "\t" << Decl::getDescriptiveKindName(AFD->getDescriptiveKind())
            << " " << getDescriptiveName(AFD);
        } else {
          llvm::errs() << "\t(closure)";
        }
        llvm::errs() << "\n";
      }

      if (WarnLimit != 0 && elapsedMS >= WarnLimit) {
        if (AFD) {
          ctx.Diags.diagnose(AFD, diag::debug_long_function_body,
                             AFD->getDescriptiveKind(), getDescriptiveName(AFD),
                             elapsedMS, WarnLimit);
        } else {
          ctx.Diags.diagnose(Function.getLoc(), diag::debug_long_closure_body,
                             elapsedMS, WarnLimit);
        }
      }
    }
  };
} // end anonymous namespace

static void setAutoClosureDiscriminators(DeclContext *DC, Stmt *S) {
  S->walk(ContextualizeClosures(DC));
}

bool TypeChecker::contextualizeInitializer(Initializer *DC, Expr *E) {
  ContextualizeClosures CC(DC);
  E->walk(CC);
  return CC.hasAutoClosures();
}

void TypeChecker::contextualizeTopLevelCode(TopLevelContext &TLC,
                                            TopLevelCodeDecl *TLCD) {
  unsigned nextDiscriminator = TLC.NextAutoClosureDiscriminator;
  ContextualizeClosures CC(TLCD, nextDiscriminator);
  TLCD->getBody()->walk(CC);
  assert(nextDiscriminator == TLC.NextAutoClosureDiscriminator &&
         "reentrant/concurrent invocation of contextualizeTopLevelCode?");
  TLC.NextAutoClosureDiscriminator = CC.NextDiscriminator;
}

/// Emits an error with a fixit for the case of unnecessary cast over a
/// OptionSet value. The primary motivation is to help with SDK changes.
/// Example:
/// \code
///   func supported() -> MyMask {
///     return Int(MyMask.Bingo.rawValue)
///   }
/// \endcode
static void tryDiagnoseUnnecessaryCastOverOptionSet(ASTContext &Ctx,
                                                    Expr *E,
                                                    Type ResultType,
                                                    ModuleDecl *module) {
  auto *NTD = ResultType->getAnyNominal();
  if (!NTD)
    return;
  auto optionSetType = dyn_cast_or_null<ProtocolDecl>(Ctx.getOptionSetDecl());
  if (!optionSetType)
    return;
  SmallVector<ProtocolConformance *, 4> conformances;
  if (!(optionSetType &&
        NTD->lookupConformance(module, optionSetType, conformances)))
    return;

  auto *CE = dyn_cast<CallExpr>(E);
  if (!CE)
    return;
  if (!isa<ConstructorRefCallExpr>(CE->getFn()))
    return;
  auto *ParenE = dyn_cast<ParenExpr>(CE->getArg());
  if (!ParenE)
    return;
  auto *ME = dyn_cast<MemberRefExpr>(ParenE->getSubExpr());
  if (!ME)
    return;
  ValueDecl *VD = ME->getMember().getDecl();
  if (!VD || VD->getBaseName() != Ctx.Id_rawValue)
    return;
  auto *BME = dyn_cast<MemberRefExpr>(ME->getBase());
  if (!BME)
    return;
  if (!BME->getType()->isEqual(ResultType))
    return;

  Ctx.Diags.diagnose(E->getLoc(), diag::unnecessary_cast_over_optionset,
                     ResultType)
    .highlight(E->getSourceRange())
    .fixItRemoveChars(E->getLoc(), ME->getStartLoc())
    .fixItRemove(SourceRange(ME->getDotLoc(), E->getEndLoc()));
}

namespace {
class StmtChecker : public StmtVisitor<StmtChecker, Stmt*> {
public:
  TypeChecker &TC;

  /// This is the current function or closure being checked.
  /// This is null for top level code.
  Optional<AnyFunctionRef> TheFunc;
  
  /// DC - This is the current DeclContext.
  DeclContext *DC;

  // Scope information for control flow statements
  // (break, continue, fallthrough).

  /// The level of loop nesting. 'break' and 'continue' are valid only in scopes
  /// where this is greater than one.
  SmallVector<LabeledStmt*, 2> ActiveLabeledStmts;

  /// The level of 'switch' nesting. 'fallthrough' is valid only in scopes where
  /// this is greater than one.
  unsigned SwitchLevel = 0;
  /// The destination block for a 'fallthrough' statement. Null if the switch
  /// scope depth is zero or if we are checking the final 'case' of the current
  /// switch.
  CaseStmt /*nullable*/ *FallthroughSource = nullptr;
  CaseStmt /*nullable*/ *FallthroughDest = nullptr;
  FallthroughStmt /*nullable*/ *PreviousFallthrough = nullptr;

  SourceLoc EndTypeCheckLoc;

  /// Used to check for discarded expression values: in the REPL top-level
  /// expressions are not discarded.
  bool IsREPL;

  /// Used to distinguish the first BraceStmt that starts a TopLevelCodeDecl.
  bool IsBraceStmtFromTopLevelDecl;

  struct AddLabeledStmt {
    StmtChecker &SC;
    AddLabeledStmt(StmtChecker &SC, LabeledStmt *LS) : SC(SC) {
      // Verify that we don't have label shadowing.
      if (!LS->getLabelInfo().Name.empty())
        for (auto PrevLS : SC.ActiveLabeledStmts) {
          if (PrevLS->getLabelInfo().Name == LS->getLabelInfo().Name) {
            SC.TC.diagnose(LS->getLabelInfo().Loc,
                        diag::label_shadowed, LS->getLabelInfo().Name);
            SC.TC.diagnose(PrevLS->getLabelInfo().Loc,
                           diag::invalid_redecl_prev, 
                           PrevLS->getLabelInfo().Name);
          }
        }

      // In any case, remember that we're in this labeled statement so that
      // break and continue are aware of it.
      SC.ActiveLabeledStmts.push_back(LS);
    }
    ~AddLabeledStmt() {
      SC.ActiveLabeledStmts.pop_back();
    }
  };
  
  struct AddSwitchNest {
    StmtChecker &SC;
    CaseStmt *OuterFallthroughDest;
    AddSwitchNest(StmtChecker &SC) : SC(SC),
        OuterFallthroughDest(SC.FallthroughDest) {
      ++SC.SwitchLevel;
    }
    
    ~AddSwitchNest() {
      --SC.SwitchLevel;
      SC.FallthroughDest = OuterFallthroughDest;
    }
  };

  StmtChecker(TypeChecker &TC, AbstractFunctionDecl *AFD)
      : TC(TC), TheFunc(AFD), DC(AFD), IsREPL(false),
        IsBraceStmtFromTopLevelDecl(false) {}

  StmtChecker(TypeChecker &TC, ClosureExpr *TheClosure)
      : TC(TC), TheFunc(TheClosure), DC(TheClosure), IsREPL(false),
        IsBraceStmtFromTopLevelDecl(false) {}

  StmtChecker(TypeChecker &TC, DeclContext *DC)
      : TC(TC), TheFunc(), DC(DC), IsREPL(false),
        IsBraceStmtFromTopLevelDecl(false) {
    if (const SourceFile *SF = DC->getParentSourceFile())
      if (SF->Kind == SourceFileKind::REPL)
        IsREPL = true;

    IsBraceStmtFromTopLevelDecl = isa<TopLevelCodeDecl>(DC);
  }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//
  
  bool isInDefer() const {
    if (!TheFunc.hasValue()) return false;
    auto *FD = dyn_cast_or_null<FuncDecl>
      (TheFunc.getValue().getAbstractFunctionDecl());
    return FD && FD->isDeferBody();
  }
  
  template<typename StmtTy>
  bool typeCheckStmt(StmtTy *&S) {
    FrontendStatsTracer StatsTracer(TC.Context.Stats, "typecheck-stmt", S);
    PrettyStackTraceStmt trace(TC.Context, "type-checking", S);
    StmtTy *S2 = cast_or_null<StmtTy>(visit(S));
    if (S2 == nullptr)
      return true;
    S = S2;
    performStmtDiagnostics(TC, S);
    return false;
  }

  /// Type-check an entire function body.
  bool typeCheckBody(BraceStmt *&S) {
    bool HadError = typeCheckStmt(S);
    setAutoClosureDiscriminators(DC, S);
    return HadError;
  }
  
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  Stmt *visitBraceStmt(BraceStmt *BS);

  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (!TheFunc.hasValue()) {
      TC.diagnose(RS->getReturnLoc(), diag::return_invalid_outside_func);
      return nullptr;
    }
    
    // If the return is in a defer, then it isn't valid either.
    if (isInDefer()) {
      TC.diagnose(RS->getReturnLoc(), diag::jump_out_of_defer, "return");
      return nullptr;
    }

    Type ResultTy = TheFunc->getBodyResultType();
    if (!ResultTy || ResultTy->hasError())
      return nullptr;

    if (!RS->hasResult()) {
      if (!ResultTy->isVoid())
        TC.diagnose(RS->getReturnLoc(), diag::return_expr_missing);
      return RS;
    }

    // If the body consisted of a single return without a result
    // 
    //   func foo() -> Int {
    //     return
    //   }
    // 
    // in parseAbstractFunctionBody the return is given an empty, implicit tuple
    // as its result
    //
    //   func foo() -> Int {
    //     return ()
    //   }
    //
    // Look for that case and diagnose it as missing return expression.
    if (!ResultTy->isVoid() && TheFunc->hasSingleExpressionBody()) {
      auto expr = TheFunc->getSingleExpressionBody();
      if (expr->isImplicit() && isa<TupleExpr>(expr) &&
          cast<TupleExpr>(expr)->getNumElements() == 0) {
        TC.diagnose(RS->getReturnLoc(), diag::return_expr_missing);
        return RS;
      }
    }

    Expr *E = RS->getResult();

    // In an initializer, the only expression allowed is "nil", which indicates
    // failure from a failable initializer.
    if (auto ctor = dyn_cast_or_null<ConstructorDecl>(
                                          TheFunc->getAbstractFunctionDecl())) {
      // The only valid return expression in an initializer is the literal
      // 'nil'.
      auto nilExpr = dyn_cast<NilLiteralExpr>(E->getSemanticsProvidingExpr());
      if (!nilExpr) {
        TC.diagnose(RS->getReturnLoc(), diag::return_init_non_nil)
          .highlight(E->getSourceRange());
        RS->setResult(nullptr);
        return RS;
      }

      // "return nil" is only permitted in a failable initializer.
      if (!ctor->isFailable()) {
        TC.diagnose(RS->getReturnLoc(), diag::return_non_failable_init)
          .highlight(E->getSourceRange());
        TC.diagnose(ctor->getLoc(), diag::make_init_failable,
                    ctor->getFullName())
          .fixItInsertAfter(ctor->getLoc(), "?");
        RS->setResult(nullptr);
        return RS;
      }

      // Replace the "return nil" with a new 'fail' statement.
      return new (TC.Context) FailStmt(RS->getReturnLoc(), nilExpr->getLoc(),
                                       RS->isImplicit());
    }
    
    TypeCheckExprOptions options = {};
    
    // If the result type is an opaque type, this is an opportunity to resolve
    // the underlying type.
    auto isOpaqueReturnTypeOfCurrentFunc = [&](OpaqueTypeDecl *opaque) -> bool {
      // Closures currently don't support having opaque types.
      auto funcDecl = TheFunc->getAbstractFunctionDecl();
      if (!funcDecl)
        return false;

      return opaque->isOpaqueReturnTypeOfFunction(funcDecl);
    };
    
    if (auto opaque = ResultTy->getAs<OpaqueTypeArchetypeType>()) {
      if (isOpaqueReturnTypeOfCurrentFunc(opaque->getDecl())) {
        options |= TypeCheckExprFlags::ConvertTypeIsOpaqueReturnType;
      }
    }

    if (EndTypeCheckLoc.isValid()) {
      assert(DiagnosticSuppression::isEnabled(TC.Diags) &&
             "Diagnosing and AllowUnresolvedTypeVariables don't seem to mix");
      options |= TypeCheckExprFlags::AllowUnresolvedTypeVariables;
    }

    ContextualTypePurpose ctp = CTP_ReturnStmt;
    if (auto func =
            dyn_cast_or_null<FuncDecl>(TheFunc->getAbstractFunctionDecl())) {
      if (func->hasSingleExpressionBody()) {
        ctp = CTP_ReturnSingleExpr;
      }
    }

    auto exprTy = TC.typeCheckExpression(E, DC, TypeLoc::withoutLoc(ResultTy),
                                         ctp,
                                         options);
    RS->setResult(E);

    if (!exprTy) {
      tryDiagnoseUnnecessaryCastOverOptionSet(TC.Context, E, ResultTy,
                                              DC->getParentModule());
    }

    return RS;
  }

  Stmt *visitYieldStmt(YieldStmt *YS) {
    // If the yield is in a defer, then it isn't valid.
    if (isInDefer()) {
      TC.diagnose(YS->getYieldLoc(), diag::jump_out_of_defer, "yield");
      return YS;
    }

    SmallVector<AnyFunctionType::Yield, 4> buffer;
    auto yieldResults = TheFunc->getBodyYieldResults(buffer);

    auto yieldExprs = YS->getMutableYields();
    if (yieldExprs.size() != yieldResults.size()) {
      TC.diagnose(YS->getYieldLoc(), diag::bad_yield_count,
                  yieldResults.size());
      return YS;
    }

    for (auto i : indices(yieldExprs)) {
      Type yieldType = yieldResults[i].getType();
      auto exprToCheck = yieldExprs[i];

      InOutExpr *inout = nullptr;

      // Classify whether we're yielding by reference or by value.
      ContextualTypePurpose contextTypePurpose;
      Type contextType = yieldType;
      if (yieldResults[i].isInOut()) {
        contextTypePurpose = CTP_YieldByReference;
        contextType = LValueType::get(contextType);

        // Check that the yielded expression is a &.
        if ((inout = dyn_cast<InOutExpr>(exprToCheck))) {
          // Strip the & off so that the constraint system doesn't complain
          // about the unparented &.
          exprToCheck = inout->getSubExpr();
        } else {
          TC.diagnose(exprToCheck->getLoc(),
                      diag::missing_address_of_yield, yieldType)
            .highlight(exprToCheck->getSourceRange());
          inout = new (TC.Context) InOutExpr(exprToCheck->getStartLoc(),
                                             exprToCheck,
                                             Type(), /*implicit*/ true);
        }
      } else {
        contextTypePurpose = CTP_YieldByValue;
      }

      TC.typeCheckExpression(exprToCheck, DC,
                             TypeLoc::withoutLoc(contextType),
                             contextTypePurpose);

      // Propagate the change into the inout expression we stripped before.
      if (inout) {
        inout->setSubExpr(exprToCheck);
        inout->setType(InOutType::get(yieldType));
        exprToCheck = inout;
      }

      // Note that this modifies the statement's expression list in-place.
      yieldExprs[i] = exprToCheck;
    }
    return YS;
  }
  
  Stmt *visitThrowStmt(ThrowStmt *TS) {
    // If the throw is in a defer, then it isn't valid.
    if (isInDefer()) {
      TC.diagnose(TS->getThrowLoc(), diag::jump_out_of_defer, "throw");
      return nullptr;
    }

    // Coerce the operand to the exception type.
    auto E = TS->getSubExpr();

    Type exnType = TC.getExceptionType(DC, TS->getThrowLoc());
    if (!exnType) return TS;

    TC.typeCheckExpression(E, DC, TypeLoc::withoutLoc(exnType), CTP_ThrowStmt);
    TS->setSubExpr(E);
    
    return TS;
  }

  Stmt *visitPoundAssertStmt(PoundAssertStmt *PA) {
    Expr *C = PA->getCondition();
    TC.typeCheckCondition(C, DC);
    PA->setCondition(C);
    return PA;
  }
    
  Stmt *visitDeferStmt(DeferStmt *DS) {
    TC.typeCheckDecl(DS->getTempDecl());

    Expr *theCall = DS->getCallExpr();
    TC.typeCheckExpression(theCall, DC);
    DS->setCallExpr(theCall);

    return DS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    StmtCondition C = IS->getCond();
    TC.typeCheckStmtCondition(C, DC, diag::if_always_true);
    IS->setCond(C);

    AddLabeledStmt ifNest(*this, IS);

    Stmt *S = IS->getThenStmt();
    typeCheckStmt(S);
    IS->setThenStmt(S);

    if ((S = IS->getElseStmt())) {
      typeCheckStmt(S);
      IS->setElseStmt(S);
    }
    
    return IS;
  }
  
  Stmt *visitGuardStmt(GuardStmt *GS) {
    StmtCondition C = GS->getCond();
    TC.typeCheckStmtCondition(C, DC, diag::guard_always_succeeds);
    GS->setCond(C);
    
    AddLabeledStmt ifNest(*this, GS);
    
    Stmt *S = GS->getBody();
    typeCheckStmt(S);
    GS->setBody(S);
    return GS;
  }

  Stmt *visitDoStmt(DoStmt *DS) {
    AddLabeledStmt loopNest(*this, DS);
    Stmt *S = DS->getBody();
    typeCheckStmt(S);
    DS->setBody(S);
    return DS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    StmtCondition C = WS->getCond();
    TC.typeCheckStmtCondition(C, DC, diag::while_always_true);
    WS->setCond(C);

    AddLabeledStmt loopNest(*this, WS);
    Stmt *S = WS->getBody();
    typeCheckStmt(S);
    WS->setBody(S);
    
    return WS;
  }
  Stmt *visitRepeatWhileStmt(RepeatWhileStmt *RWS) {
    {
      AddLabeledStmt loopNest(*this, RWS);
      Stmt *S = RWS->getBody();
      typeCheckStmt(S);
      RWS->setBody(S);
    }
    
    Expr *E = RWS->getCond();
    TC.typeCheckCondition(E, DC);
    RWS->setCond(E);
    return RWS;
  }
  
  Stmt *visitForEachStmt(ForEachStmt *S) {
    TypeResolutionOptions options(TypeResolverContext::InExpression);
    options |= TypeResolutionFlags::AllowUnspecifiedTypes;
    options |= TypeResolutionFlags::AllowUnboundGenerics;

    if (auto *P = TC.resolvePattern(S->getPattern(), DC,
                                    /*isStmtCondition*/false)) {
      S->setPattern(P);
    } else {
      S->getPattern()->setType(ErrorType::get(TC.Context));
      return nullptr;
    }
  
    if (TC.typeCheckPattern(S->getPattern(), DC, options)) {
      // FIXME: Handle errors better.
      S->getPattern()->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    if (TC.typeCheckForEachBinding(DC, S))
      return nullptr;

    if (auto *Where = S->getWhere()) {
      if (TC.typeCheckCondition(Where, DC))
        return nullptr;
      S->setWhere(Where);
    }


    // Retrieve the 'Sequence' protocol.
    ProtocolDecl *sequenceProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::Sequence);
    if (!sequenceProto) {
      return nullptr;
    }

    // Retrieve the 'Iterator' protocol.
    ProtocolDecl *iteratorProto =
        TC.getProtocol(S->getForLoc(), KnownProtocolKind::IteratorProtocol);
    if (!iteratorProto) {
      return nullptr;
    }

    Expr *sequence = S->getSequence();

    // Invoke iterator() to get an iterator from the sequence.
    Type iteratorTy;
    VarDecl *iterator;
    {
      Type sequenceType = sequence->getType();
      auto conformance =
        TypeChecker::conformsToProtocol(sequenceType, sequenceProto, DC,
                                        ConformanceCheckFlags::InExpression,
                                        sequence->getLoc());
      if (!conformance)
        return nullptr;
      S->setSequenceConformance(conformance);

      iteratorTy = conformance->getTypeWitnessByName(sequenceType,
                                                     TC.Context.Id_Iterator);
      if (iteratorTy->hasError())
        return nullptr;

      auto witness = conformance->getWitnessByName(
          sequenceType, TC.Context.Id_makeIterator);
      if (!witness)
        return nullptr;
      S->setMakeIterator(witness);

      // Create a local variable to capture the iterator.
      std::string name;
      if (auto np = dyn_cast_or_null<NamedPattern>(S->getPattern()))
        name = "$"+np->getBoundName().str().str();
      name += "$generator";

      iterator = new (TC.Context) VarDecl(
          /*IsStatic*/ false, VarDecl::Introducer::Var,
          /*IsCaptureList*/ false, S->getInLoc(),
          TC.Context.getIdentifier(name), DC);
      iterator->setType(iteratorTy);
      iterator->setInterfaceType(iteratorTy->mapTypeOutOfContext());
      iterator->setImplicit();
      S->setIteratorVar(iterator);

      auto genPat = new (TC.Context) NamedPattern(iterator);
      genPat->setImplicit();

      // TODO: test/DebugInfo/iteration.swift requires this extra info to
      // be around.
      auto nextResultType =
          OptionalType::get(conformance->getTypeWitnessByName(
                                sequenceType, TC.Context.Id_Element));
      PatternBindingDecl::createImplicit(
          TC.Context, StaticSpellingKind::None, genPat,
          new (TC.Context) OpaqueValueExpr(S->getInLoc(), nextResultType), DC,
          /*VarLoc*/ S->getForLoc());

      Type newSequenceType = cast<AbstractFunctionDecl>(witness.getDecl())
            ->getInterfaceType()
            ->castTo<AnyFunctionType>()
            ->getParams()[0].getPlainType().subst(witness.getSubstitutions());

      // Necessary type coersion for method application.
      if (TC.convertToType(sequence, newSequenceType, DC, None)) {
        return nullptr;
      }
      S->setSequence(sequence);
    }

    // Working with iterators requires Optional.
    if (TC.requireOptionalIntrinsics(S->getForLoc()))
      return nullptr;

    // Gather the witnesses from the Iterator protocol conformance, which
    // we'll use to drive the loop.
    // FIXME: Would like to customize the diagnostic emitted in
    // conformsToProtocol().
    auto genConformance = TypeChecker::conformsToProtocol(
        iteratorTy, iteratorProto, DC, ConformanceCheckFlags::InExpression,
        sequence->getLoc());
    if (!genConformance)
      return nullptr;

    Type elementTy = genConformance->getTypeWitnessByName(iteratorTy,
                                                        TC.Context.Id_Element);
    if (elementTy->hasError())
      return nullptr;

    auto *varRef =
        TC.buildCheckedRefExpr(iterator, DC, DeclNameLoc(S->getInLoc()),
                               /*implicit*/ true);
    if (!varRef)
      return nullptr;

    S->setIteratorVarRef(varRef);

    auto witness =
        genConformance->getWitnessByName(iteratorTy, TC.Context.Id_next);
    S->setIteratorNext(witness);

    auto nextResultType = cast<FuncDecl>(S->getIteratorNext().getDecl())
                              ->getResultInterfaceType()
                              .subst(S->getIteratorNext().getSubstitutions());

    // Convert that Optional<T> value to Optional<Element>.
    auto optPatternType = OptionalType::get(S->getPattern()->getType());
    if (!optPatternType->isEqual(nextResultType)) {
      OpaqueValueExpr *elementExpr =
          new (TC.Context) OpaqueValueExpr(S->getInLoc(), nextResultType);
      Expr *convertElementExpr = elementExpr;
      if (TC.convertToType(convertElementExpr, optPatternType, DC,
                           S->getPattern())) {
        return nullptr;
      }
      S->setElementExpr(elementExpr);
      S->setConvertElementExpr(convertElementExpr);
    }

    // Type-check the body of the loop.
    AddLabeledStmt loopNest(*this, S);
    BraceStmt *Body = S->getBody();
    typeCheckStmt(Body);
    S->setBody(Body);
    
    return S;
  }

  Stmt *visitBreakStmt(BreakStmt *S) {
    LabeledStmt *Target = nullptr;
    TopCollection<unsigned, LabeledStmt *> labelCorrections(3);
    // Pick the nearest break target that matches the specified name.
    if (S->getTargetName().empty()) {
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        // 'break' with no label looks through non-loop structures
        // except 'switch'.
        if (!(*I)->requiresLabelOnJump()) {
          Target = *I;
          break;
        }
      }

    } else {
      // Scan inside out until we find something with the right label.
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        if (S->getTargetName() == (*I)->getLabelInfo().Name) {
          Target = *I;
          break;
        } else {
          unsigned distance =
            TC.getCallEditDistance(S->getTargetName(), (*I)->getLabelInfo().Name,
                                   TypeChecker::UnreasonableCallEditDistance);
          if (distance < TypeChecker::UnreasonableCallEditDistance)
            labelCorrections.insert(distance, std::move(*I));
        }
      }
      labelCorrections.filterMaxScoreRange(
        TypeChecker::MaxCallEditDistanceFromBestCandidate);
    }
    
    if (!Target) {
      // If we're in a defer, produce a tailored diagnostic.
      if (isInDefer()) {
        TC.diagnose(S->getLoc(), diag::jump_out_of_defer, "break");
      } else if (S->getTargetName().empty()) {
        // If we're dealing with an unlabeled break inside of an 'if' or 'do'
        // statement, produce a more specific error.
        if (std::any_of(ActiveLabeledStmts.rbegin(),
                        ActiveLabeledStmts.rend(),
                        [&](Stmt *S) -> bool {
                          return isa<IfStmt>(S) || isa<DoStmt>(S);
                        })) {
          TC.diagnose(S->getLoc(), diag::unlabeled_break_outside_loop);
        } else {
          // Otherwise produce a generic error.
          TC.diagnose(S->getLoc(), diag::break_outside_loop);
        }
      } else {
        emitUnresolvedLabelDiagnostics(TC, S->getTargetLoc(), S->getTargetName(),
                                       labelCorrections);
      }
      return nullptr;
    }
    S->setTarget(Target);
    return S;
  }

  Stmt *visitContinueStmt(ContinueStmt *S) {
    LabeledStmt *Target = nullptr;
    TopCollection<unsigned, LabeledStmt *> labelCorrections(3);
    // Scan to see if we are in any non-switch labeled statements (loops).  Scan
    // inside out.
    if (S->getTargetName().empty()) {
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        // 'continue' with no label ignores non-loop structures.
        if (!(*I)->requiresLabelOnJump() &&
            (*I)->isPossibleContinueTarget()) {
          Target = *I;
          break;
        }
      }
    } else {
      // Scan inside out until we find something with the right label.
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        if (S->getTargetName() == (*I)->getLabelInfo().Name) {
          Target = *I;
          break;
        } else {
          unsigned distance =
            TC.getCallEditDistance(S->getTargetName(), (*I)->getLabelInfo().Name,
                                   TypeChecker::UnreasonableCallEditDistance);
          if (distance < TypeChecker::UnreasonableCallEditDistance)
            labelCorrections.insert(distance, std::move(*I));
        }
      }
      labelCorrections.filterMaxScoreRange(
        TypeChecker::MaxCallEditDistanceFromBestCandidate);
    }

    if (Target) {
      // Continue cannot be used to repeat switches, use fallthrough instead.
      if (!Target->isPossibleContinueTarget()) {
        TC.diagnose(S->getLoc(), diag::continue_not_in_this_stmt,
                    isa<SwitchStmt>(Target) ? "switch" : "if");
        return nullptr;
      }
    } else {
      // If we're in a defer, produce a tailored diagnostic.
      if (isInDefer()) {
        TC.diagnose(S->getLoc(), diag::jump_out_of_defer, "break");
      } else if (S->getTargetName().empty()) {
        // If we're dealing with an unlabeled continue, produce a generic error.
        TC.diagnose(S->getLoc(), diag::continue_outside_loop);
      } else {
        emitUnresolvedLabelDiagnostics(TC, S->getTargetLoc(), S->getTargetName(),
                                       labelCorrections);
      }
      return nullptr;
    }
    S->setTarget(Target);
    return S;
  }

  static void
  emitUnresolvedLabelDiagnostics(TypeChecker &tc, SourceLoc targetLoc, Identifier targetName,
                                 TopCollection<unsigned, LabeledStmt *> corrections) {
    // If an unresolved label was used, but we have a single correction,
    // produce the specific diagnostic and fixit.
    if (corrections.size() == 1) {
      tc.diagnose(targetLoc, diag::unresolved_label_corrected,
                  targetName, corrections.begin()->Value->getLabelInfo().Name)
        .highlight(SourceRange(targetLoc))
        .fixItReplace(SourceRange(targetLoc),
                      corrections.begin()->Value->getLabelInfo().Name.str());
      tc.diagnose(corrections.begin()->Value->getLabelInfo().Loc,
                  diag::decl_declared_here,
                  corrections.begin()->Value->getLabelInfo().Name);
    } else {
      // If we have multiple corrections or none, produce a generic diagnostic
      // and all corrections available.
      tc.diagnose(targetLoc, diag::unresolved_label, targetName)
        .highlight(SourceRange(targetLoc));
      for (auto &entry : corrections)
        tc.diagnose(entry.Value->getLabelInfo().Loc, diag::note_typo_candidate,
                    entry.Value->getLabelInfo().Name.str())
          .fixItReplace(SourceRange(targetLoc),
                        entry.Value->getLabelInfo().Name.str());
    }
  }
  
  Stmt *visitFallthroughStmt(FallthroughStmt *S) {
    if (!SwitchLevel) {
      TC.diagnose(S->getLoc(), diag::fallthrough_outside_switch);
      return nullptr;
    }
    if (!FallthroughDest) {
      TC.diagnose(S->getLoc(), diag::fallthrough_from_last_case);
      return nullptr;
    }
    S->setFallthroughSource(FallthroughSource);
    S->setFallthroughDest(FallthroughDest);
    PreviousFallthrough = S;
    return S;
  }

  void checkCaseLabelItemPattern(CaseStmt *caseBlock, CaseLabelItem &labelItem,
                                 bool &limitExhaustivityChecks,
                                 Type subjectType,
                                 SmallVectorImpl<VarDecl *> **prevCaseDecls,
                                 SmallVectorImpl<VarDecl *> **nextCaseDecls) {
    Pattern *pattern = labelItem.getPattern();
    auto *newPattern = TC.resolvePattern(pattern, DC,
                                         /*isStmtCondition*/ false);
    if (!newPattern) {
      pattern->collectVariables(**nextCaseDecls);
      std::swap(*prevCaseDecls, *nextCaseDecls);
      return;
    }

    pattern = newPattern;
    // Coerce the pattern to the subject's type.
    TypeResolutionOptions patternOptions(TypeResolverContext::InExpression);
    if (!subjectType ||
        TC.coercePatternToType(pattern, TypeResolution::forContextual(DC),
                               subjectType, patternOptions)) {
      limitExhaustivityChecks = true;

      // If that failed, mark any variables binding pieces of the pattern
      // as invalid to silence follow-on errors.
      pattern->forEachVariable([&](VarDecl *VD) { VD->markInvalid(); });
    }
    labelItem.setPattern(pattern);

    // If we do not have decls from the previous case that we need to match,
    // just return. This only happens with the first case label item.
    if (!*prevCaseDecls) {
      pattern->collectVariables(**nextCaseDecls);
      std::swap(*prevCaseDecls, *nextCaseDecls);
      return;
    }

    // Otherwise for each variable in the pattern, make sure its type is
    // identical to the initial case decl and stash the previous case decl as
    // the parent of the decl.
    pattern->forEachVariable([&](VarDecl *vd) {
      if (!vd->hasName())
        return;

      // We know that prev var decls matches the initial var decl. So if we can
      // match prevVarDecls, we can also match initial var decl... So for each
      // decl in prevVarDecls...
      for (auto *expected : **prevCaseDecls) {
        // If we do not match the name of vd, continue.
        if (!expected->hasName() || expected->getName() != vd->getName())
          continue;

        // Ok, we found a match! Before we leave, mark expected as the parent of
        // vd and add vd to the next case decl list for the next iteration.
        SWIFT_DEFER {
          vd->setParentVarDecl(expected);
          (*nextCaseDecls)->push_back(vd);
        };

        // Then we check for errors.
        //
        // NOTE: We emit the diagnostics against the initial case label item var
        // decl associated with expected to ensure that we always emit
        // diagnostics against a single reference var decl. If we used expected
        // instead, we would emit confusing diagnostics since a correct var decl
        // after an incorrect var decl will be marked as incorrect. For instance
        // given the following case statement.
        //
        //   case .a(let x), .b(var x), .c(let x):
        //
        // if we use expected, we will emit errors saying that .b(var x) needs
        // to be a let and .c(let x) needs to be a var. Thus if one
        // automatically applied both fix-its, one would still get an error
        // producing program:
        //
        //   case .a(let x), .b(let x), .c(var x):
        //
        // More complex case label item lists could cause even longer fixup
        // sequences. Thus, we emit errors against the VarDecl associated with
        // expected in the initial case label item list.
        //
        // Luckily, it is easy for us to compute this since we only change the
        // parent field of the initial case label item's VarDecls /after/ we
        // finish updating the parent pointers of the VarDecls associated with
        // all other CaseLabelItems. So that initial group of VarDecls are
        // guaranteed to still have a parent pointer pointing at our
        // CaseStmt. Since we have setup the parent pointer VarDecl linked list
        // for all other CaseLabelItem var decls that we have already processed,
        // we can use our VarDecl linked list to find that initial case label
        // item VarDecl.
        auto *initialCaseVarDecl = expected;
        while (auto *prev = initialCaseVarDecl->getParentVarDecl()) {
          initialCaseVarDecl = prev;
        }
        assert(isa<CaseStmt>(initialCaseVarDecl->getParentPatternStmt()));

        if (vd->hasType() && initialCaseVarDecl->hasType() &&
            !initialCaseVarDecl->isInvalid() &&
            !vd->getType()->isEqual(initialCaseVarDecl->getType())) {
          TC.diagnose(vd->getLoc(), diag::type_mismatch_multiple_pattern_list,
                      vd->getType(), initialCaseVarDecl->getType());
          vd->markInvalid();
          initialCaseVarDecl->markInvalid();
        }

        if (initialCaseVarDecl->isLet() == vd->isLet()) {
          return;
        }

        auto diag = TC.diagnose(vd->getLoc(),
                                diag::mutability_mismatch_multiple_pattern_list,
                                vd->isLet(), initialCaseVarDecl->isLet());

        VarPattern *foundVP = nullptr;
        vd->getParentPattern()->forEachNode([&](Pattern *P) {
          if (auto *VP = dyn_cast<VarPattern>(P))
            if (VP->getSingleVar() == vd)
              foundVP = VP;
        });
        if (foundVP)
          diag.fixItReplace(foundVP->getLoc(),
                            initialCaseVarDecl->isLet() ? "let" : "var");
        vd->markInvalid();
        initialCaseVarDecl->markInvalid();
      }
    });

    // Clear our previous case decl list and the swap in the new decls for the
    // next iteration.
    (*prevCaseDecls)->clear();
    std::swap(*prevCaseDecls, *nextCaseDecls);
  }

  void checkUnknownAttrRestrictions(CaseStmt *caseBlock,
                                    bool &limitExhaustivityChecks) {
    if (caseBlock->getCaseLabelItems().size() != 1) {
      assert(!caseBlock->getCaseLabelItems().empty() &&
             "parser should not produce case blocks with no items");
      TC.diagnose(caseBlock->getLoc(), diag::unknown_case_multiple_patterns)
          .highlight(caseBlock->getCaseLabelItems()[1].getSourceRange());
      limitExhaustivityChecks = true;
    }

    if (FallthroughDest != nullptr) {
      if (!caseBlock->isDefault())
        TC.diagnose(caseBlock->getLoc(), diag::unknown_case_must_be_last);
      limitExhaustivityChecks = true;
    }

    const auto &labelItem = caseBlock->getCaseLabelItems().front();
    if (labelItem.getGuardExpr() && !labelItem.isDefault()) {
      TC.diagnose(labelItem.getStartLoc(), diag::unknown_case_where_clause)
          .highlight(labelItem.getGuardExpr()->getSourceRange());
    }

    const Pattern *pattern =
        labelItem.getPattern()->getSemanticsProvidingPattern();
    if (!isa<AnyPattern>(pattern)) {
      TC.diagnose(labelItem.getStartLoc(), diag::unknown_case_must_be_catchall)
          .highlight(pattern->getSourceRange());
    }
  }

  void checkFallthroughPatternBindingsAndTypes(CaseStmt *caseBlock,
                                               CaseStmt *previousBlock) {
    auto firstPattern = caseBlock->getCaseLabelItems()[0].getPattern();
    SmallVector<VarDecl *, 4> vars;
    firstPattern->collectVariables(vars);

    // We know that the typechecker has already guaranteed that all of
    // the case label items in the fallthrough have the same var
    // decls. So if we match against the case body var decls,
    // transitively we will match all of the other case label items in
    // the fallthrough destination as well.
    auto previousVars = previousBlock->getCaseBodyVariablesOrEmptyArray();
    for (auto *expected : vars) {
      bool matched = false;
      if (!expected->hasName())
        continue;

      for (auto *previous : previousVars) {
        if (!previous->hasName() ||
            expected->getName() != previous->getName()) {
          continue;
        }

        if (!previous->getType()->isEqual(expected->getType())) {
          TC.diagnose(previous->getLoc(),
                      diag::type_mismatch_fallthrough_pattern_list,
                      previous->getType(), expected->getType());
          previous->markInvalid();
          expected->markInvalid();
        }

        // Ok, we found our match. Make the previous fallthrough statement var
        // decl our parent var decl.
        expected->setParentVarDecl(previous);
        matched = true;
        break;
      }

      if (!matched) {
        TC.diagnose(PreviousFallthrough->getLoc(),
                    diag::fallthrough_into_case_with_var_binding,
                    expected->getName());
      }
    }
  }

  Stmt *visitSwitchStmt(SwitchStmt *switchStmt) {
    // Type-check the subject expression.
    Expr *subjectExpr = switchStmt->getSubjectExpr();
    auto resultTy = TC.typeCheckExpression(subjectExpr, DC);
    auto limitExhaustivityChecks = !resultTy;
    if (Expr *newSubjectExpr = TC.coerceToRValue(subjectExpr))
      subjectExpr = newSubjectExpr;
    switchStmt->setSubjectExpr(subjectExpr);
    Type subjectType = switchStmt->getSubjectExpr()->getType();

    // Type-check the case blocks.
    AddSwitchNest switchNest(*this);
    AddLabeledStmt labelNest(*this, switchStmt);

    // Pre-emptively visit all Decls (#if/#warning/#error) that still exist in
    // the list of raw cases.
    for (auto &node : switchStmt->getRawCases()) {
      if (!node.is<Decl *>())
        continue;
      TC.typeCheckDecl(node.get<Decl *>());
    }

    SmallVector<VarDecl *, 8> scratchMemory1;
    SmallVector<VarDecl *, 8> scratchMemory2;

    auto cases = switchStmt->getCases();
    CaseStmt *previousBlock = nullptr;
    for (auto i = cases.begin(), e = cases.end(); i != e; ++i) {
      auto *caseBlock = *i;
      // Fallthrough transfers control to the next case block. In the
      // final case block, it is invalid.
      FallthroughSource = caseBlock;
      FallthroughDest = std::next(i) == e ? nullptr : *std::next(i);

      scratchMemory1.clear();
      scratchMemory2.clear();

      SmallVectorImpl<VarDecl *> *prevCaseDecls = nullptr;
      SmallVectorImpl<VarDecl *> *nextCaseDecls = &scratchMemory1;

      auto caseLabelItemArray = caseBlock->getMutableCaseLabelItems();
      {
        // Peel off the first iteration so we handle the first case label
        // especially since we use it to begin the validation chain.
        auto &labelItem = caseLabelItemArray.front();

        // Resolve the pattern in our case label if it has not been resolved and
        // check that our var decls follow invariants.
        checkCaseLabelItemPattern(caseBlock, labelItem, limitExhaustivityChecks,
                                  subjectType, &prevCaseDecls, &nextCaseDecls);

        // After this is complete, prevCaseDecls will be pointing at
        // scratchMemory1 which contains the initial case block's var decls and
        // nextCaseDecls will be a nullptr. Set nextCaseDecls to point at
        // scratchMemory2 for the next iterations.
        assert(prevCaseDecls == &scratchMemory1);
        assert(nextCaseDecls == nullptr);
        nextCaseDecls = &scratchMemory2;

        // Check the guard expression, if present.
        if (auto *guard = labelItem.getGuardExpr()) {
          limitExhaustivityChecks |= TC.typeCheckCondition(guard, DC);
          labelItem.setGuardExpr(guard);
        }
      }

      // Setup the types of our case body var decls.
      for (auto *expected : caseBlock->getCaseBodyVariablesOrEmptyArray()) {
        assert(expected->hasName());
        for (auto *prev : *prevCaseDecls) {
          if (!prev->hasName() || expected->getName() != prev->getName()) {
            continue;
          }
          if (prev->hasType())
            expected->setType(prev->getType());
          if (prev->hasInterfaceType())
            expected->setInterfaceType(prev->getInterfaceType());
          break;
        }
      }

      // Then check the rest.
      for (auto &labelItem : caseLabelItemArray.drop_front()) {
        // Resolve the pattern in our case label if it has not been resolved
        // and check that our var decls follow invariants.
        checkCaseLabelItemPattern(caseBlock, labelItem, limitExhaustivityChecks,
                                  subjectType, &prevCaseDecls, &nextCaseDecls);
        // Check the guard expression, if present.
        if (auto *guard = labelItem.getGuardExpr()) {
          limitExhaustivityChecks |= TC.typeCheckCondition(guard, DC);
          labelItem.setGuardExpr(guard);
        }
      }

      // Our last CaseLabelItem's VarDecls are now in
      // prevCaseDecls. Wire them up as parents of our case body var
      // decls.
      //
      // NOTE: We know that the two lists of var decls must be in sync. Remember
      // that we constructed our case body VarDecls from the first
      // CaseLabelItems var decls. Just now we proved that all other
      // CaseLabelItems have matching var decls of the first meaning
      // transitively that our last case label item must have matching var decls
      // for our case stmts CaseBodyVarDecls.
      //
      // NOTE: We do not check that we matched everything here. That is because
      // the check has already been done by comparing the 1st CaseLabelItem var
      // decls. If we insert a check here, we will emit the same error multiple
      // times.
      for (auto *expected : caseBlock->getCaseBodyVariablesOrEmptyArray()) {
        assert(expected->hasName());
        for (auto *prev : *prevCaseDecls) {
          if (!prev->hasName() || expected->getName() != prev->getName()) {
            continue;
          }
          expected->setParentVarDecl(prev);
          break;
        }
      }

      // Check restrictions on '@unknown'.
      if (caseBlock->hasUnknownAttr()) {
        checkUnknownAttrRestrictions(caseBlock, limitExhaustivityChecks);
      }

      // If the previous case fellthrough, similarly check that that case's bindings
      // includes our first label item's pattern bindings and types.
      if (PreviousFallthrough && previousBlock) {
        checkFallthroughPatternBindingsAndTypes(caseBlock, previousBlock);
      }

      // Type-check the body statements.
      PreviousFallthrough = nullptr;
      Stmt *body = caseBlock->getBody();
      limitExhaustivityChecks |= typeCheckStmt(body);
      caseBlock->setBody(body);
      previousBlock = caseBlock;
    }

    if (!switchStmt->isImplicit()) {
      TC.checkSwitchExhaustiveness(switchStmt, DC, limitExhaustivityChecks);
    }

    return switchStmt;
  }

  Stmt *visitCaseStmt(CaseStmt *S) {
    // Cases are handled in visitSwitchStmt.
    llvm_unreachable("case stmt outside of switch?!");
  }

  Stmt *visitCatchStmt(CatchStmt *S) {
    // Catches are handled in visitDoCatchStmt.
    llvm_unreachable("catch stmt outside of do-catch?!");
  }

  void checkCatchStmt(CatchStmt *S) {
    // Check the catch pattern.
    TC.typeCheckCatchPattern(S, DC);

    // Check the guard expression, if present.
    if (Expr *guard = S->getGuardExpr()) {
      TC.typeCheckCondition(guard, DC);
      S->setGuardExpr(guard);
    }
      
    // Type-check the clause body.
    Stmt *body = S->getBody();
    typeCheckStmt(body);
    S->setBody(body);
  }

  Stmt *visitDoCatchStmt(DoCatchStmt *S) {
    // The labels are in scope for both the 'do' and all of the catch
    // clauses.  This allows the user to break out of (or restart) the
    // entire construct.
    AddLabeledStmt loopNest(*this, S);

    // Type-check the 'do' body.  Type failures in here will generally
    // not cause type failures in the 'catch' clauses.
    Stmt *newBody = S->getBody();
    typeCheckStmt(newBody);
    S->setBody(newBody);

    // Check all the catch clauses independently.
    for (auto clause : S->getCatches()) {
      checkCatchStmt(clause);
    }
    
    return S;
  }

  Stmt *visitFailStmt(FailStmt *S) {
    // These are created as part of type-checking "return" in an initializer.
    // There is nothing more to do.
    return S;
  }

};
} // end anonymous namespace

bool TypeChecker::typeCheckCatchPattern(CatchStmt *S, DeclContext *DC) {
  // Grab the standard exception type.
  Type exnType = getExceptionType(DC, S->getCatchLoc());

  Pattern *pattern = S->getErrorPattern();
  if (Pattern *newPattern = resolvePattern(pattern, DC,
                                           /*isStmtCondition*/false)) {
    pattern = newPattern;

    // Coerce the pattern to the exception type.
    TypeResolutionOptions patternOptions(TypeResolverContext::InExpression);
    if (!exnType ||
        coercePatternToType(pattern, TypeResolution::forContextual(DC), exnType,
                            patternOptions)) {
      // If that failed, be sure to give the variables error types
      // before we type-check the guard.  (This will probably kill
      // most of the type-checking, but maybe not.)
      pattern->forEachVariable([&](VarDecl *var) {
        var->markInvalid();
      });
    }

    S->setErrorPattern(pattern);
  }
  return false;
}

static bool isDiscardableType(Type type) {
  return (type->hasError() ||
          type->isUninhabited() ||
          type->lookThroughAllOptionalTypes()->isVoid());
}

static void diagnoseIgnoredLiteral(TypeChecker &TC, LiteralExpr *LE) {
  const auto getLiteralDescription = [](LiteralExpr *LE) -> StringRef {
    switch (LE->getKind()) {
    case ExprKind::IntegerLiteral: return "integer";
    case ExprKind::FloatLiteral: return "floating-point";
    case ExprKind::BooleanLiteral: return "boolean";
    case ExprKind::StringLiteral: return "string";
    case ExprKind::InterpolatedStringLiteral: return "string";
    case ExprKind::MagicIdentifierLiteral:
      switch (cast<MagicIdentifierLiteralExpr>(LE)->getKind()) {
      case MagicIdentifierLiteralExpr::Kind::File: return "#file";
      case MagicIdentifierLiteralExpr::Kind::Line: return "#line";
      case MagicIdentifierLiteralExpr::Kind::Column: return "#column";
      case MagicIdentifierLiteralExpr::Kind::Function: return "#function";
      case MagicIdentifierLiteralExpr::Kind::DSOHandle: return "#dsohandle";
      }
    case ExprKind::NilLiteral: return "nil";
    case ExprKind::ObjectLiteral: return "object";

    // Define an unreachable case for all non-literal expressions.
    // This way, if a new literal is added in the future, the compiler
    // will warn that a case is missing from this switch.
#define LITERAL_EXPR(Id, Parent)
#define EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
      llvm_unreachable("Not a literal expression");
    }

    llvm_unreachable("Unhandled ExprKind in switch.");
  };

  TC.diagnose(LE->getLoc(), diag::expression_unused_literal,
              getLiteralDescription(LE))
    .highlight(LE->getSourceRange());
}

void TypeChecker::checkIgnoredExpr(Expr *E) {
  // For parity with C, several places in the grammar accept multiple
  // comma-separated expressions and then bind them together as an implicit
  // tuple.  Break these apart and check them separately.
  if (E->isImplicit() && isa<TupleExpr>(E)) {
    for (auto Elt : cast<TupleExpr>(E)->getElements()) {
      checkIgnoredExpr(Elt);
    }
    return;
  }

  // Skip checking if there is no type, which presumably means there was a
  // type error.
  if (!E->getType()) {
    return;
  }

  // Complain about l-values that are neither loaded nor stored.
  if (E->getType()->hasLValueType()) {
    // This must stay in sync with diag::expression_unused_lvalue.
    enum {
        SK_Variable = 0,
        SK_Property,
        SK_Subscript
    } storageKind = SK_Variable;
    if (auto declRef = E->getReferencedDecl()) {
      auto decl = declRef.getDecl();
      if (isa<SubscriptDecl>(decl))
        storageKind = SK_Subscript;
      else if (decl->getDeclContext()->isTypeContext())
        storageKind = SK_Property;
    }
    diagnose(E->getLoc(), diag::expression_unused_lvalue, storageKind)
      .highlight(E->getSourceRange());
    return;
  }

  // Drill through noop expressions we don't care about.
  auto valueE = E;
  while (1) {
    valueE = valueE->getValueProvidingExpr();
    
    if (auto *OEE = dyn_cast<OpenExistentialExpr>(valueE))
      valueE = OEE->getSubExpr();
    else if (auto *CRCE = dyn_cast<CovariantReturnConversionExpr>(valueE))
      valueE = CRCE->getSubExpr();
    else if (auto *EE = dyn_cast<ErasureExpr>(valueE))
      valueE = EE->getSubExpr();
    else
      break;
  }
  
  // Complain about functions that aren't called.
  // TODO: What about tuples which contain functions by-value that are
  // dead?
  if (E->getType()->is<AnyFunctionType>()) {
    bool isDiscardable = false;

    // The called function could be wrapped inside a `dot_syntax_call_expr`
    // node, for example:
    //
    // class Bar {
    //   @discardableResult
    //   func foo() -> Int { return 0 }
    //
    //   func baz() {
    //     self.foo
    //     foo
    //   }
    // }
    //
    // So look through the DSCE and get the function being called.
    auto expr =
        isa<DotSyntaxCallExpr>(E) ? cast<DotSyntaxCallExpr>(E)->getFn() : E;

    if (auto *Fn = dyn_cast<ApplyExpr>(expr)) {
      if (auto *declRef = dyn_cast<DeclRefExpr>(Fn->getFn())) {
        if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(declRef->getDecl())) {
          if (funcDecl->getAttrs().hasAttribute<DiscardableResultAttr>()) {
            isDiscardable = true;
          }
        }
      }
    }

    if (!isDiscardable) {
      diagnose(E->getLoc(), diag::expression_unused_function)
          .highlight(E->getSourceRange());
      return;
    }
  }

  // If the result of this expression is of type "Never" or "()"
  // (the latter potentially wrapped in optionals) then it is
  // safe to ignore.
  if (isDiscardableType(valueE->getType()))
    return;
  
  // Complain about '#selector'.
  if (auto *ObjCSE = dyn_cast<ObjCSelectorExpr>(valueE)) {
    diagnose(ObjCSE->getLoc(), diag::expression_unused_selector_result)
      .highlight(E->getSourceRange());
    return;
  }

  // Complain about '#keyPath'.
  if (isa<KeyPathExpr>(valueE)) {
    diagnose(valueE->getLoc(), diag::expression_unused_keypath_result)
      .highlight(E->getSourceRange());
    return;
  }
    
  // Always complain about 'try?'.
  if (auto *OTE = dyn_cast<OptionalTryExpr>(valueE)) {
    diagnose(OTE->getTryLoc(), diag::expression_unused_optional_try)
      .highlight(E->getSourceRange());
    return;
  }

  // If we have an OptionalEvaluationExpr at the top level, then someone is
  // "optional chaining" and ignoring the result.  Produce a diagnostic if it
  // doesn't make sense to ignore it.
  if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(valueE)) {
    if (auto *IIO = dyn_cast<InjectIntoOptionalExpr>(OEE->getSubExpr()))
      return checkIgnoredExpr(IIO->getSubExpr());
    if (auto *C = dyn_cast<CallExpr>(OEE->getSubExpr()))
      return checkIgnoredExpr(C);
    if (auto *OE = dyn_cast<OpenExistentialExpr>(OEE->getSubExpr()))
      return checkIgnoredExpr(OE);
  }

  if (auto *LE = dyn_cast<LiteralExpr>(valueE)) {
    diagnoseIgnoredLiteral(*this, LE);
    return;
  }

  // Check if we have a call to a function not marked with
  // '@discardableResult'.
  if (auto call = dyn_cast<ApplyExpr>(valueE)) {
    // Dig through all levels of calls.
    Expr *fn = call->getFn();
    while (true) {
      fn = fn->getSemanticsProvidingExpr();
      if (auto applyFn = dyn_cast<ApplyExpr>(fn)) {
        fn = applyFn->getFn();
      } else if (auto FVE = dyn_cast<ForceValueExpr>(fn)) {
        fn = FVE->getSubExpr();
      } else if (auto dotSyntaxRef = dyn_cast<DotSyntaxBaseIgnoredExpr>(fn)) {
        fn = dotSyntaxRef->getRHS();
      } else {
        break;
      }
    }

    // Find the callee.
    AbstractFunctionDecl *callee = nullptr;
    if (auto declRef = dyn_cast<DeclRefExpr>(fn))
      callee = dyn_cast<AbstractFunctionDecl>(declRef->getDecl());
    else if (auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn))
      callee = ctorRef->getDecl();
    else if (auto memberRef = dyn_cast<MemberRefExpr>(fn))
      callee = dyn_cast<AbstractFunctionDecl>(memberRef->getMember().getDecl());
    else if (auto dynMemberRef = dyn_cast<DynamicMemberRefExpr>(fn))
      callee = dyn_cast<AbstractFunctionDecl>(
                 dynMemberRef->getMember().getDecl());
    
    // If the callee explicitly allows its result to be ignored, then don't
    // complain.
    if (callee && callee->getAttrs().getAttribute<DiscardableResultAttr>())
      return;

    // Otherwise, complain.  Start with more specific diagnostics.

    // Diagnose unused literals that were translated to implicit
    // constructor calls during CSApply / ExprRewriter::convertLiteral.
    if (call->isImplicit()) {
      Expr *arg = call->getArg();
      if (auto *TE = dyn_cast<TupleExpr>(arg))
        if (TE->getNumElements() == 1)
          arg = TE->getElement(0);

      if (auto *LE = dyn_cast<LiteralExpr>(arg)) {
        diagnoseIgnoredLiteral(*this, LE);
        return;
      }
    }

    // Other unused constructor calls.
    if (callee && isa<ConstructorDecl>(callee) && !call->isImplicit()) {
      diagnose(fn->getLoc(), diag::expression_unused_init_result,
               callee->getDeclContext()->getDeclaredInterfaceType())
        .highlight(call->getArg()->getSourceRange());
      return;
    }
    
    SourceRange SR1 = call->getArg()->getSourceRange(), SR2;
    if (auto *BO = dyn_cast<BinaryExpr>(call)) {
      SR1 = BO->getArg()->getElement(0)->getSourceRange();
      SR2 = BO->getArg()->getElement(1)->getSourceRange();
    }
    
    // Otherwise, produce a generic diagnostic.
    if (callee) {
      auto &ctx = callee->getASTContext();
      if (callee->isImplicit()) {
        // Translate calls to implicit functions to their user-facing names
        if (callee->getBaseName() == ctx.Id_derived_enum_equals ||
            callee->getBaseName() == ctx.Id_derived_struct_equals) {
          diagnose(fn->getLoc(), diag::expression_unused_result_operator,
                   ctx.Id_EqualsOperator)
            .highlight(SR1).highlight(SR2);
          return;
        }
      }

      auto diagID = diag::expression_unused_result_call;
      if (callee->getFullName().isOperator())
        diagID = diag::expression_unused_result_operator;
      
      diagnose(fn->getLoc(), diagID, callee->getFullName())
        .highlight(SR1).highlight(SR2);
    } else
      diagnose(fn->getLoc(), diag::expression_unused_result_unknown,
               isa<ClosureExpr>(fn), valueE->getType())
        .highlight(SR1).highlight(SR2);

    return;
  }

  // Produce a generic diagnostic.
  diagnose(valueE->getLoc(), diag::expression_unused_result, valueE->getType())
    .highlight(valueE->getSourceRange());
}

Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  const SourceManager &SM = TC.Context.SourceMgr;

  // Diagnose defer statement being last one in block (only if
  // BraceStmt does not start a TopLevelDecl).
  if (IsBraceStmtFromTopLevelDecl) {
    IsBraceStmtFromTopLevelDecl = false;
  } else if (BS->getNumElements() > 0) {
    if (auto stmt =
            BS->getElement(BS->getNumElements() - 1).dyn_cast<Stmt *>()) {
      if (auto deferStmt = dyn_cast<DeferStmt>(stmt)) {
        TC.diagnose(deferStmt->getStartLoc(), diag::defer_stmt_at_block_end)
            .fixItReplace(deferStmt->getStartLoc(), "do");
      }
    }
  }

  for (auto &elem : BS->getElements()) {
    if (auto *SubExpr = elem.dyn_cast<Expr*>()) {
      SourceLoc Loc = SubExpr->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      // Type check the expression.
      TypeCheckExprOptions options = TypeCheckExprFlags::IsExprStmt;
      bool isDiscarded = !(IsREPL && isa<TopLevelCodeDecl>(DC))
        && !TC.Context.LangOpts.Playground
        && !TC.Context.LangOpts.DebuggerSupport;
      if (isDiscarded)
        options |= TypeCheckExprFlags::IsDiscarded;

      if (EndTypeCheckLoc.isValid()) {
        assert(DiagnosticSuppression::isEnabled(TC.Diags) &&
               "Diagnosing and AllowUnresolvedTypeVariables don't seem to mix");
        options |= TypeCheckExprFlags::AllowUnresolvedTypeVariables;
      }

      auto resultTy =
          TC.typeCheckExpression(SubExpr, DC, TypeLoc(), CTP_Unused, options);

      // If a closure expression is unused, the user might have intended
      // to write "do { ... }".
      auto *CE = dyn_cast<ClosureExpr>(SubExpr);
      if (CE || isa<CaptureListExpr>(SubExpr)) {
        TC.diagnose(SubExpr->getLoc(), diag::expression_unused_closure);
        
        if (CE && CE->hasAnonymousClosureVars() &&
            CE->getParameters()->size() == 0) {
          TC.diagnose(CE->getStartLoc(), diag::brace_stmt_suggest_do)
            .fixItInsert(CE->getStartLoc(), "do ");
        }
      } else if (isDiscarded && resultTy)
        TC.checkIgnoredExpr(SubExpr);

      elem = SubExpr;
      continue;
    }

    if (auto *SubStmt = elem.dyn_cast<Stmt*>()) {
      SourceLoc Loc = SubStmt->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      typeCheckStmt(SubStmt);
      elem = SubStmt;
      continue;
    }

    Decl *SubDecl = elem.get<Decl *>();
    SourceLoc Loc = SubDecl->getStartLoc();
    if (EndTypeCheckLoc.isValid() &&
        (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
      break;

    TC.typeCheckDecl(SubDecl);
  }

  return BS;
}

static Optional<unsigned>
getParamIndex(const ParameterList *paramList, const ParamDecl *decl) {
  ArrayRef<ParamDecl *> params = paramList->getArray();
  for (unsigned i = 0; i < params.size(); ++i) {
    if (params[i] == decl) return i;
  }
  return None;
}

static void
checkInheritedDefaultValueRestrictions(TypeChecker &TC, ParamDecl *PD) {
  if (PD->getDefaultArgumentKind() != DefaultArgumentKind::Inherited)
    return;

  auto *DC = PD->getInnermostDeclContext();
  const SourceFile *SF = DC->getParentSourceFile();
  assert((SF && SF->Kind == SourceFileKind::Interface || PD->isImplicit()) &&
         "explicit inherited default argument outside of a module interface?");

  // The containing decl should be a designated initializer.
  auto ctor = dyn_cast<ConstructorDecl>(DC);
  if (!ctor || ctor->isConvenienceInit()) {
    TC.diagnose(
        PD, diag::inherited_default_value_not_in_designated_constructor);
    return;
  }

  // The decl it overrides should also be a designated initializer.
  auto overridden = ctor->getOverriddenDecl();
  if (!overridden || overridden->isConvenienceInit()) {
    TC.diagnose(
        PD, diag::inherited_default_value_used_in_non_overriding_constructor);
    if (overridden)
      TC.diagnose(overridden, diag::overridden_here);
    return;
  }

  // The corresponding parameter should have a default value.
  Optional<unsigned> idx = getParamIndex(ctor->getParameters(), PD);
  assert(idx && "containing decl does not contain param?");
  ParamDecl *equivalentParam = overridden->getParameters()->get(*idx);
  if (equivalentParam->getDefaultArgumentKind() == DefaultArgumentKind::None) {
    TC.diagnose(PD, diag::corresponding_param_not_defaulted);
    TC.diagnose(equivalentParam, diag::inherited_default_param_here);
  }
}

/// Check the default arguments that occur within this pattern.
void TypeChecker::checkDefaultArguments(ParameterList *params,
                                        ValueDecl *VD) {
  for (auto *param : *params) {
    checkInheritedDefaultValueRestrictions(*this, param);
    if (!param->getDefaultValue() ||
        !param->hasInterfaceType() ||
        param->getInterfaceType()->hasError())
      continue;

    Expr *e = param->getDefaultValue();
    auto *initContext = param->getDefaultArgumentInitContext();

    auto resultTy =
        typeCheckParameterDefault(e, initContext, param->getType(),
                                  /*isAutoClosure=*/param->isAutoClosure());

    if (resultTy) {
      param->setDefaultValue(e);
    }

    checkInitializerErrorHandling(initContext, e);

    // Walk the checked initializer and contextualize any closures
    // we saw there.
    (void)contextualizeInitializer(initContext, e);
  }
}

static Type getFunctionBuilderType(FuncDecl *FD) {
  Type builderType = FD->getFunctionBuilderType();

  // For getters, fall back on looking on the attribute on the storage.
  if (!builderType) {
    auto accessor = dyn_cast<AccessorDecl>(FD);
    if (accessor && accessor->getAccessorKind() == AccessorKind::Get) {
      builderType = accessor->getStorage()->getFunctionBuilderType();
    }
  }

  return builderType;
}

bool TypeChecker::typeCheckAbstractFunctionBody(AbstractFunctionDecl *AFD) {
  auto result = typeCheckAbstractFunctionBodyUntil(AFD, SourceLoc());
  checkFunctionErrorHandling(AFD);
  return result;
}

static Expr* constructCallToSuperInit(ConstructorDecl *ctor,
                                      ClassDecl *ClDecl) {
  ASTContext &Context = ctor->getASTContext();
  Expr *superRef = new (Context) SuperRefExpr(ctor->getImplicitSelfDecl(),
                                              SourceLoc(), /*Implicit=*/true);
  Expr *r = new (Context) UnresolvedDotExpr(superRef, SourceLoc(),
                                            DeclBaseName::createConstructor(),
                                            DeclNameLoc(),
                                            /*Implicit=*/true);
  r = CallExpr::createImplicit(Context, r, { }, { });

  if (ctor->hasThrows())
    r = new (Context) TryExpr(SourceLoc(), r, Type(), /*implicit=*/true);

  TypeChecker &tc = *static_cast<TypeChecker *>(Context.getLazyResolver());
  DiagnosticSuppression suppression(tc.Diags);
  auto resultTy =
      tc.typeCheckExpression(r, ctor, TypeLoc(), CTP_Unused,
                             TypeCheckExprFlags::IsDiscarded);
  if (!resultTy)
    return nullptr;
  
  return r;
}

/// Check a super.init call.
///
/// \returns true if an error occurred.
static bool checkSuperInit(TypeChecker &tc, ConstructorDecl *fromCtor, 
                           ApplyExpr *apply, bool implicitlyGenerated) {
  // Make sure we are referring to a designated initializer.
  auto otherCtorRef = dyn_cast<OtherConstructorDeclRefExpr>(
                        apply->getSemanticFn());
  if (!otherCtorRef)
    return false;
  
  auto ctor = otherCtorRef->getDecl();
  if (!ctor->isDesignatedInit()) {
    if (!implicitlyGenerated) {
      auto selfTy = fromCtor->getDeclContext()->getSelfInterfaceType();
      if (auto classTy = selfTy->getClassOrBoundGenericClass()) {
        assert(classTy->getSuperclass());
        tc.diagnose(apply->getArg()->getLoc(), diag::chain_convenience_init,
                    classTy->getSuperclass());
        ctor->diagnose(diag::convenience_init_here);
      }
    }
    return true;
  }
  
  // For an implicitly generated super.init() call, make sure there's
  // only one designated initializer.
  if (implicitlyGenerated) {
    auto superclassTy = ctor->getDeclContext()->getDeclaredInterfaceType();
    auto lookupOptions = defaultConstructorLookupOptions;
    lookupOptions |= NameLookupFlags::KnownPrivate;

    // If a constructor is only visible as a witness for a protocol
    // requirement, it must be an invalid override. Also, protocol
    // extensions cannot yet define designated initializers.
    lookupOptions -= NameLookupFlags::ProtocolMembers;
    lookupOptions -= NameLookupFlags::PerformConformanceCheck;

    for (auto member : TypeChecker::lookupConstructors(fromCtor, superclassTy,
                                                       lookupOptions)) {
      auto superclassCtor = dyn_cast<ConstructorDecl>(member.getValueDecl());
      if (!superclassCtor || !superclassCtor->isDesignatedInit() ||
          superclassCtor == ctor)
        continue;
      
      // Found another designated initializer in the superclass. Don't add the
      // super.init() call.
      return true;
    }

    // Make sure we can reference the designated initializer correctly.
    if (fromCtor->getResilienceExpansion() == ResilienceExpansion::Minimal) {
      TypeChecker::FragileFunctionKind fragileKind;
      bool treatUsableFromInlineAsPublic;
      std::tie(fragileKind, treatUsableFromInlineAsPublic) =
          TypeChecker::getFragileFunctionKind(fromCtor);
      tc.diagnoseInlinableDeclRef(
          fromCtor->getLoc(), ctor, fromCtor, fragileKind,
          treatUsableFromInlineAsPublic);
    }
  }

  return false;
}

static bool isKnownEndOfConstructor(ASTNode N) {
  auto *S = N.dyn_cast<Stmt*>();
  if (!S) return false;

  return isa<ReturnStmt>(S) || isa<FailStmt>(S);
}

/// Check for problems specific to the body of a constructor within a
/// class, involving (e.g.) implicit calls to the superclass initializer and
/// issues related to designated/convenience initializers.
static void checkClassConstructorBody(ClassDecl *classDecl,
                                      ConstructorDecl *ctor,
                                      BraceStmt *body) {
  ASTContext &ctx = classDecl->getASTContext();
  TypeChecker &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
  bool wantSuperInitCall = false;
  bool isDelegating = false;
  ApplyExpr *initExpr = nullptr;
  switch (ctor->getDelegatingOrChainedInitKind(&ctx.Diags, &initExpr)) {
  case ConstructorDecl::BodyInitKind::Delegating:
    isDelegating = true;
    wantSuperInitCall = false;
    break;

  case ConstructorDecl::BodyInitKind::Chained:
    checkSuperInit(tc, ctor, initExpr, false);

    /// A convenience initializer cannot chain to a superclass constructor.
    if (ctor->isConvenienceInit()) {
      ctx.Diags.diagnose(initExpr->getLoc(),
                         diag::delegating_convenience_super_init,
                         ctor->getDeclContext()->getDeclaredInterfaceType());
    }

    LLVM_FALLTHROUGH;

  case ConstructorDecl::BodyInitKind::None:
    wantSuperInitCall = false;
    break;

  case ConstructorDecl::BodyInitKind::ImplicitChained:
    wantSuperInitCall = true;
    break;
  }

  // A class designated initializer must never be delegating.
  if (ctor->isDesignatedInit() && isDelegating) {
    ctor->diagnose(diag::delegating_designated_init,
                   ctor->getDeclContext()->getDeclaredInterfaceType())
      .fixItInsert(ctor->getLoc(), "convenience ");
    ctx.Diags.diagnose(initExpr->getLoc(), diag::delegation_here);
  }

  // An inlinable constructor in a class must always be delegating,
  // unless the class is '@_fixed_layout'.
  // Note: This is specifically not using isFormallyResilient. We relax this
  // rule for classes in non-resilient modules so that they can have inlinable
  // constructors, as long as those constructors don't reference private
  // declarations.
  if (!isDelegating && classDecl->isResilient() &&
      ctor->getResilienceExpansion() == ResilienceExpansion::Minimal) {
    auto kind = TypeChecker::getFragileFunctionKind(ctor);
    ctor->diagnose(diag::class_designated_init_inlinable_resilient,
                   classDecl->getDeclaredInterfaceType(),
                   static_cast<unsigned>(kind.first));
  }

  // If we don't want a super.init call, we're done.
  if (!wantSuperInitCall)
    return;

  // Find a default initializer in the superclass.
  Expr *SuperInitCall = constructCallToSuperInit(ctor, classDecl);
  if (!SuperInitCall)
    return;

  // If the initializer we found is a designated initializer, we're okay.
  class FindOtherConstructorRef : public ASTWalker {
  public:
    ApplyExpr *Found = nullptr;

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto apply = dyn_cast<ApplyExpr>(E)) {
        if (isa<OtherConstructorDeclRefExpr>(apply->getSemanticFn())) {
          Found = apply;
          return { false, E };
        }
      }

      return { Found == nullptr, E };
    }
  };

  FindOtherConstructorRef finder;
  SuperInitCall->walk(finder);
  if (!checkSuperInit(tc, ctor, finder.Found, true)) {
    // Store the super.init expression within the constructor declaration
    // to be emitted during SILGen.
    ctor->setSuperInitCall(SuperInitCall);
  }
}

llvm::Expected<bool>
TypeCheckFunctionBodyUntilRequest::evaluate(Evaluator &evaluator,
                                            AbstractFunctionDecl *AFD,
                                            SourceLoc endTypeCheckLoc) const {
  ASTContext &ctx = AFD->getASTContext();

  // Accounting for type checking of function bodies.
  // FIXME: We could probably take this away, given that the request-evaluator
  // does much of it for us.
  FrontendStatsTracer StatsTracer(ctx.Stats, "typecheck-fn", AFD);
  PrettyStackTraceDecl StackEntry("type-checking", AFD);

  if (ctx.Stats)
    ctx.Stats->getFrontendCounters().NumFunctionsTypechecked++;

  Optional<FunctionBodyTimer> timer;
  TypeChecker &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
  if (tc.DebugTimeFunctionBodies || tc.WarnLongFunctionBodies)
    timer.emplace(AFD, tc.DebugTimeFunctionBodies, tc.WarnLongFunctionBodies);

  tc.validateDecl(AFD);
  tc.checkDefaultArguments(AFD->getParameters(), AFD);

  BraceStmt *body = AFD->getBody();
  if (!body || AFD->isBodyTypeChecked())
    return false;

  if (auto *func = dyn_cast<FuncDecl>(AFD)) {
    if (Type builderType = getFunctionBuilderType(func)) {
      body = tc.applyFunctionBuilderBodyTransform(func, body, builderType);
      if (!body)
        return true;
    } else if (func->hasSingleExpressionBody()) {
      auto resultTypeLoc = func->getBodyResultTypeLoc();
      auto expr = func->getSingleExpressionBody();

      if (resultTypeLoc.isNull() || resultTypeLoc.getType()->isVoid()) {
        // The function returns void.  We don't need an explicit return, no matter
        // what the type of the expression is.  Take the inserted return back out.
        body->setElement(0, expr);
      }
    }
  } else if (isa<ConstructorDecl>(AFD) &&
             (body->getNumElements() == 0 ||
                !isKnownEndOfConstructor(body->getElements().back()))) {
    // For constructors, we make sure that the body ends with a "return" stmt,
    // which we either implicitly synthesize, or the user can write.  This
    // simplifies SILGen.
    SmallVector<ASTNode, 8> Elts(body->getElements().begin(),
                                 body->getElements().end());
    Elts.push_back(new (ctx) ReturnStmt(body->getRBraceLoc(),
                                        /*value*/nullptr,
                                        /*implicit*/true));
    body = BraceStmt::create(ctx, body->getLBraceLoc(), Elts,
                             body->getRBraceLoc(), body->isImplicit());
  }

  StmtChecker SC(tc, AFD);
  SC.EndTypeCheckLoc = endTypeCheckLoc;
  bool hadError = SC.typeCheckBody(body);

  // If this was a function with a single expression body, let's see
  // if implicit return statement came out to be `Never` which means
  // that we have eagerly converted something like `{ fatalError() }`
  // into `{ return fatalError() }` that has to be corrected here.
  if (isa<FuncDecl>(AFD) && cast<FuncDecl>(AFD)->hasSingleExpressionBody()) {
    if (auto *stmt = body->getElement(0).dyn_cast<Stmt *>()) {
      if (auto *retStmt = dyn_cast<ReturnStmt>(stmt)) {
        if (retStmt->isImplicit() && retStmt->hasResult()) {
          auto returnType = retStmt->getResult()->getType();
          if (returnType && returnType->isUninhabited())
            body->setElement(0, retStmt->getResult());
        }
      }
    }
  }

  // Class constructor checking.
  if (auto *ctor = dyn_cast<ConstructorDecl>(AFD)) {
    if (auto classDecl = ctor->getDeclContext()->getSelfClassDecl()) {
      checkClassConstructorBody(classDecl, ctor, body);
    }
  }

  // If nothing went wrong yet, perform extra checking.
  if (!hadError && endTypeCheckLoc.isInvalid())
    performAbstractFuncDeclDiagnostics(tc, AFD, body);

  // Wire up the function body now.
  AFD->setBody(body, AbstractFunctionDecl::BodyKind::TypeChecked);
  return hadError;
}

bool TypeChecker::typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                                     SourceLoc EndTypeCheckLoc) {
  return evaluateOrDefault(
             Context.evaluator,
             TypeCheckFunctionBodyUntilRequest{AFD, EndTypeCheckLoc},
             true);
}

bool TypeChecker::typeCheckClosureBody(ClosureExpr *closure) {
  checkParameterAttributes(closure->getParameters());

  BraceStmt *body = closure->getBody();

  Optional<FunctionBodyTimer> timer;
  if (DebugTimeFunctionBodies || WarnLongFunctionBodies)
    timer.emplace(closure, DebugTimeFunctionBodies, WarnLongFunctionBodies);

  bool HadError = StmtChecker(*this, closure).typeCheckBody(body);
  if (body) {
    closure->setBody(body, closure->hasSingleExpressionBody());
  }
  return HadError;
}

bool TypeChecker::typeCheckTapBody(TapExpr *expr, DeclContext *DC) {
  // We intentionally use typeCheckStmt instead of typeCheckBody here
  // because we want to contextualize TapExprs with the body they're in.
  BraceStmt *body = expr->getBody();
  bool HadError = StmtChecker(*this, DC).typeCheckStmt(body);
  if (body) {
    expr->setBody(body);
  }
  return HadError;
}

void TypeChecker::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  // We intentionally use typeCheckStmt instead of typeCheckBody here
  // because we want to contextualize all the TopLevelCode
  // declarations simultaneously.
  BraceStmt *Body = TLCD->getBody();
  StmtChecker(*this, TLCD).typeCheckStmt(Body);
  TLCD->setBody(Body);
  checkTopLevelErrorHandling(TLCD);
  performTopLevelDeclDiagnostics(*this, TLCD);
}
