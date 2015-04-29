//===--- TypeCheckStmt.cpp - Type Checking for Statements -----------------===//
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
// This file implements semantic analysis for statements.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "MiscDiagnostics.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/LocalContext.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Timer.h"

using namespace swift;

namespace {
  class ContextualizeClosures : public ASTWalker {
    DeclContext *ParentDC;
  public:
    unsigned NextDiscriminator = 0;

    ContextualizeClosures(DeclContext *parent,
                          unsigned nextDiscriminator = 0)
      : ParentDC(parent), NextDiscriminator(nextDiscriminator) {}

    /// Change the context we're contextualizing to.  This is
    /// basically only reasonable when processing all the different
    /// top-level code declarations.
    void setContext(TopLevelCodeDecl *parent) {
      ParentDC = parent;
    }

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

      // Explicit closures start their own sequence.
      if (auto CE = dyn_cast<ClosureExpr>(E)) {
        // In the repl, the parent top-level context may have been re-written.
        if (CE->getParent() != ParentDC) {
          if ((CE->getParent()->getContextKind() !=
                    ParentDC->getContextKind()) ||
              ParentDC->getContextKind() != DeclContextKind::TopLevelCodeDecl) {
          // If a closure is nested within an auto closure, we'll need to update
          // its parent to the auto closure parent.
          assert(ParentDC->getContextKind() ==
                 DeclContextKind::AbstractClosureExpr &&
                 "Incorrect parent decl context for closure");
          CE->setParent(ParentDC);
          }
        }

        // If the closure has a single expression body, we need to
        // walk into it with a new sequence.  Otherwise, it'll have
        // been separately type-checked.
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

  class FunctionBodyTimer {
    PointerUnion<const AbstractFunctionDecl *,
                 const AbstractClosureExpr *> Function;
    llvm::TimeRecord StartTime = llvm::TimeRecord::getCurrentTime();

  public:
    FunctionBodyTimer(decltype(Function) Fn) : Function(Fn) {}
    ~FunctionBodyTimer() {
      llvm::TimeRecord endTime = llvm::TimeRecord::getCurrentTime(false);

      auto elapsed = endTime.getProcessTime() - StartTime.getProcessTime();
      llvm::errs() << llvm::format("%0.1f", elapsed * 1000) << "ms\t";

      if (auto *AFD = Function.dyn_cast<const AbstractFunctionDecl *>()) {
        AFD->getLoc().print(llvm::errs(), AFD->getASTContext().SourceMgr);
        llvm::errs() << "\t";
        AFD->print(llvm::errs(), PrintOptions());
      } else {
        auto *ACE = Function.get<const AbstractClosureExpr *>();
        ACE->getLoc().print(llvm::errs(), ACE->getASTContext().SourceMgr);
        llvm::errs() << "\t(closure)";
      }
      llvm::errs() << "\n";
    }
  };
}

static void setAutoClosureDiscriminators(DeclContext *DC, Stmt *S) {
  S->walk(ContextualizeClosures(DC));
}

bool TypeChecker::contextualizeInitializer(Initializer *DC, Expr *E) {
  ContextualizeClosures CC(DC);
  E->walk(CC);
  return CC.hasAutoClosures();
}

void TypeChecker::contextualizeTopLevelCode(TopLevelContext &TLC,
                                            ArrayRef<Decl*> topLevel) {
  unsigned nextDiscriminator = TLC.NextAutoClosureDiscriminator;
  ContextualizeClosures CC(nullptr, nextDiscriminator);
  for (auto decl : topLevel) {
    auto topLevelCode = dyn_cast<TopLevelCodeDecl>(decl);
    if (!topLevelCode) continue;
    CC.setContext(topLevelCode);
    topLevelCode->getBody()->walk(CC);
  }
  assert(nextDiscriminator == TLC.NextAutoClosureDiscriminator &&
         "reentrant/concurrent invocation of contextualizeTopLevelCode?");
  TLC.NextAutoClosureDiscriminator = CC.NextDiscriminator;
}

namespace {
class StmtChecker : public StmtVisitor<StmtChecker, Stmt*> {
public:
  TypeChecker &TC;

  /// \brief This is the current function or closure being checked.
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
  CaseStmt /*nullable*/ *FallthroughDest = nullptr;

  SourceLoc EndTypeCheckLoc;

  /// Used to check for discarded expression values: in the REPL top-level
  /// expressions are not discarded.
  bool IsREPL;

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
    : TC(TC), TheFunc(AFD), DC(AFD), IsREPL(false) { }

  StmtChecker(TypeChecker &TC, ClosureExpr *TheClosure)
    : TC(TC), TheFunc(TheClosure), DC(TheClosure), IsREPL(false) { }

  StmtChecker(TypeChecker &TC, DeclContext *DC)
    : TC(TC), TheFunc(), DC(DC), IsREPL(false) {
    if (const SourceFile *SF = DC->getParentSourceFile())
      if (SF->Kind == SourceFileKind::REPL)
        IsREPL = true;
  }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//
  
  bool isInDefer() const {
    if (!TheFunc.hasValue()) return false;
    auto *CE = dyn_cast_or_null<ClosureExpr>(TheFunc->getAbstractClosureExpr());
    return CE && CE->isDeferBody();
  }
  
  template<typename StmtTy>
  bool typeCheckStmt(StmtTy *&S) {
    StmtTy *S2 = cast_or_null<StmtTy>(visit(S));
    if (S2 == 0) return true;
    S = S2;
    performStmtDiagnostics(TC, S);
    return false;
  }

  /// Type-check an entire function body.
  bool typeCheckBody(BraceStmt *&S) {
    if (typeCheckStmt(S)) return true;
    setAutoClosureDiscriminators(DC, S);
    return false;
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
    if (!ResultTy || ResultTy->is<ErrorType>())
      return nullptr;

    if (!RS->hasResult()) {
      if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context)))
        TC.diagnose(RS->getReturnLoc(), diag::return_expr_missing);
      return RS;
    }

    Expr *E = RS->getResult();
    
    if (E)
      E->setIsReturnExpr();

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
      if (ctor->getFailability() == OTK_None) {
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

    auto failed = TC.typeCheckExpression(E, DC, ResultTy, Type(), false);
    RS->setResult(E);
    
    if (failed)
      return nullptr;
    
    return RS;
  }
  
  Stmt *visitDeferStmt(DeferStmt *DS) {
    TC.typeCheckDecl(DS->getPatternBinding(), /*isFirstPass*/false);
    TC.typeCheckDecl(DS->getTempDecl(), /*isFirstPass*/false);

    Expr *theCall = DS->getCallExpr();
    if (!TC.typeCheckExpression(theCall, DC, Type(), Type(), false))
      return nullptr;
    DS->setCallExpr(theCall);
    
    return DS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    StmtCondition C = IS->getCond();
    if (TC.typeCheckCondition(C, DC)) return 0;
    IS->setCond(C);

    AddLabeledStmt ifNest(*this, IS);

    Stmt *S = IS->getThenStmt();
    if (typeCheckStmt(S)) return 0;
    IS->setThenStmt(S);

    if ((S = IS->getElseStmt())) {
      if (typeCheckStmt(S)) return 0;
      IS->setElseStmt(S);
    }
    
    return IS;
  }
  
  Stmt *visitIfConfigStmt(IfConfigStmt *ICS) {
    
    // Active members are attached to the enclosing declaration, so there's no
    // need to walk anything within.
    
    return ICS;
  }

  Stmt *visitDoStmt(DoStmt *DS) {
    AddLabeledStmt loopNest(*this, DS);
    Stmt *S = DS->getBody();
    if (typeCheckStmt(S)) return 0;
    DS->setBody(S);
    return DS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    StmtCondition C = WS->getCond();
    if (TC.typeCheckCondition(C, DC)) return 0;
    WS->setCond(C);

    AddLabeledStmt loopNest(*this, WS);
    Stmt *S = WS->getBody();
    if (typeCheckStmt(S)) return 0;
    WS->setBody(S);
    
    return WS;
  }
  Stmt *visitRepeatWhileStmt(RepeatWhileStmt *RWS) {
    {
      AddLabeledStmt loopNest(*this, RWS);
      Stmt *S = RWS->getBody();
      if (typeCheckStmt(S)) return 0;
      RWS->setBody(S);
    }
    
    Expr *E = RWS->getCond();
    if (TC.typeCheckCondition(E, DC)) return 0;
    RWS->setCond(E);
    return RWS;
  }
  Stmt *visitForStmt(ForStmt *FS) {
    // Type check any var decls in the initializer.
    for (auto D : FS->getInitializerVarDecls())
      TC.typeCheckDecl(D, /*isFirstPass*/false);

    if (auto *Initializer = FS->getInitializer().getPtrOrNull()) {
      if (TC.typeCheckExpression(Initializer, DC, Type(), Type(),
                                 /*discardedExpr=*/true))
        return 0;
      FS->setInitializer(Initializer);
    }

    if (auto *Cond = FS->getCond().getPtrOrNull()) {
      if (TC.typeCheckCondition(Cond, DC))
        return 0;
      FS->setCond(Cond);
    }

    if (auto *Increment = FS->getIncrement().getPtrOrNull()) {
      if (TC.typeCheckExpression(Increment, DC, Type(), Type(),
                                 /*discardedExpr=*/true))
        return 0;
      FS->setIncrement(Increment);
    }

    AddLabeledStmt loopNest(*this, FS);
    Stmt *S = FS->getBody();
    if (typeCheckStmt(S)) return 0;
    FS->setBody(S);
    
    return FS;
  }
  
  Stmt *visitForEachStmt(ForEachStmt *S) {
    TypeResolutionOptions options;
    options |= TR_AllowUnspecifiedTypes;
    options |= TR_AllowUnboundGenerics;
    options |= TR_InExpression;
    if (TC.typeCheckPattern(S->getPattern(), DC, options)) {
      // FIXME: Handle errors better.
      S->getPattern()->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    if (TC.typeCheckForEachBinding(DC, S))
      return nullptr;

    // Retrieve the 'Sequence' protocol.
    ProtocolDecl *sequenceProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::SequenceType);
    if (!sequenceProto) {
      return nullptr;
    }

    // Retrieve the 'Generator' protocol.
    ProtocolDecl *generatorProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::GeneratorType);
    if (!generatorProto) {
      return nullptr;
    }
    
    // If the sequence is an implicitly unwrapped optional, force it.
    Expr *sequence = S->getSequence();
    if (auto objectTy
          = sequence->getType()->getImplicitlyUnwrappedOptionalObjectType()) {
      sequence = new (TC.Context) ForceValueExpr(sequence,
                                                 sequence->getEndLoc());
      sequence->setType(objectTy);
      sequence->setImplicit();
      S->setSequence(sequence);
    }

    // Invoke generate() to get a generator from the sequence.
    Type generatorTy;
    VarDecl *generator;
    {
      Type sequenceType = sequence->getType();
      ProtocolConformance *conformance = nullptr;
      if (!TC.conformsToProtocol(sequenceType, sequenceProto, DC,
                                 ConformanceCheckFlags::InExpression,
                                 &conformance, sequence->getLoc()))
        return nullptr;
      
      if (conformance && conformance->isInvalid())
        return nullptr;

      generatorTy = TC.getWitnessType(sequenceType, sequenceProto,
                                      conformance,
                                      TC.Context.Id_Generator,
                                      diag::sequence_protocol_broken);
      
      Expr *getGenerator
        = TC.callWitness(sequence, DC, sequenceProto, conformance,
                         TC.Context.Id_generate,
                         {}, diag::sequence_protocol_broken);
      if (!getGenerator) return nullptr;
      
      // Create a local variable to capture the generator.
      std::string name;
      if (auto np = dyn_cast_or_null<NamedPattern>(S->getPattern()))
        name = "$"+np->getBoundName().str().str();
      name += "$generator";
      generator = new (TC.Context)
        VarDecl(/*static*/ false, /*IsLet*/ false, S->getInLoc(),
                TC.Context.getIdentifier(name), generatorTy, DC);
      generator->setImplicit();
      
      // Create a pattern binding to initialize the generator.
      auto genPat = new (TC.Context) NamedPattern(generator);
      genPat->setImplicit();
      auto genBinding =
          PatternBindingDecl::create(TC.Context, SourceLoc(),
                                     StaticSpellingKind::None,
                                     S->getForLoc(), genPat, getGenerator, DC);
      genBinding->setImplicit();
      S->setGenerator(genBinding);
    }
    
    // Working with generators requires Optional.
    if (TC.requireOptionalIntrinsics(S->getForLoc()))
      return nullptr;
    
    // Gather the witnesses from the Generator protocol conformance, which
    // we'll use to drive the loop.
    // FIXME: Would like to customize the diagnostic emitted in
    // conformsToProtocol().
    ProtocolConformance *genConformance = nullptr;
    if (!TC.conformsToProtocol(generatorTy, generatorProto, DC,
                               ConformanceCheckFlags::InExpression,
                               &genConformance, sequence->getLoc()))
      return nullptr;
    
    Type elementTy = TC.getWitnessType(generatorTy, generatorProto,
                                       genConformance, TC.Context.Id_Element,
                                       diag::generator_protocol_broken);
    if (!elementTy)
      return nullptr;
    
    // Compute the expression that advances the generator.
    Expr *genNext
      = TC.callWitness(TC.buildCheckedRefExpr(generator, DC, S->getInLoc(),
                                              /*implicit*/true),
                       DC, generatorProto, genConformance,
                       TC.Context.Id_next, {}, diag::generator_protocol_broken);
    if (!genNext) return nullptr;
    // Check that next() produces an Optional<T> value.
    if (genNext->getType()->getCanonicalType()->getAnyNominal()
          != TC.Context.getOptionalDecl()) {
      TC.diagnose(S->getForLoc(), diag::generator_protocol_broken);
      return nullptr;
    }

    // Convert that Optional<T> value to Optional<Element>.
    auto optPatternType = OptionalType::get(S->getPattern()->getType());
    if (!optPatternType->isEqual(genNext->getType()) &&
        TC.convertToType(genNext, optPatternType, DC)) {
      return nullptr;
    }

    S->setGeneratorNext(genNext);
    
    // Type-check the body of the loop.
    AddLabeledStmt loopNest(*this, S);
    BraceStmt *Body = S->getBody();
    if (typeCheckStmt(Body)) return nullptr;
    S->setBody(Body);
    
    return S;
  }

  Stmt *visitBreakStmt(BreakStmt *S) {
    LabeledStmt *Target = nullptr;
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
        }
      }
    }
    
    if (!Target) {
      // If we're in a defer, produce a tailored diagnostic.
      if (isInDefer()) {
        TC.diagnose(S->getLoc(), diag::jump_out_of_defer, "break");
        return nullptr;
      }
      
      auto diagid = diag::break_outside_loop;

      // If someone is using an unlabeled break inside of an 'if' statement,
      // produce a more specific error.
      if (S->getTargetName().empty() && !ActiveLabeledStmts.empty() &&
          isa<IfStmt>(ActiveLabeledStmts.back()))
        diagid = diag::unlabeled_break_outside_loop;

      TC.diagnose(S->getLoc(), diagid);
      return nullptr;
    }
    S->setTarget(Target);
    return S;
  }

  Stmt *visitContinueStmt(ContinueStmt *S) {
    LabeledStmt *Target = nullptr;
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
        }
      }
    }

    if (!Target) {
      // If we're in a defer, produce a tailored diagnostic.
      if (isInDefer()) {
        TC.diagnose(S->getLoc(), diag::jump_out_of_defer, "break");
        return nullptr;
      }

      TC.diagnose(S->getLoc(), diag::continue_outside_loop);
      return nullptr;
    }

    // Continue cannot be used to repeat switches, use fallthrough instead.
    if (!Target->isPossibleContinueTarget()) {
      TC.diagnose(S->getLoc(), diag::continue_not_in_this_stmt,
                  isa<SwitchStmt>(Target) ? "switch" : "if");
      return nullptr;
    }

    S->setTarget(Target);
    return S;
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
    if (FallthroughDest->hasBoundDecls())
      TC.diagnose(S->getLoc(), diag::fallthrough_into_case_with_var_binding);
    S->setFallthroughDest(FallthroughDest);
    return S;
  }
  
  Stmt *visitSwitchStmt(SwitchStmt *S) {
    // Type-check the subject expression.
    Expr *subjectExpr = S->getSubjectExpr();
    bool hadTypeError = false;

    if (TC.typeCheckExpression(subjectExpr, DC, Type(), Type(),
                               /*discardedExpr=*/false)) {
      hadTypeError = true;
    }
    
    subjectExpr = TC.coerceToMaterializable(subjectExpr);
    
    if (!subjectExpr)
      return nullptr;
    
    S->setSubjectExpr(subjectExpr);
    Type subjectType = subjectExpr->getType();

    // Type-check the case blocks.
    AddSwitchNest switchNest(*this);
    AddLabeledStmt labelNest(*this, S);

    for (unsigned i = 0, e = S->getCases().size(); i < e; ++i) {
      auto *caseBlock = S->getCases()[i];
      // Fallthrough transfers control to the next case block. In the
      // final case block, it is invalid.
      FallthroughDest = i+1 == e ? nullptr : S->getCases()[i+1];

      for (auto &labelItem : caseBlock->getMutableCaseLabelItems()) {
        // Resolve the pattern in the label.
        Pattern *pattern = labelItem.getPattern();
        if (auto *newPattern = TC.resolvePattern(pattern, DC)) {
          pattern = newPattern;
        } else {
          hadTypeError = true;
          continue;
        }

        // Coerce the pattern to the subject's type.
        if (TC.coercePatternToType(pattern, DC, subjectType, TR_InExpression)) {
          // If that failed, mark any variables binding pieces of the pattern
          // as invalid to silence follow-on errors.
          pattern->forEachVariable([&](VarDecl *VD) {
            VD->overwriteType(ErrorType::get(TC.Context));
            VD->setInvalid();
          });
          hadTypeError = true;
        }
        labelItem.setPattern(pattern);

        // Check the guard expression, if present.
        if (auto *guard = labelItem.getGuardExpr()) {
          if (TC.typeCheckCondition(guard, DC))
            hadTypeError = true;
          else
            labelItem.setGuardExpr(guard);
        }
      }
      
      // Type-check the body statements.
      Stmt *body = caseBlock->getBody();
      if (typeCheckStmt(body))
        hadTypeError = true;
      caseBlock->setBody(body);
    }
    
    return hadTypeError ? nullptr : S;
  }

  Stmt *visitCaseStmt(CaseStmt *S) {
    // Cases are handled in visitSwitchStmt.
    llvm_unreachable("case stmt outside of switch?!");
  }

  Stmt *visitCatchStmt(CatchStmt *S) {
    // Catches are handled in visitDoCatchStmt.
    llvm_unreachable("catch stmt outside of do-catch?!");
  }

  bool checkCatchStmt(CatchStmt *S) {
    bool hadTypeError = false;

    // Grab the standard exception type.
    Type exnType = TC.getExceptionType(DC, S->getCatchLoc());

    // Type-check the catch pattern.
    Pattern *pattern = S->getErrorPattern();
    if (Pattern *newPattern = TC.resolvePattern(pattern, DC)) {
      pattern = newPattern;

      // Coerce the pattern to the exception type.
      if (!exnType ||
          TC.coercePatternToType(pattern, DC, exnType, TR_InExpression)) {
        // If that failed, be sure to give the variables error types
        // before we type-check the guard.  (This will probably kill
        // most of the type-checking, but maybe not.)
        pattern->forEachVariable([&](VarDecl *var) {
            var->overwriteType(ErrorType::get(TC.Context));
            var->setInvalid();
          });
        hadTypeError = true;
      }

      S->setErrorPattern(pattern);
    } else {
      hadTypeError = true;
    }

    // Check the guard expression, if present.
    if (Expr *guard = S->getGuardExpr()) {
      hadTypeError |= TC.typeCheckCondition(guard, DC);
      S->setGuardExpr(guard);
    }
      
    // Type-check the clause body.
    Stmt *body = S->getBody();
    hadTypeError |= typeCheckStmt(body);
    S->setBody(body);

    return hadTypeError;
  }

  Stmt *visitDoCatchStmt(DoCatchStmt *S) {
    // The labels are in scope for both the 'do' and all of the catch
    // clauses.  This allows the user to break out of (or restart) the
    // entire construct.
    AddLabeledStmt loopNest(*this, S);

    bool hadTypeError = false;

    // Type-check the 'do' body.  Type failures in here will generally
    // not cause type failures in the 'catch' clauses.
    Stmt *newBody = S->getBody();
    if (typeCheckStmt(newBody)) {
      hadTypeError = true;
    } else {
      S->setBody(newBody);
    }

    // Check all the catch clauses independently.
    for (auto clause : S->getCatches()) {
      hadTypeError |= checkCatchStmt(clause);
    }
    
    return hadTypeError ? nullptr : S;
  }

  Stmt *visitFailStmt(FailStmt *S) {
    // These are created as part of type-checking "return" in an initializer.
    // There is nothing more to do.
    return S;
  }
};
  
} // end anonymous namespace
  
/// Check an expression whose result is not being used at all.
static void diagnoseIgnoredExpr(TypeChecker &TC, Expr *E) {
  // Complain about l-values that are neither loaded nor stored.
  if (E->getType()->isLValueType()) {
    TC.diagnose(E->getLoc(), diag::expression_unused_lvalue)
      .highlight(E->getSourceRange());
    return;
  }

  // Complain about functions that aren't called.
  // TODO: What about tuples which contain functions by-value that are
  // dead?
  if (E->getType()->is<AnyFunctionType>()) {
    TC.diagnose(E->getLoc(), diag::expression_unused_function)
      .highlight(E->getSourceRange());
    return;
  }

  // FIXME: Complain about literals
}

Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  const SourceManager &SM = TC.Context.SourceMgr;
  for (auto &elem : BS->getElements()) {
    if (Expr *SubExpr = elem.dyn_cast<Expr*>()) {
      SourceLoc Loc = SubExpr->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      // Type check the expression.
      bool isDiscarded = !(IsREPL && isa<TopLevelCodeDecl>(DC))
        && !TC.Context.LangOpts.Playground
        && !TC.Context.LangOpts.DebuggerSupport;
      if (TC.typeCheckExpression(SubExpr, DC, Type(), Type(), isDiscarded)) {
        elem = SubExpr;
        continue;
      }
      
      if (isDiscarded)
        diagnoseIgnoredExpr(TC, SubExpr);
      
      elem = SubExpr;
      continue;
    }

    if (Stmt *SubStmt = elem.dyn_cast<Stmt*>()) {
      SourceLoc Loc = SubStmt->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      if (!typeCheckStmt(SubStmt))
        elem = SubStmt;
      continue;
    }

    Decl *SubDecl = elem.get<Decl *>();
    SourceLoc Loc = SubDecl->getStartLoc();
    if (EndTypeCheckLoc.isValid() &&
        (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
      break;

    TC.typeCheckDecl(SubDecl, /*isFirstPass*/false);

    // Make sure to type check the 'else' in a conditional PatternBinding.
    if (auto *PBD = dyn_cast<PatternBindingDecl>(SubDecl))
      if (auto *Else = PBD->getElse().getExplicitBody())
        typeCheckStmt(Else);
  }
  
  return BS;
}

/// Check the default arguments that occur within this pattern.
static void checkDefaultArguments(TypeChecker &tc, Pattern *pattern,
                                  unsigned &nextArgIndex,
                                  DeclContext *dc) {
  assert(dc->isLocalContext());

  switch (pattern->getKind()) {
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(pattern)->getElements()) {
      unsigned curArgIndex = nextArgIndex++;
      if (field.getInit() &&
          field.getPattern()->hasType() &&
          !field.getPattern()->getType()->is<ErrorType>()) {

        Expr *e = field.getInit()->getExpr();

        // Re-use an existing initializer context if possible.
        auto existingContext = e->findExistingInitializerContext();
        DefaultArgumentInitializer *initContext;
        if (existingContext) {
          initContext = cast<DefaultArgumentInitializer>(existingContext);
          assert(initContext->getIndex() == curArgIndex);
          assert(initContext->getParent() == dc);

        // Otherwise, allocate one temporarily.
        } else {
          initContext =
            tc.Context.createDefaultArgumentContext(dc, curArgIndex);
        }

        // Type-check the initializer, then flag that we did so.
        if (tc.typeCheckExpression(e, initContext,
                                   field.getPattern()->getType(), Type(),
                                   /*discardedExpr=*/false))
          field.getInit()->setExpr(field.getInit()->getExpr(), true);
        else
          field.getInit()->setExpr(e, true);

        tc.checkInitializerErrorHandling(initContext, e);

        // Walk the checked initializer and contextualize any closures
        // we saw there.
        bool hasClosures = tc.contextualizeInitializer(initContext, e);

        // If we created a new context and didn't run into any autoclosures
        // during the walk, give the context back to the ASTContext.
        if (!hasClosures && !existingContext)
          tc.Context.destroyDefaultArgumentContext(initContext);
      }
    }
    return;
  case PatternKind::Paren:
    return checkDefaultArguments(tc,
                                 cast<ParenPattern>(pattern)->getSubPattern(),
                                 nextArgIndex,
                                 dc);
  case PatternKind::Var:
    return checkDefaultArguments(tc, cast<VarPattern>(pattern)->getSubPattern(),
                                 nextArgIndex,
                                 dc);
  case PatternKind::Typed:
  case PatternKind::Named:
  case PatternKind::Any:
    return;

#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("pattern can't appear in argument list!");
  }
  llvm_unreachable("bad pattern kind!");
}

bool TypeChecker::typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                                     SourceLoc EndTypeCheckLoc) {
  if (auto *FD = dyn_cast<FuncDecl>(AFD))
    return typeCheckFunctionBodyUntil(FD, EndTypeCheckLoc);

  if (auto *CD = dyn_cast<ConstructorDecl>(AFD))
    return typeCheckConstructorBodyUntil(CD, EndTypeCheckLoc);

  auto *DD = cast<DestructorDecl>(AFD);
  return typeCheckDestructorBodyUntil(DD, EndTypeCheckLoc);
}

bool TypeChecker::typeCheckAbstractFunctionBody(AbstractFunctionDecl *AFD) {
  if (!AFD->getBody())
    return false;

  Optional<FunctionBodyTimer> timer;
  if (DebugTimeFunctionBodies)
    timer.emplace(AFD);

  return typeCheckAbstractFunctionBodyUntil(AFD, SourceLoc());
}

// Type check a function body (defined with the func keyword) that is either a
// named function or an anonymous func expression.
bool TypeChecker::typeCheckFunctionBodyUntil(FuncDecl *FD,
                                             SourceLoc EndTypeCheckLoc) {
  // Check the default argument definitions.
  unsigned nextArgIndex = 0;
  for (auto pattern : FD->getBodyParamPatterns()) {
    checkDefaultArguments(*this, pattern, nextArgIndex, FD);
  }

  // Clang imported inline functions do not have a Swift body to
  // typecheck.
  if (FD->getClangDecl())
    return false;

  BraceStmt *BS = FD->getBody();
  assert(BS && "Should have a body");

  StmtChecker SC(*this, static_cast<AbstractFunctionDecl *>(FD));
  SC.EndTypeCheckLoc = EndTypeCheckLoc;
  bool HadError = SC.typeCheckBody(BS);

  FD->setBody(BS);
  return HadError;
}

Expr* TypeChecker::constructCallToSuperInit(ConstructorDecl *ctor,
                                            ClassDecl *ClDecl) {
  Expr *superRef = new (Context) SuperRefExpr(ctor->getImplicitSelfDecl(),
                                              SourceLoc(), /*Implicit=*/true);
  Expr *r = new (Context) UnresolvedConstructorExpr(superRef,
                                                    SourceLoc(),
                                                         SourceLoc(),
                                                         /*Implicit=*/true);
  Expr *args = TupleExpr::createEmpty(Context, SourceLoc(), SourceLoc(),
                                      /*Implicit=*/true);
  r = new (Context) CallExpr(r, args, /*Implicit=*/true);
  r = new (Context) RebindSelfInConstructorExpr(r, ctor->getImplicitSelfDecl());

  /// Expression type checking listener for the generated call to super ensures
  /// that we suppress diagnostics.
  class NoDiagnosticsListener : public ExprTypeCheckListener {
  public:
    virtual bool suppressDiagnostics() const { return true; }
  } listener;

  if (!typeCheckExpression(r, ctor, Type(), Type(), /*discardedExpr=*/true,
                           FreeTypeVariableBinding::Disallow, &listener))
    return r;

  return 0;
}

/// Check a super.init call.
///
/// \returns true if an error occurred.
static bool checkSuperInit(TypeChecker &tc, ConstructorDecl *fromCtor, 
                           ApplyExpr *apply, bool implicitlyGenerated) {
  // Make sure we are referring to a designated initializer.
  auto otherCtorRef = dyn_cast<OtherConstructorDeclRefExpr>(
                        apply->getFn()->getSemanticsProvidingExpr());
  if (!otherCtorRef)
    return false;
  
  auto ctor = otherCtorRef->getDecl();
  if (!ctor->isDesignatedInit()) {
    if (!implicitlyGenerated) {
      auto contextTy = fromCtor->getDeclContext()->getDeclaredTypeInContext();
      if (auto classTy = contextTy->getClassOrBoundGenericClass()) {
        assert(classTy->getSuperclass());
        tc.diagnose(apply->getArg()->getLoc(), diag::chain_convenience_init,
                    classTy->getSuperclass());
        tc.diagnose(ctor, diag::convenience_init_here);
      }
    }
    return true;
  }
  
  // For an implicitly generated super.init() call, make sure there's
  // only one designated initializer.
  if (implicitlyGenerated) {
    auto superclassTy = ctor->getExtensionType();
    for (auto member : tc.lookupConstructors(superclassTy, fromCtor,
                                             /*knownPrivate=*/true)) {
      auto superclassCtor = dyn_cast<ConstructorDecl>(member);
      if (!superclassCtor || !superclassCtor->isDesignatedInit() ||
          superclassCtor == ctor)
        continue;
      
      // Found another designated initializer in the superclass. Don't add the
      // super.init() call.
      return true;
    }
  }
  
  return false;
}

static bool isKnownEndOfConstructor(ASTNode N) {
  auto *S = N.dyn_cast<Stmt*>();
  if (!S) return false;

  return isa<ReturnStmt>(S) || isa<FailStmt>(S);
}

bool TypeChecker::typeCheckConstructorBodyUntil(ConstructorDecl *ctor,
                                                SourceLoc EndTypeCheckLoc) {
  // Check the default argument definitions.
  unsigned nextArgIndex = 0;
  for (auto pattern : ctor->getBodyParamPatterns())
    checkDefaultArguments(*this, pattern, nextArgIndex, ctor);

  BraceStmt *body = ctor->getBody();
  if (!body)
    return true;

  // For constructors, we make sure that the body ends with a "return" stmt,
  // which we either implicitly synthesize, or the user can write.  This
  // simplifies SILGen.
  if (body->getNumElements() == 0 ||
      !isKnownEndOfConstructor(body->getElements().back())) {
    SmallVector<ASTNode, 8> Elts(body->getElements().begin(),
                                 body->getElements().end());
    Elts.push_back(new (Context) ReturnStmt(SourceLoc(), /*value*/nullptr,
                                            /*implicit*/true));
    body = BraceStmt::create(Context, body->getLBraceLoc(), Elts,
                             body->getRBraceLoc(), body->isImplicit());
    ctor->setBody(body);
  }
  
  // Type-check the body.
  StmtChecker SC(*this, static_cast<AbstractFunctionDecl *>(ctor));
  SC.EndTypeCheckLoc = EndTypeCheckLoc;
  bool HadError = SC.typeCheckBody(body);

  if (ctor->isInvalid())
    return HadError;

  // Determine whether we need to introduce a super.init call.
  auto nominalDecl = ctor->getDeclContext()->getDeclaredTypeInContext()
    ->getNominalOrBoundGenericNominal();
  ClassDecl *ClassD = dyn_cast<ClassDecl>(nominalDecl);
  bool wantSuperInitCall = false;
  if (ClassD) {
    bool isDelegating = false;
    ApplyExpr *initExpr = nullptr;
    switch (ctor->getDelegatingOrChainedInitKind(&Diags, &initExpr)) {
    case ConstructorDecl::BodyInitKind::Delegating:
      isDelegating = true;
      wantSuperInitCall = false;
      break;

    case ConstructorDecl::BodyInitKind::Chained:
      checkSuperInit(*this, ctor, initExpr, false);

      /// A convenience initializer cannot chain to a superclass constructor.
      if (ctor->isConvenienceInit()) {
        diagnose(initExpr->getLoc(), diag::delegating_convenience_super_init,
                 ctor->getDeclContext()->getDeclaredTypeOfContext());
      }

      SWIFT_FALLTHROUGH;

    case ConstructorDecl::BodyInitKind::None:
      wantSuperInitCall = false;
      break;

    case ConstructorDecl::BodyInitKind::ImplicitChained:
      wantSuperInitCall = true;
      break;
    }

    // A class designated initializer must never be delegating.
    if (ctor->isDesignatedInit() && ClassD && isDelegating) {
      diagnose(ctor->getLoc(),
               diag::delegating_designated_init,
               ctor->getDeclContext()->getDeclaredTypeOfContext())
        .fixItInsert(ctor->getLoc(), "convenience ");
      diagnose(initExpr->getLoc(), diag::delegation_here);
      ctor->setInitKind(CtorInitializerKind::Convenience);
    }
  }

  // If we want a super.init call...
  if (wantSuperInitCall) {
    // Find a default initializer in the superclass.
    if (Expr *SuperInitCall = constructCallToSuperInit(ctor, ClassD)) {
      // If the initializer we found is a designated initializer, we're okay.
      class FindOtherConstructorRef : public ASTWalker {
      public:
        ApplyExpr *Found = nullptr;

        std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
          if (auto apply = dyn_cast<ApplyExpr>(E)) {
            if (isa<OtherConstructorDeclRefExpr>(
                  apply->getFn()->getSemanticsProvidingExpr())) {
              Found = apply;
              return { false, E };
            }
          }

          return { Found == nullptr, E };
        }
      };

      FindOtherConstructorRef finder;
      SuperInitCall->walk(finder);
      if (!checkSuperInit(*this, ctor, finder.Found, true)) {
        // Store the super.init expression within the constructor declaration
        // to be emitted during SILGen.
        ctor->setSuperInitCall(SuperInitCall);
      }
    }
  }

  return HadError;
}

bool TypeChecker::typeCheckDestructorBodyUntil(DestructorDecl *DD,
                                               SourceLoc EndTypeCheckLoc) {
  StmtChecker SC(*this, static_cast<AbstractFunctionDecl *>(DD));
  SC.EndTypeCheckLoc = EndTypeCheckLoc;
  BraceStmt *Body = DD->getBody();
  if (!Body)
    return false;

  bool HadError = SC.typeCheckBody(Body);

  DD->setBody(Body);
  return HadError;
}

void TypeChecker::typeCheckClosureBody(ClosureExpr *closure) {
  BraceStmt *body = closure->getBody();

  Optional<FunctionBodyTimer> timer;
  if (DebugTimeFunctionBodies)
    timer.emplace(closure);

  StmtChecker(*this, closure).typeCheckBody(body);
  if (body) {
    closure->setBody(body, closure->hasSingleExpressionBody());
  }
}

void TypeChecker::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  // We intentionally use typeCheckStmt instead of typeCheckBody here
  // because we want to contextualize all the TopLevelCode
  // declarations simultaneously.
  BraceStmt *Body = TLCD->getBody();
  StmtChecker(*this, TLCD).typeCheckStmt(Body);
  TLCD->setBody(Body);
  checkTopLevelErrorHandling(TLCD);
}
