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
#include "swift/Basic/Optional.h"
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
        if (CE->getParent() != ParentDC) {
          // If a closure is nested within an auto closure, we'll need to update
          // its parent to the auto closure parent.
          if (ParentDC->getContextKind() ==
                DeclContextKind::AbstractClosureExpr) {
            CE->setParent(ParentDC);
          } else {
            llvm_unreachable("Incorrect parent decl context for closure");
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
/// StmtChecker - This class implements 
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
  unsigned LoopNestLevel = 0;
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

  struct AddLoopNest {
    StmtChecker &SC;
    AddLoopNest(StmtChecker &SC) : SC(SC) {
      ++SC.LoopNestLevel;
    }
    ~AddLoopNest() {
      --SC.LoopNestLevel;
    }
  };
  
  struct AddSwitchNest {
    StmtChecker &SC;
    CaseStmt *OuterFallthroughDest;
    AddSwitchNest(StmtChecker &SC)
      : SC(SC),
        OuterFallthroughDest(SC.FallthroughDest)
    {
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
      return 0;
    }

    Type ResultTy = TheFunc->getBodyResultType();
    if (!ResultTy || ResultTy->is<ErrorType>())
      return 0;

    if (!RS->hasResult()) {
      if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context)))
        TC.diagnose(RS->getReturnLoc(), diag::return_expr_missing);
      return RS;
    }

    Expr *E = RS->getResult();
    if (TC.typeCheckExpression(E, DC, ResultTy, /*discardedExpr=*/false))
      return 0;
    RS->setResult(E);

    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    StmtCondition C = IS->getCond();
    if (TC.typeCheckCondition(C, DC)) return 0;
    IS->setCond(C);

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
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    StmtCondition C = WS->getCond();
    if (TC.typeCheckCondition(C, DC)) return 0;
    WS->setCond(C);

    AddLoopNest loopNest(*this);
    Stmt *S = WS->getBody();
    if (typeCheckStmt(S)) return 0;
    WS->setBody(S);
    
    return WS;
  }
  Stmt *visitDoWhileStmt(DoWhileStmt *WS) {
    {
      AddLoopNest loopNest(*this);
      Stmt *S = WS->getBody();
      if (typeCheckStmt(S)) return 0;
      WS->setBody(S);
    }
    
    Expr *E = WS->getCond();
    if (TC.typeCheckCondition(E, DC)) return 0;
    WS->setCond(E);
    return WS;
  }
  Stmt *visitForStmt(ForStmt *FS) {
    // Type check any var decls in the initializer.
    for (auto D : FS->getInitializerVarDecls())
      TC.typeCheckDecl(D, /*isFirstPass*/false);

    if (auto *Initializer = FS->getInitializer().getPtrOrNull()) {
      if (TC.typeCheckExpression(Initializer, DC, Type(), /*discardedExpr=*/true))
        return 0;
      FS->setInitializer(Initializer);
    }

    if (auto *Cond = FS->getCond().getPtrOrNull()) {
      if (TC.typeCheckCondition(Cond, DC))
        return 0;
      FS->setCond(Cond);
    }

    if (auto *Increment = FS->getIncrement().getPtrOrNull()) {
      if (TC.typeCheckExpression(Increment, DC, Type(), /*discardedExpr=*/true))
        return 0;
      FS->setIncrement(Increment);
    }

    AddLoopNest loopNest(*this);
    Stmt *S = FS->getBody();
    if (typeCheckStmt(S)) return 0;
    FS->setBody(S);
    
    return FS;
  }
  
  Stmt *visitForEachStmt(ForEachStmt *S) {
    // Type-check the container and convert it to an rvalue.
    Expr *Sequence = S->getSequence();
    if (TC.typeCheckExpression(Sequence, DC, Type(), /*discardedExpr=*/false))
      return nullptr;
    S->setSequence(Sequence);

    // Retrieve the 'Sequence' protocol.
    ProtocolDecl *SequenceProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::Sequence);
    if (!SequenceProto) {
      return nullptr;
    }

    // Retrieve the 'Generator' protocol.
    ProtocolDecl *GeneratorProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::Generator);
    if (!GeneratorProto) {
      return nullptr;
    }
    
    // Verify that the container conforms to the Sequence protocol, and
    // invoke getElements() on it container to retrieve the range of elements.
    Type GeneratorTy;
    VarDecl *Generator;
    {
      Type SequenceType = Sequence->getType()->getRValueType();

      ProtocolConformance *Conformance = nullptr;
      if (!TC.conformsToProtocol(SequenceType, SequenceProto, DC,
                                 &Conformance, Sequence->getLoc()))
        return nullptr;
      
      if (Conformance && Conformance->isInvalid())
        return nullptr;

      GeneratorTy = TC.getWitnessType(SequenceType, SequenceProto,
                                      Conformance,
                                      TC.Context.getIdentifier("GeneratorType"),
                                      diag::sequence_protocol_broken);
      
      Expr *GetGenerator
        = TC.callWitness(Sequence, DC, SequenceProto, Conformance,
                         TC.Context.getIdentifier("generate"),
                         {}, diag::sequence_protocol_broken);
      if (!GetGenerator) return nullptr;
      
      // Create a local variable to capture the generator.
      Generator = new (TC.Context) VarDecl(/*static*/ false, /*IsLet*/ false,
                                           S->getInLoc(),
                                     TC.Context.getIdentifier("$generator"),
                                     GeneratorTy, DC);
      Generator->setImplicit();
      
      // Create a pattern binding to initialize the generator.
      auto GenPat = new (TC.Context) NamedPattern(Generator);
      GenPat->setImplicit();
      auto GenBinding = new (TC.Context)
          PatternBindingDecl(SourceLoc(), StaticSpellingKind::None,
                             S->getForLoc(), GenPat, GetGenerator,
                             /*conditional*/ false, DC);
      GenBinding->setImplicit();
      S->setGenerator(GenBinding);
    }
    
    // Working with generators requires Optional.
    if (TC.requireOptionalIntrinsics(S->getForLoc()))
      return nullptr;
    
    // Gather the witnesses from the Generator protocol conformance, which
    // we'll use to drive the loop.
    
    // FIXME: Would like to customize the diagnostic emitted in
    // conformsToProtocol().
    ProtocolConformance *GenConformance = nullptr;
    if (!TC.conformsToProtocol(GeneratorTy, GeneratorProto, DC, &GenConformance,
                               Sequence->getLoc()))
      return nullptr;
    
    Type ElementTy = TC.getWitnessType(GeneratorTy, GeneratorProto,
                                       GenConformance,
                                       TC.Context.getIdentifier("Element"),
                                       diag::generator_protocol_broken);
    if (!ElementTy)
      return nullptr;
    
    // Compute the expression that advances the generator.
    Expr *GenNext
      = TC.callWitness(TC.buildCheckedRefExpr(Generator, DC, S->getInLoc(),
                                              /*implicit*/true),
                       DC, GeneratorProto, GenConformance,
                       TC.Context.getIdentifier("next"),
                       {}, diag::generator_protocol_broken);
    if (!GenNext) return nullptr;
    // Check that next() produces an Optional<Element> value.
    if (GenNext->getType()->getCanonicalType()->getAnyNominal()
          != TC.Context.getOptionalDecl()) {
      TC.diagnose(S->getForLoc(), diag::generator_protocol_broken);
      return nullptr;
    }
    S->setGeneratorNext(GenNext);
    
    // Coerce the pattern to the element type, now that we know the element
    // type.
    // FIXME: Could allow unbound generic types in this pattern, then override
    // here.
    Pattern *pattern = S->getPattern();
    if (TC.coercePatternToType(pattern, DC, ElementTy, None))
      return nullptr;
    S->setPattern(pattern);
    
    // Type-check the body of the loop.
    AddLoopNest loopNest(*this);
    BraceStmt *Body = S->getBody();
    if (typeCheckStmt(Body)) return nullptr;
    S->setBody(Body);
    
    return S;
  }

  Stmt *visitBreakStmt(BreakStmt *S) {
    if (!LoopNestLevel) {
      TC.diagnose(S->getLoc(), diag::break_outside_loop);
      return nullptr;
    }
    return S;
  }

  Stmt *visitContinueStmt(ContinueStmt *S) {
    if (!LoopNestLevel) {
      TC.diagnose(S->getLoc(), diag::continue_outside_loop);
      return nullptr;
    }
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
    if (TC.typeCheckExpression(subjectExpr, DC, Type(),
                               /*discardedExpr=*/false))
      return nullptr;
    subjectExpr = TC.coerceToMaterializable(subjectExpr);
    if (!subjectExpr)
      return nullptr;
    S->setSubjectExpr(subjectExpr);
    Type subjectType = subjectExpr->getType();

    // Type-check the case blocks.
    AddSwitchNest switchNest(*this);
    bool hadTypeError = false;
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
        if (TC.coercePatternToType(pattern, DC, subjectType, None)) {
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
};
  
} // end anonymous namespace
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  const SourceManager &SM = TC.Context.SourceMgr;
  for (auto &elem : BS->getElements()) {
    if (Expr *SubExpr = elem.dyn_cast<Expr*>()) {
      SourceLoc Loc = SubExpr->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      // Type check the expression.
      bool isDiscarded = !(IsREPL && isa<TopLevelCodeDecl>(DC));
      if (TC.typeCheckExpression(SubExpr, DC, Type(), isDiscarded)) {
        elem = SubExpr;
        continue;
      }
      
      if (isDiscarded)
        TC.typeCheckIgnoredExpr(SubExpr);
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
  }
  
  return BS;
}

/// Check an expression whose result is not being used at all.
void TypeChecker::typeCheckIgnoredExpr(Expr *E) {
  // Complain about l-values that are neither loaded nor stored.
  if (E->getType()->is<LValueType>()) {
    diagnose(E->getLoc(), diag::expression_unused_lvalue)
      .highlight(E->getSourceRange());
    return;
  }

  // Complain about functions that aren't called.
  // TODO: What about tuples which contain functions by-value that are
  // dead?
  if (E->getType()->is<AnyFunctionType>()) {
    diagnose(E->getLoc(), diag::expression_unused_function)
      .highlight(E->getSourceRange());
    return;
  }

  // FIXME: Complain about literals
}

/// Check the default arguments that occur within this pattern.
static void checkDefaultArguments(TypeChecker &tc, Pattern *pattern,
                                  unsigned &nextArgIndex,
                                  DeclContext *dc) {
  assert(dc->isLocalContext());

  switch (pattern->getKind()) {
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(pattern)->getFields()) {
      if (field.getPattern()->hasType() &&
          field.getPattern()->getType()->is<ErrorType>())
        continue;
      
      unsigned curArgIndex = nextArgIndex++;
      if (field.getInit()) {
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
                                   field.getPattern()->getType(),
                                   /*discardedExpr=*/false))
          field.getInit()->setExpr(field.getInit()->getExpr(), true);
        else
          field.getInit()->setExpr(e, true);

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
  Expr *args = new (Context) TupleExpr(SourceLoc(), SourceLoc(),
                                       /*Implicit=*/true);
  r = new (Context) CallExpr(r, args, /*Implicit=*/true);
  r = new (Context) RebindSelfInConstructorExpr(r, ctor->getImplicitSelfDecl());

  /// Expression type checking listener for the generated call to super ensures
  /// that we suppress diagnostics.
  class NoDiagnosticsListener : public ExprTypeCheckListener {
  public:
    virtual bool suppressDiagnostics() const { return true; }
  } listener;

  if (!typeCheckExpression(r, ctor, Type(), /*discardedExpr=*/true,
                           FreeTypeVariableBinding::Disallow, &listener))
    return r;

  return 0;
}

/// Check a super.init call.
///
/// \returns true if an error occurred.
static bool checkSuperInit(TypeChecker &tc, DeclContext *dc, ApplyExpr *apply,
                           bool implicitlyGenerated) {
  // Make sure we are referring to a designated initializer.
  auto otherCtorRef = cast<OtherConstructorDeclRefExpr>(
                        apply->getFn()->getSemanticsProvidingExpr());
  auto ctor = otherCtorRef->getDecl();
  if (!ctor->isSubobjectInit()) {
    if (!implicitlyGenerated) {
      tc.diagnose(apply->getArg()->getLoc(), diag::chain_complete_object_init,
                  apply->getArg()->getType());
      tc.diagnose(ctor, diag::complete_object_init_here);
    }
    return true;
  }
  
  // For an implicitly generated super.init() call, make sure there's
  // only one designated initializer.
  if (implicitlyGenerated) {
    auto superclassTy = ctor->getExtensionType();
    for (auto member : tc.lookupConstructors(superclassTy, dc)) {
      auto superclassCtor = dyn_cast<ConstructorDecl>(member);
      if (!superclassCtor || superclassCtor->isCompleteObjectInit() ||
          superclassCtor == ctor)
        continue;
      
      // Found another designated initializer in the superclass. Don't add the
      // super.init() call.
      return true;
    }
  }
  
  return false;
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
      SWIFT_FALLTHROUGH;

    case ConstructorDecl::BodyInitKind::None:
      wantSuperInitCall = false;
      break;

    case ConstructorDecl::BodyInitKind::ImplicitChained:
      wantSuperInitCall = true;
      break;
    }

    /// A complete object initializer must always be delegating.
    if (ctor->isCompleteObjectInit() && !isDelegating) {
      diagnose(initExpr? initExpr->getLoc() : ctor->getLoc(),
               diag::non_delegating_complete_object_init,
               ctor->getDeclContext()->getDeclaredTypeOfContext());
    }

    // A class subobject initializer must never be delegating.
    if (ctor->isSubobjectInit() && ClassD && isDelegating) {
      SourceLoc fixItLoc = ctor->getBodyParamPatterns().back()->getEndLoc();
      fixItLoc = Lexer::getLocForEndOfToken(Context.SourceMgr, fixItLoc);
      diagnose(ctor->getLoc(),
               diag::delegating_subobject_init,
               ctor->getDeclContext()->getDeclaredTypeOfContext())
        .fixItInsert(fixItLoc, " -> Self"); 
      diagnose(initExpr->getLoc(), diag::delegation_here);
      ctor->setCompleteObjectInit(true);
    }
  }

  // If we want a super.init call...
  if (wantSuperInitCall) {
    // Find a default initializer in the superclass.
    if (Expr *SuperInitCall = constructCallToSuperInit(ctor, ClassD)) {
      // If the initializer we found is a subobject initializer, we're okay.
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
}
