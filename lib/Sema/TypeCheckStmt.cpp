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

#include "swift/Subsystems.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/Interleave.h"
#include "swift/Basic/Range.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"

using namespace swift;

namespace {
/// StmtChecker - This class implements 
class StmtChecker : public StmtVisitor<StmtChecker, Stmt*> {
public:
  TypeChecker &TC;
  
  // FIXME: ConstructorDecls and DestructorDecls ought to represent their bodies
  // as normal FuncExprs.
  
  /// TheFunc - This is the current FuncExpr, ConstructorDecl, or DestructorDecl
  /// being checked.  This is null for top level code.
  FuncExprLike TheFunc;
  
  /// DC - This is the current DeclContext.
  DeclContext *DC;

  unsigned LoopNestLevel, SwitchLevel;
  
  CaseStmt /*nullable*/ *FallthroughDest;

  struct AddLoopNest {
    StmtChecker &SC;
    AddLoopNest(StmtChecker &SC) : SC(SC) {
      ++SC.LoopNestLevel;
    }
    ~AddLoopNest() {
      --SC.LoopNestLevel;
    }
  };

  StmtChecker(TypeChecker &TC, FuncExpr *TheFunc)
    : TC(TC), TheFunc(TheFunc), DC(TheFunc),
      LoopNestLevel(0), SwitchLevel(0), FallthroughDest(nullptr) { }

  StmtChecker(TypeChecker &TC, PipeClosureExpr *TheClosure)
    : TC(TC), TheFunc(TheClosure), DC(TheClosure),
      LoopNestLevel(0), SwitchLevel(0), FallthroughDest(nullptr) { }

  StmtChecker(TypeChecker &TC, ConstructorDecl *TheCtor)
    : TC(TC), TheFunc(TheCtor), DC(TheCtor),
      LoopNestLevel(0), SwitchLevel(0), FallthroughDest(nullptr) { }
  
  StmtChecker(TypeChecker &TC, DestructorDecl *TheDtor)
    : TC(TC), TheFunc(TheDtor), DC(TheDtor),
      LoopNestLevel(0), SwitchLevel(0), FallthroughDest(nullptr) { }
  
  StmtChecker(TypeChecker &TC, DeclContext *DC)
    : TC(TC), TheFunc(), DC(DC),
      LoopNestLevel(0), SwitchLevel(0), FallthroughDest(nullptr) { }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//
  
  bool typeCheckExpr(Expr *&E, Type DestTy = Type()) {
    return TC.typeCheckExpression(E, DC, DestTy);
  }

  template<typename StmtTy>
  bool typeCheckStmt(StmtTy *&S) {
    StmtTy *S2 = cast_or_null<StmtTy>(visit(S));
    if (S2 == 0) return true;
    S = S2;
    return false;
  }
  
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  Stmt *visitBraceStmt(BraceStmt *BS);
  
  Type returnTypeOfFunc() {
    if (auto fe = TheFunc.dyn_cast<FuncExpr*>()) {
      Type resultTy = fe->getType();
      if (resultTy->is<ErrorType>())
        return resultTy;
      for (unsigned i = 0, e = fe->getNumParamPatterns();
           i != e;
           ++i)
        resultTy = resultTy->castTo<AnyFunctionType>()->getResult();
      return resultTy;
    } else if (auto closure = TheFunc.dyn_cast<PipeClosureExpr *>()) {
      return closure->getResultType();
    } else
      return TupleType::getEmpty(TC.Context);
  }
  
  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (TheFunc.isNull()) {
      TC.diagnose(RS->getReturnLoc(), diag::return_invalid_outside_func);
      return 0;
    }

    Type ResultTy = returnTypeOfFunc();
    if (ResultTy->is<ErrorType>())
      return 0;

    if (!RS->hasResult()) {
      if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context)))
        TC.diagnose(RS->getReturnLoc(), diag::return_expr_missing);
      return RS;
    }

    Expr *E = RS->getResult();
    if (typeCheckExpr(E, ResultTy))
      return 0;
    RS->setResult(E);

    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    Expr *E = IS->getCond();
    if (TC.typeCheckCondition(E, DC)) return 0;
    IS->setCond(E);

    Stmt *S = IS->getThenStmt();
    if (typeCheckStmt(S)) return 0;
    IS->setThenStmt(S);

    if ((S = IS->getElseStmt())) {
      if (typeCheckStmt(S)) return 0;
      IS->setElseStmt(S);
    }
    
    return IS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    Expr *E = WS->getCond();
    if (TC.typeCheckCondition(E, DC)) return 0;
    WS->setCond(E);

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

    Expr *Tmp = FS->getInitializer();
    if (Tmp) {
      if (typeCheckExpr(Tmp)) return 0;
      FS->setInitializer(Tmp);
    }
    
    // Type check the condition if present.
    if (FS->getCond().isNonNull()) {
      Expr *E = FS->getCond().get();
      if (TC.typeCheckCondition(E, DC)) return 0;
      FS->setCond(E);
    }
    
    Tmp = FS->getIncrement();
    if (Tmp) {
      if (typeCheckExpr(Tmp)) return 0;
      FS->setIncrement(Tmp);
    }

    AddLoopNest loopNest(*this);
    Stmt *S = FS->getBody();
    if (typeCheckStmt(S)) return 0;
    FS->setBody(S);
    
    return FS;
  }
  
  Stmt *visitForEachStmt(ForEachStmt *S) {
    // Type-check the container and convert it to an rvalue.
    Expr *Container = S->getContainer();
    if (TC.typeCheckExpression(Container, DC)) return nullptr;
    S->setContainer(Container);

    // Retrieve the 'Enumerable' protocol.
    ProtocolDecl *EnumerableProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::Enumerable);
    if (!EnumerableProto) {
      return nullptr;
    }

    // Retrieve the 'Enumerator' protocol.
    ProtocolDecl *EnumeratorProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::Enumerator);
    if (!EnumeratorProto) {
      return nullptr;
    }
    
    // Verify that the container conforms to the Enumerable protocol, and
    // invoke getElements() on it container to retrieve the range of elements.
    Type RangeTy;
    VarDecl *Range;
    {
      Type ContainerType = Container->getType()->getRValueType();

      ProtocolConformance *Conformance = nullptr;
      if (!TC.conformsToProtocol(ContainerType, EnumerableProto, &Conformance,
                                 Container->getLoc()))
        return nullptr;

      for (auto Member : EnumerableProto->getMembers()) {
        auto Value = dyn_cast<ValueDecl>(Member);
        if (!Value)
          continue;
        
        StringRef Name = Value->getName().str();
        if (Name.equals("EnumeratorType") && isa<TypeDecl>(Value)) {
          if (Conformance) {
            ArchetypeType *Archetype
              = cast<TypeDecl>(Value)->getDeclaredType()->getAs<ArchetypeType>();
            RangeTy = Conformance->TypeMapping[Archetype];
          } else {
            RangeTy = cast<TypeDecl>(Value)->getDeclaredType();
          }
          RangeTy = TC.substMemberTypeWithBase(RangeTy, Value, ContainerType);
        }
      }

      if (!RangeTy) {
        TC.diagnose(EnumerableProto->getLoc(), diag::enumerable_protocol_broken);
        return nullptr;
      }

      Expr *GetElements
        = TC.callWitness(Container, DC, EnumerableProto, Conformance,
                         TC.Context.getIdentifier("getEnumeratorType"),
                         { },
                         diag::enumerable_protocol_broken);
      if (!GetElements) return nullptr;
      
      // Create a local variable to capture the range.
      // FIXME: Mark declaration as implicit?
      Range = new (TC.Context) VarDecl(S->getInLoc(),
                                       TC.Context.getIdentifier("__range"),
                                       RangeTy, DC);
      
      // Create a pattern binding to initialize the range and wire it into the
      // AST.
      Pattern *RangePat = new (TC.Context) NamedPattern(Range);
      S->setRange(new (TC.Context) PatternBindingDecl(S->getForLoc(),
                                                      RangePat, GetElements,
                                                      DC));
    }
    
    // FIXME: Would like to customize the diagnostic emitted in
    // conformsToProtocol().
    ProtocolConformance *Conformance = nullptr;
    if (!TC.conformsToProtocol(RangeTy, EnumeratorProto, &Conformance,
                               Container->getLoc()))
      return nullptr;
    
    // Gather the witnesses from the Range protocol conformance. These are
    // the functions we'll call.
    FuncDecl *nextFn = 0;
    Type ElementTy;
    
    for (auto Member : EnumeratorProto->getMembers()) {
      auto Value = dyn_cast<ValueDecl>(Member);
      if (!Value)
        continue;
      
      StringRef Name = Value->getName().str();
      if (Name.equals("Element") && isa<TypeDecl>(Value)) {
        if (Conformance) {
          ArchetypeType *Archetype
            = cast<TypeDecl>(Value)->getDeclaredType()->getAs<ArchetypeType>();
          ElementTy = Conformance->TypeMapping[Archetype];
        } else {
          ElementTy = cast<TypeDecl>(Value)->getDeclaredType();
        }
        ElementTy = TC.substMemberTypeWithBase(ElementTy, Value, RangeTy);
      } else if (Name.equals("next") && isa<FuncDecl>(Value)) {
        if (Conformance)
          nextFn = cast<FuncDecl>(Conformance->Mapping[Value]);
        else
          nextFn = cast<FuncDecl>(Value);
      }
    }
    
    if (!nextFn || !ElementTy) {
      TC.diagnose(EnumeratorProto->getLoc(), diag::range_protocol_broken);
      return nullptr;
    }
    
    // Compute the expression that determines whether the range is empty.
    Expr *Empty
      = TC.callWitness(new (TC.Context) DeclRefExpr(
                                          Range, S->getInLoc(),
                                          Range->getTypeOfReference()),
                       DC, EnumeratorProto, Conformance,
                       TC.Context.getIdentifier("isEmpty"),
                       { },
                       diag::range_protocol_broken);
    if (!Empty) return nullptr;
    if (TC.typeCheckCondition(Empty, DC)) return nullptr;
    S->setRangeEmpty(Empty);
    
    // Compute the expression that extracts a value from the range.
    Expr *GetFirstAndAdvance
      = TC.callWitness(new (TC.Context) DeclRefExpr(
                                          Range, S->getInLoc(),
                                          Range->getTypeOfReference()),
                       DC, EnumeratorProto, Conformance,
                       TC.Context.getIdentifier("next"),
                       { },
                       diag::range_protocol_broken);
    if (!GetFirstAndAdvance) return nullptr;
    
    S->setElementInit(new (TC.Context) PatternBindingDecl(S->getForLoc(),
                                                          S->getPattern(),
                                                          GetFirstAndAdvance,
                                                          DC));

    // Coerce the pattern to the element type, now that we know the element
    // type.
    if (TC.coerceToType(S->getPattern(), DC, ElementTy))
      return nullptr;
    
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
    S->setFallthroughDest(FallthroughDest);
    return nullptr;
  }
  
  Stmt *visitSwitchStmt(SwitchStmt *S) {
    TC.diagnose(S->getLoc(), diag::not_implemented);
    return nullptr;
  }

  Stmt *visitCaseStmt(CaseStmt *S) {
    // Cases are handled in typeCheckCaseStmt.
    llvm_unreachable("case stmt outside of switch?!");
  }
};
  
} // end anonymous namespace
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  for (auto &elem : BS->getElements()) {
    if (Expr *SubExpr = elem.dyn_cast<Expr*>()) {
      // Type check the expression.
      if (typeCheckExpr(SubExpr)) continue;
      
      if (TC.TU.Kind != TranslationUnit::REPL || !isa<TopLevelCodeDecl>(DC))
        TC.typeCheckIgnoredExpr(SubExpr);
      elem = SubExpr;
      continue;
    }

    if (Stmt *SubStmt = elem.dyn_cast<Stmt*>()) {
      if (!typeCheckStmt(SubStmt))
        elem = SubStmt;
    } else {
      TC.typeCheckDecl(elem.get<Decl*>(), /*isFirstPass*/false);
    }
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
}

// Type check a function body (defined with the func keyword) that is either a
// named function or an anonymous func expression.
void TypeChecker::typeCheckFunctionBody(FuncExpr *FE) {
  // If this was a func() expression whose context did not fully infer types for
  // the arguments, mark the argument types as error type.
  if (FE->getType()->isUnresolvedType()) {
    for (auto P : FE->getArgParamPatterns())
      coerceToType(P, FE, ErrorType::get(Context));
    for (auto P : FE->getBodyParamPatterns())
      coerceToType(P, FE, ErrorType::get(Context));
  }
  
  BraceStmt *BS = FE->getBody();
  if (!BS)
    return;

  StmtChecker(*this, FE).typeCheckStmt(BS);
  FE->setBody(BS);
}

/// \brief Given a pattern declaring some number of member variables, build an
/// expression that references the variables relative to 'this' with the same
/// structure as the pattern.
///
/// \param tc The type checker.
/// \param thisDecl The 'this' declaration.
/// \param pattern The pattern.
static Expr *createPatternMemberRefExpr(TypeChecker &tc, VarDecl *thisDecl,
                                        Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Any:
    // FIXME: Unfortunate case. We have no way to represent 'forget this value'
    // in the AST.
    return nullptr;

  case PatternKind::Named:
    return new (tc.Context) UnresolvedDotExpr(
             new (tc.Context) DeclRefExpr(thisDecl,
                                          SourceLoc(),
                                          thisDecl->getTypeOfReference()),
             SourceLoc(), 
             cast<NamedPattern>(pattern)->getDecl()->getName(), 
             SourceLoc());

  case PatternKind::Paren:
    return createPatternMemberRefExpr(
             tc, thisDecl,
             cast<ParenPattern>(pattern)->getSubPattern());

  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(pattern);
    SmallVector<Expr *, 4> elements;
    for (auto elt : tuple->getFields()) {
      auto sub = createPatternMemberRefExpr(tc, thisDecl, elt.getPattern());
      if (!sub)
        return nullptr;

      elements.push_back(sub);
    }

    if (elements.size() == 1)
      return elements[0];
    return new (tc.Context) TupleExpr(SourceLoc(),
                                      tc.Context.AllocateCopy(elements),
                                      nullptr,
                                      SourceLoc(),
                                      /*hasTrailingClosure=*/false);
  }

  case PatternKind::Typed:
    return createPatternMemberRefExpr(
             tc,
             thisDecl,
             cast<TypedPattern>(pattern)->getSubPattern());
      
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("pattern can't appear in constructor decl!");
  }
}

void TypeChecker::typeCheckConstructorBody(ConstructorDecl *ctor) {
  if (auto allocThis = ctor->getAllocThisExpr()) {
    if (!typeCheckExpression(allocThis, ctor))
      ctor->setAllocThisExpr(allocThis);
  }

  BraceStmt *body = ctor->getBody();

  if (body) {
    // Type-check the body.
    StmtChecker(*this, ctor).typeCheckStmt(body);

    // Figure out which members already have initializers. We don't
    // default-initialize those members.
    // FIXME: This traversal is quite simplistic and quite stupid. It should
    // use dataflow analysis to determine which members are guaranteed to
    // be (manually) initialized before they are used.
    bool allOfThisInitialized = false;
    auto nominalDecl = ctor->getDeclContext()->getDeclaredTypeInContext()
                         ->getNominalOrBoundGenericNominal();
    llvm::SmallPtrSet<VarDecl *, 4> initializedMembers;
    for (auto elt : body->getElements()) {
      auto expr = elt.dyn_cast<Expr *>();
      if (!expr)
        continue;

      auto assign = dyn_cast<AssignExpr>(expr);
      if (!assign)
        continue;

      // We have an assignment. Check whether the left-hand side refers to
      // a member of our class.
      // FIXME: Also look into TupleExpr destinations.
      VarDecl *member = nullptr;
      auto dest = assign->getDest()->getSemanticsProvidingExpr();
      if (auto memberRef = dyn_cast<MemberRefExpr>(dest))
        member = memberRef->getDecl();
      else if (auto memberRef = dyn_cast<ExistentialMemberRefExpr>(dest))
        member = dyn_cast<VarDecl>(memberRef->getDecl());
      else if (auto memberRef = dyn_cast<ArchetypeMemberRefExpr>(dest))
        member = dyn_cast<VarDecl>(memberRef->getDecl());
      else if (auto memberRef = dyn_cast<GenericMemberRefExpr>(dest))
        member = dyn_cast<VarDecl>(memberRef->getDecl());
      else if (auto memberRef = dyn_cast<UnresolvedDotExpr>(dest)) {
        if (auto base = dyn_cast<DeclRefExpr>(
                          memberRef->getBase()->getSemanticsProvidingExpr())) {
          if (base->getDecl()->getName().str().equals("this")) {
            // Look for the member within this type.
            auto memberDecls
              = lookupMember(nominalDecl->getDeclaredTypeInContext(),
                             memberRef->getName(), /*isTypeLookup=*/false);
            if (memberDecls.size() == 1)
              member = dyn_cast<VarDecl>(memberDecls[0]);
          }
        }
      } else if (auto declRef = dyn_cast<DeclRefExpr>(dest)) {
        // If the left-hand side is 'this', we're initializing the
        // whole object.
        if (declRef->getDecl()->getName().str().equals("this")) {
          allOfThisInitialized = true;
          break;
        }
      }

      if (member)
        initializedMembers.insert(member);
    }

    // Default-initialize all of the members.
    SmallVector<BraceStmt::ExprStmtOrDecl, 4> defaultInits;
    if (!allOfThisInitialized) {
      for (auto member : nominalDecl->getMembers()) {
        // We only care about pattern bindings.
        auto patternBind = dyn_cast<PatternBindingDecl>(member);
        if (!patternBind)
          continue;

        // If the pattern has an initializer, use it.
        // FIXME: Implement this.
        if (auto initializer = patternBind->getInit()) {
          // Create a tuple expression with the same structure as the
          // pattern.
          if (Expr *dest = createPatternMemberRefExpr(
                             *this,
                             ctor->getImplicitThisDecl(),
                             patternBind->getPattern())) {
            initializer = new (Context) DefaultValueExpr(initializer);
            Expr *assign = new (Context) AssignExpr(dest, SourceLoc(),
                                                    initializer);
            typeCheckExpression(assign, ctor);
            defaultInits.push_back(assign);
            continue;
          }

          diagnose(body->getLBraceLoc(), diag::decl_no_default_init_ivar_hole);
          diagnose(patternBind->getLoc(), diag::decl_init_here);
        }

        // Find the variables in the pattern. They'll each need to be
        // default-initialized.
        SmallVector<VarDecl *, 4> variables;
        patternBind->getPattern()->collectVariables(variables);

        // Initialize the variables.
        for (auto var : variables) {
          if (var->isProperty())
            continue;

          // If we already saw an initializer for this member, don't initialize it.
          if (!initializedMembers.insert(var))
            continue;

          // If this variable is not default-initializable, we're done: we can't
          // add the default constructor because it will be ill-formed.
          Expr *initializer = nullptr;
          if (!isDefaultInitializable(var->getType(), &initializer)) {
            diagnose(body->getLBraceLoc(), diag::decl_no_default_init_ivar,
                     var->getName(), var->getType());
            diagnose(var->getLoc(), diag::decl_declared_here, var->getName());
            continue;
          }

          // Create the assignment.
          auto thisDecl = ctor->getImplicitThisDecl();
          Expr *dest
            = new (Context) UnresolvedDotExpr(
                new (Context) DeclRefExpr(thisDecl,
                                          SourceLoc(),
                                          thisDecl->getTypeOfReference()),
                SourceLoc(), 
                var->getName(),
                SourceLoc());
          Expr *assign = new (Context) AssignExpr(dest, SourceLoc(),
                                                  initializer);
          typeCheckExpression(assign, ctor);
          defaultInits.push_back(assign);
        }
      }
    }
    
    // If we added any default initializers, update the body.
    if (!defaultInits.empty()) {
      defaultInits.append(body->getElements().begin(),
                          body->getElements().end());

      body = BraceStmt::create(Context, body->getLBraceLoc(), defaultInits,
                               body->getRBraceLoc());
    }

    ctor->setBody(body);
  }
}

void TypeChecker::typeCheckDestructorBody(DestructorDecl *DD) {
  Stmt *Body = DD->getBody();
  StmtChecker(*this, DD).typeCheckStmt(Body);
}

void TypeChecker::typeCheckClosureBody(PipeClosureExpr *closure) {
  BraceStmt *body = closure->getBody();
  StmtChecker(*this, closure).typeCheckStmt(body);
  if (body) {
    closure->setBody(body, closure->hasSingleExpressionBody());
  }
}

void TypeChecker::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  BraceStmt *Body = TLCD->getBody();
  StmtChecker(*this, TLCD).typeCheckStmt(Body);
  TLCD->setBody(Body);
}
