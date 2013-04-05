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

  unsigned LoopNestLevel;

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
    : TC(TC), TheFunc(TheFunc), DC(TheFunc), LoopNestLevel(0) { }

  StmtChecker(TypeChecker &TC, ConstructorDecl *TheCtor)
    : TC(TC), TheFunc(TheCtor), DC(TheCtor), LoopNestLevel(0) { }
  
  StmtChecker(TypeChecker &TC, DestructorDecl *TheDtor)
    : TC(TC), TheFunc(TheDtor), DC(TheDtor), LoopNestLevel(0) { }
  
  StmtChecker(TypeChecker &TC, DeclContext *DC)
    : TC(TC), TheFunc(), DC(DC), LoopNestLevel(0) { }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//
  
  bool typeCheckExpr(Expr *&E, Type DestTy = Type()) {
    return TC.typeCheckExpression(E, DestTy);
  }

  template<typename StmtTy>
  bool typeCheckStmt(StmtTy *&S) {
    StmtTy *S2 = cast_or_null<StmtTy>(visit(S));
    if (S2 == 0) return true;
    S = S2;
    return false;
  }
  
  bool typeCheck(PointerUnion<Expr*, AssignStmt*> &Val) {
    if (Expr *E = Val.dyn_cast<Expr*>()) {
      if (typeCheckExpr(E)) return true;
      Val = E;
    } else if (AssignStmt *S = Val.dyn_cast<AssignStmt*>()) {
      if (typeCheckStmt(S)) return true;
      Val = S;
    }
    return false;
  }
 
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  Stmt *visitAssignStmt(AssignStmt *S) {
    Expr *Dest = S->getDest();
    Expr *Src = S->getSrc();
    if (TC.typeCheckAssignment(Dest, S->getEqualLoc(), Src))
      return 0;
    
    S->setDest(Dest);
    S->setSrc(Src);
    return S;
  }
  
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
    if (TC.typeCheckCondition(E)) return 0;
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
    if (TC.typeCheckCondition(E)) return 0;
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
    if (TC.typeCheckCondition(E)) return 0;
    WS->setCond(E);
    return WS;
  }
  Stmt *visitForStmt(ForStmt *FS) {
    // Type check any var decls in the initializer.
    for (auto D : FS->getInitializerVarDecls())
      TC.typeCheckDecl(D, /*isFirstPass*/false);

    PointerUnion<Expr*, AssignStmt*> Tmp = FS->getInitializer();
    if (typeCheck(Tmp)) return 0;
    FS->setInitializer(Tmp);
    
    // Type check the condition if present.
    if (FS->getCond().isNonNull()) {
      Expr *E = FS->getCond().get();
      if (TC.typeCheckCondition(E)) return 0;
      FS->setCond(E);
    }
    
    Tmp = FS->getIncrement();
    if (typeCheck(Tmp)) return 0;
    FS->setIncrement(Tmp);

    AddLoopNest loopNest(*this);
    Stmt *S = FS->getBody();
    if (typeCheckStmt(S)) return 0;
    FS->setBody(S);
    
    return FS;
  }
  
  /// callNullaryMethodOf - Form a call (with no arguments) to the given
  /// method of the given base.
  Expr *callNullaryMethodOf(Expr *Base, FuncDecl *Method, SourceLoc Loc) {
    // Form the method reference.
    Expr *Mem = TC.buildMemberRefExpr(Base, Loc, Method, Loc);
    if (!Mem) return nullptr;
    Mem = TC.recheckTypes(Mem);
    if (!Mem) return nullptr;

    // Call the method.
    Expr *EmptyArgs
      = new (TC.Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, Loc,
                                   TupleType::getEmpty(TC.Context));
    ApplyExpr *Call = new (TC.Context) CallExpr(Mem, EmptyArgs);
    Expr *Result = TC.semaApplyExpr(Call);
    if (!Result) return nullptr;
    return TC.convertToRValue(Result);
  }
  
  /// callNullaryMemberOf - Form a call (with no arguments) to a method of the
  /// given base.
  Expr *callNullaryMethodOf(Expr *Base, Identifier Name, SourceLoc Loc,
                            Diag<Type> MissingMember,
                            Diag<Type> NonFuncMember) {
    Type BaseType = Base->getType()->getRValueType();

    // Look for name.
    MemberLookup Lookup(BaseType, Name, TC.TU);
    if (!Lookup.isSuccess()) {
      TC.diagnose(Loc, MissingMember, BaseType)
        << Base->getSourceRange();
      return nullptr;
    }
    
    // Make sure we found a function (which may be overloaded, of course).
    if (!isa<FuncDecl>(Lookup.Results.front().D)) {
      TC.diagnose(Loc, NonFuncMember, BaseType)
        << Base->getSourceRange();
      TC.diagnose(Lookup.Results.front().D,
                  diag::decl_declared_here,
                  Lookup.Results.front().D->getName());
      return nullptr;
    }
    
    // Form base.name
    Expr *Mem = TC.buildMemberRefExpr(Base, Loc, Lookup, Loc);
    Mem = TC.recheckTypes(Mem);
    if (!Mem) return nullptr;
    
    // Call base.name()
    Expr *EmptyArgs
      = new (TC.Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, Loc,
                                   TupleType::getEmpty(TC.Context));
    ApplyExpr *Call = new (TC.Context) CallExpr(Mem, EmptyArgs);
    Expr *Result = TC.semaApplyExpr(Call);
    if (!Result) return nullptr;
    return TC.convertToRValue(Result);
  }
  
  Stmt *visitForEachStmt(ForEachStmt *S) {
    // Type-check the container and convert it to an rvalue.
    Expr *Container = S->getContainer();
    if (TC.typeCheckExpression(Container)) return nullptr;
    S->setContainer(Container);

    // Retrieve the 'Enumerable' protocol.
    ProtocolDecl *EnumerableProto = TC.getEnumerableProtocol();
    if (!EnumerableProto) {
      TC.diagnose(S->getForLoc(), diag::foreach_missing_enumerable);
      return nullptr;
    }

    // Retrieve the 'Enumerator' protocol.
    ProtocolDecl *EnumeratorProto = TC.getEnumeratorProtocol();
    if (!EnumeratorProto) {
      TC.diagnose(S->getForLoc(), diag::foreach_missing_range);
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

      // Gather the witness from the Enumerable protocol conformance. This are
      // the functions we'll call.
      FuncDecl *getElementsFn = 0;
      
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
        } else if (Name.equals("getEnumeratorType") && isa<FuncDecl>(Value)) {
          if (Conformance)
            getElementsFn = cast<FuncDecl>(Conformance->Mapping[Value]);
          else
            getElementsFn = cast<FuncDecl>(Value);
        }
      }

      if (!getElementsFn || !RangeTy) {
        TC.diagnose(EnumerableProto->getLoc(), diag::enumerable_protocol_broken);
        return nullptr;
      }
      
      Expr *GetElements = callNullaryMethodOf(Container, getElementsFn,
                                              S->getInLoc());
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
    FuncDecl *isEmptyFn = 0;
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
      } else if (Name.equals("isEmpty") && isa<FuncDecl>(Value)) {
        if (Conformance)
          isEmptyFn = cast<FuncDecl>(Conformance->Mapping[Value]);
        else
          isEmptyFn = cast<FuncDecl>(Value);
      }
      else if (Name.equals("next") && isa<FuncDecl>(Value)) {
        if (Conformance)
          nextFn = cast<FuncDecl>(Conformance->Mapping[Value]);
        else
          nextFn = cast<FuncDecl>(Value);
      }
    }
    
    if (!isEmptyFn || !nextFn || !ElementTy) {
      TC.diagnose(EnumeratorProto->getLoc(), diag::range_protocol_broken);
      return nullptr;
    }
    
    // Compute the expression that determines whether the range is empty.
    Expr *Empty
      = callNullaryMethodOf(
          new (TC.Context) DeclRefExpr(Range, S->getInLoc(),
                                       Range->getTypeOfReference()),
          isEmptyFn, S->getInLoc());
    if (!Empty) return nullptr;
    if (TC.typeCheckCondition(Empty)) return nullptr;
    S->setRangeEmpty(Empty);
    
    // Compute the expression that extracts a value from the range.
    Expr *GetFirstAndAdvance
      = callNullaryMethodOf(
          new (TC.Context) DeclRefExpr(Range, S->getInLoc(),
                                       Range->getTypeOfReference()),
          nextFn,
          S->getInLoc());
    if (!GetFirstAndAdvance) return nullptr;
    
    S->setElementInit(new (TC.Context) PatternBindingDecl(S->getForLoc(),
                                                          S->getPattern(),
                                                          GetFirstAndAdvance,
                                                          DC));

    // Coerce the pattern to the element type, now that we know the element
    // type.
    if (TC.coerceToType(S->getPattern(), ElementTy, /*isFirstPass*/false))
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
  
  /// Check a case of a switch statement.
  bool typeCheckCaseStmt(CaseStmt *Case, SwitchStmt *ParentSwitch) {
    if (!Case->isDefault()) {      
      // Synthesize the expression ($switch =~ value) || ($switch =~ value) || ...
      // from the value expressions of the case as its condition expression.
      VarDecl *subject = ParentSwitch->getSubjectDecl();
      
      // Make overloaded refs for the '=~' and '||' operators.
      
      Identifier matchOp = TC.Context.getIdentifier("=~");
      UnqualifiedLookup matchLookup(matchOp, &TC.TU);
      if (!matchLookup.isSuccess()) {
        TC.diagnose(ParentSwitch->getLoc(), diag::no_match_operator);
        return true;
      }
      SmallVector<ValueDecl*, 8> matchDecls;
      std::transform(matchLookup.Results.begin(), matchLookup.Results.end(),
                     std::back_inserter(matchDecls),
                     [](UnqualifiedLookupResult &res) {
                       return res.getValueDecl();
                     });
      
      Identifier orOp = TC.Context.getIdentifier("||");
      UnqualifiedLookup orLookup(orOp, &TC.TU);
      if (!orLookup.isSuccess()) {
        TC.diagnose(ParentSwitch->getLoc(), diag::no_or_operator);
        return true;
      }
      SmallVector<ValueDecl*, 2> orDecls;
      std::transform(orLookup.Results.begin(), orLookup.Results.end(),
                     std::back_inserter(orDecls),
                     [](UnqualifiedLookupResult &res) {
                       return res.getValueDecl();
                     });
      
      // Construct the condition expression.
      Expr *condition = nullptr;
      
      for (Expr *valueExpr : Case->getValueExprs()) {
        auto *subjRef
          = new (TC.Context) DeclRefExpr(subject,
                                         valueExpr->getLoc(),
                                         subject->getTypeOfReference());
        Expr *matchArgs[] = {subjRef, valueExpr};
        TupleExpr *matchArgExpr
          = new (TC.Context) TupleExpr(SourceLoc(),
                     TC.Context.AllocateCopy(MutableArrayRef<Expr*>(matchArgs)),
                     nullptr,
                     SourceLoc());
        Expr *matchRef = TC.buildRefExpr(matchDecls, valueExpr->getLoc());
        Expr *nextCondition
          = new (TC.Context) BinaryExpr(matchRef, matchArgExpr);
        
        if (condition) {
          Expr *orArgs[] = {condition, nextCondition};
          TupleExpr *orArgExpr
            = new (TC.Context) TupleExpr(SourceLoc(),
                       TC.Context.AllocateCopy(MutableArrayRef<Expr*>(orArgs)),
                       nullptr,
                       SourceLoc());
          Expr *orRef = TC.buildRefExpr(orDecls, valueExpr->getLoc());
          condition = new (TC.Context) BinaryExpr(orRef, orArgExpr);
        } else
          condition = nextCondition;
      }
      
      if (TC.typeCheckCondition(condition))
        return true;
      Case->setConditionExpr(condition);
    }
    
    Stmt *Body = Case->getBody();
    if (typeCheckStmt(Body))
      return true;
    Case->setBody(Body);
    
    return false;
  }
  
  Stmt *visitSwitchStmt(SwitchStmt *S) {
    Expr *subjectExpr = S->getSubjectExpr();
    if (typeCheckExpr(subjectExpr))
      return nullptr;
    subjectExpr = TC.convertToMaterializable(subjectExpr);
    if (!subjectExpr)
      return nullptr;
    S->setSubjectExpr(subjectExpr);
    S->getSubjectDecl()->setType(subjectExpr->getType());
    
    bool failed = false;
    CaseStmt *defaultCase = nullptr;
    for (CaseStmt *C : S->getCases()) {
      if (C->isDefault()) {
        if (defaultCase) {
          TC.diagnose(C->getLoc(), diag::multiple_defaults_in_switch);
          TC.diagnose(defaultCase->getLoc(), diag::previous_default);
        } else
          defaultCase = C;
      }
      failed |= typeCheckCaseStmt(C, S);
    }
    
    return failed ? nullptr : S;
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

      // If this is the top level of the REPL, apply secret sauce.
      if (isa<TopLevelCodeDecl>(DC) && TC.TU.Kind == TranslationUnit::Repl)
        TC.typeCheckTopLevelReplExpr(SubExpr, cast<TopLevelCodeDecl>(DC));
      else
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
      << E->getSourceRange();
    return;
  }

  // Complain about functions that aren't called.
  // TODO: What about tuples which contain functions by-value that are
  // dead?
  if (E->getType()->is<AnyFunctionType>()) {
    diagnose(E->getLoc(), diag::expression_unused_function)
      << E->getSourceRange();
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
      coerceToType(P, ErrorType::get(Context), false);
    for (auto P : FE->getBodyParamPatterns())
      coerceToType(P, ErrorType::get(Context), false);
  }
  
  BraceStmt *BS = FE->getBody();
  if (!BS)
    return;

  StmtChecker(*this, FE).typeCheckStmt(BS);
  FE->setBody(BS);

  // Compute the capture list, now that we have analyzed the expression.
  FE->computeCaptures(Context);
}

void TypeChecker::typeCheckConstructorBody(ConstructorDecl *CD) {
  if (auto allocThis = CD->getAllocThisExpr()) {
    if (!typeCheckExpression(allocThis))
      CD->setAllocThisExpr(allocThis);
  }

  Stmt *Body = CD->getBody();
  if (Body)
    StmtChecker(*this, CD).typeCheckStmt(Body);
}

void TypeChecker::typeCheckDestructorBody(DestructorDecl *DD) {
  Stmt *Body = DD->getBody();
  StmtChecker(*this, DD).typeCheckStmt(Body);
}

void TypeChecker::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  BraceStmt *Body = TLCD->getBody();
  StmtChecker(*this, TLCD).typeCheckStmt(Body);
  TLCD->setBody(Body);
}

ProtocolDecl *TypeChecker::getEnumerableProtocol() {
  if (!EnumerableProto) {
    UnqualifiedLookup Globals(Context.getIdentifier("Enumerable"), &TU);
    EnumerableProto
      = dyn_cast_or_null<ProtocolDecl>(Globals.getSingleTypeResult());
  }
  
  return EnumerableProto;
}

ProtocolDecl *TypeChecker::getEnumeratorProtocol() {
  if (!EnumeratorProto) {
    UnqualifiedLookup Globals(Context.getIdentifier("Enumerator"), &TU);
    EnumeratorProto
      = dyn_cast_or_null<ProtocolDecl>(Globals.getSingleTypeResult());
  }
  
  return EnumeratorProto;
}

ProtocolDecl *TypeChecker::getArrayLiteralProtocol() {
  if (!ArrayLiteralProto) {
    UnqualifiedLookup Globals(Context.getIdentifier("ArrayLiteralConvertible"),
                              &TU);
    ArrayLiteralProto
      = dyn_cast_or_null<ProtocolDecl>(Globals.getSingleTypeResult());
  }
  
  return ArrayLiteralProto;
}

ProtocolDecl *TypeChecker::getDictionaryLiteralProtocol() {
  if (!DictionaryLiteralProto) {
    UnqualifiedLookup Globals(
                        Context.getIdentifier("DictionaryLiteralConvertible"),
                        &TU);
    DictionaryLiteralProto
      = dyn_cast_or_null<ProtocolDecl>(Globals.getSingleTypeResult());
  }
  
  return DictionaryLiteralProto;
}
