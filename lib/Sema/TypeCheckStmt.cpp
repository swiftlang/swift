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
  
  // TheFunc - This is the current FuncExpr being checked.  This is null for
  // top level code.
  FuncExpr *TheFunc;
  
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

  StmtChecker(TypeChecker &TC, DeclContext *DC)
    : TC(TC), TheFunc(nullptr), DC(DC), LoopNestLevel(0) { }

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

  Stmt *visitSemiStmt(SemiStmt *S) {
    return S;
  }

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
  
  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (TheFunc == 0) {
      TC.diagnose(RS->getReturnLoc(), diag::return_invalid_outside_func);
      return 0;
    }

    Type ResultTy = TheFunc->getBodyResultType();
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
      TC.diagnose(Lookup.Results.front().D->getStartLoc(),
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

    // Retrieve the 'Range' protocol.
    ProtocolDecl *RangeProto = TC.getRangeProtocol();
    if (!RangeProto) {
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
        if (Name.equals("Elements") && isa<TypeDecl>(Value)) {
          if (Conformance) {
            ArchetypeType *Archetype
              = cast<TypeDecl>(Value)->getDeclaredType()->getAs<ArchetypeType>();
            RangeTy = Conformance->TypeMapping[Archetype];
          } else {
            RangeTy = cast<TypeDecl>(Value)->getDeclaredType();
          }
          RangeTy = TC.substMemberTypeWithBase(RangeTy, Value, ContainerType);
        } else if (Name.equals("getElements") && isa<FuncDecl>(Value)) {
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
    if (!TC.conformsToProtocol(RangeTy, RangeProto, &Conformance,
                               Container->getLoc()))
      return nullptr;
    
    // Gather the witnesses from the Range protocol conformance. These are
    // the functions we'll call.
    FuncDecl *isEmptyFn = 0;
    FuncDecl *getFirstAndAdvanceFn = 0;
    Type ElementTy;
    
    for (auto Member : RangeProto->getMembers()) {
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
      else if (Name.equals("getFirstAndAdvance") && isa<FuncDecl>(Value)) {
        if (Conformance)
          getFirstAndAdvanceFn = cast<FuncDecl>(Conformance->Mapping[Value]);
        else
          getFirstAndAdvanceFn = cast<FuncDecl>(Value);
      }
    }
    
    if (!isEmptyFn || !getFirstAndAdvanceFn || !ElementTy) {
      TC.diagnose(RangeProto->getLoc(), diag::range_protocol_broken);
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
          getFirstAndAdvanceFn,
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
};
  
} // end anonymous namespace
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  for (auto &elem : BS->elements()) {
    if (Expr *SubExpr = elem.dyn_cast<Expr*>()) {
      if (typeCheckExpr(SubExpr)) continue;
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

// Type check a function body (defined with the func keyword) that is either a
// named function or an anonymous func expression.
void TypeChecker::typeCheckFunctionBody(FuncExpr *FE) {
  // If this was a func() expression whose context did not fully infer types for
  // the arguments, mark the argument types as error type.
  if (FE->getType()->isUnresolvedType()) {
    for (auto P : FE->getParamPatterns())
      coerceToType(P, ErrorType::get(Context), false);
  }
  // If we failed to infer a return type for the FuncExpr, force it to
  // ErrorType.
  if (FE->getBodyResultType()->isUnresolvedType())
    FE->getBodyResultTypeLoc().setInvalidType(Context);
  
  BraceStmt *BS = FE->getBody();
  if (!BS)
    return;

  StmtChecker(*this, FE).typeCheckStmt(BS);
  FE->setBody(BS);
}

void TypeChecker::typeCheckConstructorBody(ConstructorDecl *CD) {
  Stmt *Body = CD->getBody();
  if (Body)
    StmtChecker(*this, CD).typeCheckStmt(Body);
}

void TypeChecker::typeCheckDestructorBody(DestructorDecl *DD) {
  Stmt *Body = DD->getBody();
  StmtChecker(*this, DD).typeCheckStmt(Body);
}

void TypeChecker::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  auto Elem = TLCD->getBody();
  if (Expr *E = Elem.dyn_cast<Expr*>()) {
    if (typeCheckExpression(E))
      return;
    if (TU.Kind == TranslationUnit::Repl)
      typeCheckTopLevelReplExpr(E, TLCD);
    else
      typeCheckIgnoredExpr(E);
    TLCD->setBody(E);
  } else {
    Stmt *S = Elem.get<Stmt*>();
    if (StmtChecker(*this, TLCD).typeCheckStmt(S))
      return;
    TLCD->setBody(S);
  }
}

ProtocolDecl *TypeChecker::getEnumerableProtocol() {
  if (!EnumerableProto) {
    UnqualifiedLookup Globals(Context.getIdentifier("Enumerable"), &TU);
    EnumerableProto
      = dyn_cast_or_null<ProtocolDecl>(Globals.getSingleTypeResult());
  }
  
  return EnumerableProto;
}

ProtocolDecl *TypeChecker::getRangeProtocol() {
  if (!RangeProto) {
    UnqualifiedLookup Globals(Context.getIdentifier("Range"), &TU);
    RangeProto = dyn_cast_or_null<ProtocolDecl>(Globals.getSingleTypeResult());
  }
  
  return RangeProto;
}
