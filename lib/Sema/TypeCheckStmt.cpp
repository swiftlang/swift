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
#include "NameLookup.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/PrettyStackTrace.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "NameLookup.h"

using namespace swift;

namespace {
/// StmtChecker - This class implements 
class StmtChecker : public StmtVisitor<StmtChecker, Stmt*> {
public:
  TypeChecker &TC;
  
  // TheFunc - This is the current FuncExpr being checked.  This is null for
  // top level code.
  FuncExpr *TheFunc;
  
  /// TopLevelCode - This is the current top-level code declaration being
  /// checked, or null if we are in a function.
  TopLevelCodeDecl *TopLevelCode;
  
  StmtChecker(TypeChecker &TC, FuncExpr *TheFunc)
    : TC(TC), TheFunc(TheFunc), TopLevelCode() { }

  void setTopLevelCode(TopLevelCodeDecl *TLC) { TopLevelCode = TLC; }
  
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

  Stmt *visitErrorStmt(ErrorStmt *S) {
    return S;
  }

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

    Stmt *S = WS->getBody();
    if (typeCheckStmt(S)) return 0;
    WS->setBody(S);
    
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
    
    Stmt *S = FS->getBody();
    if (typeCheckStmt(S)) return 0;
    FS->setBody(S);
    
    return FS;
  }
  
  /// callNullaryMethodOf - Form a call (with no arguments) to the given
  /// method of the given base.
  Expr *callNullaryMethodOf(Expr *Base, FuncDecl *Method, SourceLoc Loc) {
    // Reference the method.
    Expr *Fn = new (TC.Context) DeclRefExpr(Method, Loc,
                                            Method->getTypeOfReference());
    
    // Form the method reference.
    // FIXME: Deal with protocols, when they get their own AST.
    Expr *Mem = new (TC.Context) DotSyntaxCallExpr(Fn, Loc, Base);
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
    Type BaseType = Base->getType();
    if (LValueType *BaseLV = BaseType->getAs<LValueType>())
      BaseType = BaseLV->getObjectType();
    
    // Look for name.
    MemberLookup Lookup(BaseType, Name, TC.TU);
    if (!Lookup.isSuccess()) {
      TC.diagnose(Loc, MissingMember, BaseType)
        << Base->getSourceRange();
      return nullptr;
    }
    
    // Make sure we found a function (which may be overloaded, of course).
    if (Lookup.Results.front().Kind == MemberLookupResult::TupleElement ||
        !isa<FuncDecl>(Lookup.Results.front().D)) {
      TC.diagnose(Loc, NonFuncMember, BaseType)
        << Base->getSourceRange();
      if (!Lookup.Results.front().Kind == MemberLookupResult::TupleElement)
        TC.diagnose(Lookup.Results.front().D->getLocStart(),
                    diag::decl_declared_here,
                    Lookup.Results.front().D->getName());
      return nullptr;
    }
    
    // Form base.name
    Expr *Mem = Lookup.createResultAST(Base, Loc, Loc, TC.Context);
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

    // Retrieve the 'Range' protocol.
    ProtocolDecl *RangeProto = TC.getRangeProtocol();
    if (!RangeProto) {
      TC.diagnose(S->getForLoc(), diag::foreach_missing_range);
      return nullptr;
    }
    
    // Figure out the declaration context we're in.
    DeclContext *DC = &TC.TU;
    if (TheFunc)
      DC = TheFunc;
    else if (TopLevelCode)
      DC = TopLevelCode;
    
    // Invoke getElements() on the container to retrieve the range of elements.
    Type RangeTy;
    VarDecl *Range;
    {
      Type ContainerType = Container->getType();
      if (LValueType *ContainerLV = ContainerType->getAs<LValueType>())
        ContainerType = ContainerLV->getObjectType();
      
      Expr *GetElements
        = callNullaryMethodOf(Container,
                              TC.Context.getIdentifier("getElements"),
                              S->getInLoc(), diag::foreach_getelements,
                              diag::foreach_nonfunc_getelements);
      if (!GetElements) return nullptr;
      
      // Make sure our range type is materializable.
      if (!GetElements->getType()->isMaterializable()) {
        TC.diagnose(S->getInLoc(), diag::foreach_nonmaterializable_range,
                    GetElements->getType(), ContainerType)
          << Container->getSourceRange();
        return nullptr;
      }
      
      // Create a local variable to capture the range.
      // FIXME: Mark declaration as implicit?
      RangeTy = GetElements->getType();
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
    ProtocolConformance *Conformance
      = TC.conformsToProtocol(RangeTy, RangeProto, Container->getLoc());
    if (!Conformance)
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
        ArchetypeType *Archetype
          = cast<TypeDecl>(Value)->getDeclaredType()->getAs<ArchetypeType>();
        ElementTy = Conformance->TypeMapping[Archetype];
      } else if (Name.equals("isEmpty") && isa<FuncDecl>(Value))
        isEmptyFn = cast<FuncDecl>(Conformance->Mapping[Value]);
      else if (Name.equals("getFirstAndAdvance") && isa<FuncDecl>(Value))
        getFirstAndAdvanceFn = cast<FuncDecl>(Conformance->Mapping[Value]);
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
    BraceStmt *Body = S->getBody();
    if (typeCheckStmt(Body)) return nullptr;
    S->setBody(Body);
    
    return S;
  }
};
  
} // end anonymous namespace
  
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  for (unsigned i = 0, e = BS->getNumElements(); i != e; ++i) {
    if (Expr *SubExpr = BS->getElement(i).dyn_cast<Expr*>()) {
      if (typeCheckExpr(SubExpr)) continue;
      TC.typeCheckIgnoredExpr(SubExpr);
      BS->setElement(i, SubExpr);
      continue;
    }
    
    if (Stmt *SubStmt = BS->getElement(i).dyn_cast<Stmt*>()) {
      if (!typeCheckStmt(SubStmt))
        BS->setElement(i, SubStmt);
    } else {
      TC.typeCheckDecl(BS->getElement(i).get<Decl*>(), /*isFirstPass*/false);
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
  if (E->getType()->is<FunctionType>()) {
    diagnose(E->getLoc(), diag::expression_unused_function)
      << E->getSourceRange();
    return;
  }
}

void
PrintLiteralString(StringRef Str, ASTContext &Context, SourceLoc Loc,
                   SmallVectorImpl<ValueDecl*> &PrintDecls,
                   SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent) {
  Expr *PrintStr = new (Context) StringLiteralExpr(Str, Loc);
  Expr *PrintStrFn = OverloadedDeclRefExpr::createWithCopy(PrintDecls, Loc);
  BodyContent.push_back(new (Context) CallExpr(PrintStrFn, PrintStr));
}

static void
PrintReplExpr(TypeChecker &TC, VarDecl *Arg, CanType T, SourceLoc Loc,
              SourceLoc EndLoc,
              SmallVectorImpl<unsigned> &MemberIndexes,
              SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent,
              SmallVectorImpl<ValueDecl*> &PrintDecls) {
  ASTContext &Context = TC.Context;
  TranslationUnit &TU = TC.TU;

  if (TupleType *TT = dyn_cast<TupleType>(T)) {
    // We print a tuple by printing each element.
    PrintLiteralString("(", Context, Loc, PrintDecls, BodyContent);

    for (unsigned i = 0, e = TT->getFields().size(); i < e; ++i) {
      MemberIndexes.push_back(i);
      CanType SubType = TT->getElementType(i)->getCanonicalType();
      PrintReplExpr(TC, Arg, SubType, Loc, EndLoc, MemberIndexes,
                    BodyContent, PrintDecls);
      MemberIndexes.pop_back();

      if (i + 1 != e)
        PrintLiteralString(", ", Context, Loc, PrintDecls, BodyContent);
    }

    PrintLiteralString(")", Context, Loc, PrintDecls, BodyContent);
    return;
  }

  Identifier MemberName = Context.getIdentifier("replPrint");
  MemberLookup Lookup(T, MemberName, TU);
  if (Lookup.isSuccess()) {
    Expr *ArgRef = new (Context) DeclRefExpr(Arg, Loc,
                                             Arg->getTypeOfReference());
    ArgRef = TC.convertToRValue(ArgRef);
    for (unsigned i : MemberIndexes) {
      // For each index, we look through a TupleType or StructType.
      CanType CurT = ArgRef->getType()->getCanonicalType();
      if (StructType *ST = dyn_cast<StructType>(CurT)) {
        VarDecl *VD = cast<VarDecl>(ST->getDecl()->getMembers()[i]);
        ArgRef = new (Context) MemberRefExpr(ArgRef, Loc, VD, Loc);
        ArgRef = TC.recheckTypes(ArgRef);
        continue;
      }
      TupleType *TT = cast<TupleType>(CurT);
      ArgRef = new (Context) SyntacticTupleElementExpr(ArgRef, Loc, i, Loc,
                                                       TT->getElementType(i));
    }
    Expr *Res = TC.recheckTypes(Lookup.createResultAST(ArgRef, Loc, EndLoc,
                                                       Context));
    if (!Res)
      return;
    TupleExpr *CallArgs =
        new (Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, EndLoc,
                                TupleType::getEmpty(Context));
    CallExpr *CE = new (Context) CallExpr(Res, CallArgs, Type());
    Res = TC.semaApplyExpr(CE);
    if (!Res)
      return;
    BodyContent.push_back(Res);
    return;
  }

  if (StructType *ST = dyn_cast<StructType>(T)) {
    // We print a struct by printing each non-property member variable.
    PrintLiteralString(ST->getDecl()->getName().str(), Context, Loc,
                       PrintDecls, BodyContent);
    PrintLiteralString("(", Context, Loc, PrintDecls, BodyContent);

    unsigned idx = 0;
    bool isFirstMember = true;
    for (Decl *D : ST->getDecl()->getMembers()) {
      if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
        if (!VD->isProperty()) {
          if (isFirstMember)
            isFirstMember = false;
          else
            PrintLiteralString(", ", Context, Loc, PrintDecls, BodyContent);

          MemberIndexes.push_back(idx);
          CanType SubType = VD->getType()->getCanonicalType();
          PrintReplExpr(TC, Arg, SubType, Loc, EndLoc, MemberIndexes,
                        BodyContent, PrintDecls);
          MemberIndexes.pop_back();
        }
      }
      ++idx;
    }
    PrintLiteralString(")", Context, Loc, PrintDecls, BodyContent);
    return;
  }

  // FIXME: Handle ClassTypes?

  // FIXME: We should handle OneOfTypes at some point, but
  // it's tricky to represent in the AST without a "match" statement.

  PrintLiteralString("<unprintable value>", Context, Loc, PrintDecls,
                     BodyContent);
}

/// Check an expression at the top level in a REPL.
void TypeChecker::typeCheckTopLevelReplExpr(Expr *&E, TopLevelCodeDecl *TLCD) {
  // If the input is an lvalue, force an lvalue-to-rvalue conversion.
  Expr *ConvertedE = convertToMaterializable(E);
  if (!ConvertedE)
    return;
  E = ConvertedE;

  CanType T = E->getType()->getCanonicalType();
  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();

  // Don't try to print invalid expressions.
  if (isa<ErrorType>(T))
    return;

  // Skip printing for expressions of void type.
  if (isa<TupleType>(T) && cast<TupleType>(T)->getFields().empty())
    return;

  // Build a function to call to print the expression.
  Type FuncTy = T;
  if (!isa<TupleType>(FuncTy)) {
    TupleTypeElt Elt(T, Context.getIdentifier("arg"));
    FuncTy = TupleType::get(Elt, Context);
  }
  FuncTy = FunctionType::get(FuncTy, TupleType::getEmpty(Context), Context);
  VarDecl *Arg = new (Context) VarDecl(Loc, Context.getIdentifier("arg"), T,
                                       nullptr);
  Pattern* ParamPat = new (Context) NamedPattern(Arg);
  FuncExpr *FE = FuncExpr::create(Context, Loc, ParamPat, FuncTy, 0, TLCD);
  Arg->setDeclContext(FE);

  // Build the body of the function which prints the expression.
  SmallVector<unsigned, 4> MemberIndexes;
  SmallVector<BraceStmt::ExprStmtOrDecl, 4> BodyContent;
  SmallVector<ValueDecl*, 4> PrintDecls;
  TU.lookupGlobalValue(Context.getIdentifier("print"),
                       NLKind::UnqualifiedLookup, PrintDecls);
  if (PrintDecls.empty())
    return;

  // Printing format is "Int = 0\n".
  auto TypeStr = Context.getIdentifier(E->getType()->getString()).str();
  PrintLiteralString(TypeStr, Context, Loc, PrintDecls, BodyContent);
  PrintLiteralString(" = ", Context, Loc, PrintDecls, BodyContent);
  PrintReplExpr(*this, Arg, T, Loc, EndLoc, MemberIndexes, BodyContent,
                PrintDecls);
  PrintLiteralString("\n", Context, Loc, PrintDecls, BodyContent);

  // Typecheck the function.
  BraceStmt *Body = BraceStmt::create(Context, Loc, BodyContent, EndLoc);
  StmtChecker(*this, FE).typeCheckStmt(Body);
  FE->setBody(Body);

  // Typecheck the call.
  CallExpr *CE = new (Context) CallExpr(FE, E);
  E = semaApplyExpr(CE);
}

struct PatternBindingPrintLHS : public ASTVisitor<PatternBindingPrintLHS> {
  SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent;
  SmallVectorImpl<ValueDecl*> &PrintDecls;
  SourceLoc Loc;
  ASTContext &Context;

  PatternBindingPrintLHS(SmallVectorImpl<BraceStmt::ExprStmtOrDecl>&BodyContent,
                         SmallVectorImpl<ValueDecl*> &PrintDecls,
                         SourceLoc Loc,
                         ASTContext &Context)
    : BodyContent(BodyContent), PrintDecls(PrintDecls), Loc(Loc),
      Context(Context) {}

  void print(StringRef Str) {
    PrintLiteralString(Str, Context, Loc, PrintDecls, BodyContent);
  }

  void visitTuplePattern(TuplePattern *P) {
    // We print tuples as "(x, y)".
    print("(");
    for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
      visit(P->getFields()[i].getPattern());
      if (i + 1 != e)
        print(", ");
    }
    print(")");
  }
  void visitNamedPattern(NamedPattern *P) {
    print(P->getBoundName().str());
  }
  void visitAnyPattern(AnyPattern *P) {
    print("_");
  }

  void visitTypedPattern(TypedPattern *P) {
    // We prefer to print the type separately.
    visit(P->getSubPattern());
  }
  void visitParenPattern(ParenPattern *P) {
    // Don't print parentheses: they break the correspondence with the
    // way we print the expression.
    visit(P->getSubPattern());
  }
};

static void REPLCheckPatternBinding(PatternBindingDecl *D, TypeChecker &TC) {
  ASTContext &Context = TC.Context;
  Expr *E = D->getInit();

  // FIXME: I'm assuming we don't need to bother printing the pattern binding
  // if it's a null initialization.
  if (!E)
    return;

  CanType T = E->getType()->getCanonicalType();
  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();

  SmallVector<ValueDecl*, 4> PrintDecls;
  TC.TU.lookupGlobalValue(Context.getIdentifier("print"),
                          NLKind::UnqualifiedLookup, PrintDecls);
  if (PrintDecls.empty())
    return;

  // Build function of type T->T which prints the operand.
  Type FuncTy = T;
  if (!isa<TupleType>(FuncTy)) {
    TupleTypeElt Elt(T, Context.getIdentifier("arg"));
    FuncTy = TupleType::get(Elt, Context);
  }
  FuncTy = FunctionType::get(FuncTy, FuncTy, Context);
  VarDecl *Arg = new (Context) VarDecl(Loc, Context.getIdentifier("arg"), T,
                                       nullptr);
  Pattern* ParamPat = new (Context) NamedPattern(Arg);
  FuncExpr *FE = FuncExpr::create(Context, Loc, ParamPat, FuncTy, 0, &TC.TU);
  Arg->setDeclContext(FE);
  
  // Fill in body of function.
  SmallVector<BraceStmt::ExprStmtOrDecl, 4> BodyContent;
  PatternBindingPrintLHS PatPrinter(BodyContent, PrintDecls, Loc, Context);
  PatPrinter.visit(D->getPattern());
  PrintLiteralString(" : ", Context, Loc, PrintDecls, BodyContent);
  auto TypeStr = Context.getIdentifier(E->getType()->getString()).str();
  PrintLiteralString(TypeStr, Context, Loc, PrintDecls, BodyContent);
  PrintLiteralString(" = ", Context, Loc, PrintDecls, BodyContent);
  SmallVector<unsigned, 4> MemberIndexes;
  PrintReplExpr(TC, Arg, T, Loc, EndLoc, MemberIndexes, BodyContent,
                PrintDecls);
  PrintLiteralString("\n", Context, Loc, PrintDecls, BodyContent);
  Expr *ArgRef = new (Context) DeclRefExpr(Arg, Loc, Arg->getTypeOfReference());
  BodyContent.push_back(new (Context) ReturnStmt(Loc, ArgRef));

  // Typecheck the function.
  BraceStmt *Body = BraceStmt::create(Context, Loc, BodyContent, EndLoc);
  StmtChecker(TC, FE).typeCheckStmt(Body);
  FE->setBody(Body);

  CallExpr *CE = new (Context) CallExpr(FE, E);
  D->setInit(TC.semaApplyExpr(CE));
}

/// \brief Check for circular inheritance of protocols.
///
/// \param Path The circular path through the protocol inheritance hierarchy,
/// which will be constructed (backwards) if there is in fact a circular path.
///
/// \returns True if there was circular inheritance, false otherwise.
bool checkProtocolCircularity(TypeChecker &TC, ProtocolDecl *Proto,
                              llvm::SmallPtrSet<ProtocolDecl *, 16> &Visited,
                              llvm::SmallPtrSet<ProtocolDecl *, 16> &Local,
                              llvm::SmallVectorImpl<ProtocolDecl *> &Path) {
  for (auto InheritedTy : Proto->getInherited()) {
    if (ProtocolType *InheritedProtoTy = InheritedTy->getAs<ProtocolType>()) {
      ProtocolDecl *InheritedProto = InheritedProtoTy->getDecl();
      if (Visited.count(InheritedProto)) {
        // We've seen this protocol as part of another protocol search;
        // it's not circular.
        continue;
      }

      // Whether we've seen this protocol before in our search or visiting it
      // detects a circularity, record it in the path and abort.
      if (!Local.insert(InheritedProto) ||
          checkProtocolCircularity(TC, InheritedProto, Visited, Local, Path)) {
        Path.push_back(InheritedProto);
        return true;
      }
    }
  }
  
  return false;
}

/// BindNameToIVar - We have an unresolved reference to an identifier in some
/// FuncDecl.  Check to see if this is a reference to an instance variable,
/// and return an AST for the reference if so.  If not, return null with no
/// error emitted.
static Expr *BindNameToIVar(UnresolvedDeclRefExpr *UDRE, Type ExtendedType,
                            ValueDecl *BaseDecl, TypeChecker &TC) {
  // Do a full "dot syntax" name lookup with the implicit 'this' base.
  MemberLookup Lookup(ExtendedType, UDRE->getName(), TC.TU);
  
  // On failure, this isn't an member reference.
  if (!Lookup.isSuccess()) return 0;
  
  // On success, this is a member reference. Build either a reference to the
  // implicit 'this' VarDecl (for instance methods) or to the metaclass
  // instance (for static methods).
  Expr *BaseExpr = new (TC.Context) DeclRefExpr(BaseDecl, SourceLoc(),
                                               BaseDecl->getTypeOfReference());
  
  return Lookup.createResultAST(BaseExpr, SourceLoc(), UDRE->getLoc(),
                                TC.Context);
}

/// BindName - Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression.  Context is the DeclContext used
/// for the lookup.
static Expr *BindName(UnresolvedDeclRefExpr *UDRE, DeclContext *Context,
                      TypeChecker &TC) {
  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!Context->isModuleContext()) {
    ValueDecl *BaseDecl = 0;
    Type ExtendedType;
    if (FuncExpr *FE = dyn_cast<FuncExpr>(Context)) {
      FuncDecl *FD = FE->getDecl();
      if (FD && FD->getExtensionType() && !FD->isStatic()) {
        ExtendedType = FD->getExtensionType();
        BaseDecl = FD->getImplicitThisDecl();
        Context = Context->getParent();
      }
    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(Context)) {
      ExtendedType = ED->getExtendedType();
      BaseDecl = ExtendedType->castTo<NominalType>()->getDecl();
    } else if (NominalTypeDecl *ND = dyn_cast<NominalTypeDecl>(Context)) {
      ExtendedType = ND->getDeclaredType();
      BaseDecl = ND;
    }

    if (BaseDecl)
      if (Expr *E = BindNameToIVar(UDRE, ExtendedType, BaseDecl, TC))
        return E;

    Context = Context->getParent();
  }

  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  Identifier Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();
  SmallVector<ValueDecl*, 4> Decls;
  // Perform standard value name lookup.
  TC.TU.lookupGlobalValue(Name, NLKind::UnqualifiedLookup, Decls);

  // If that fails, this may be the name of a module, try looking that up.
  if (Decls.empty()) {
    for (const auto &ImpEntry : TC.TU.getImportedModules())
      if (ImpEntry.second->Name == Name) {
        ModuleType *MT = ModuleType::get(ImpEntry.second);
        return new (TC.Context) ModuleExpr(Loc, MT);
      }
  }

  if (Decls.empty()) {
    TC.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return new (TC.Context) ErrorExpr(Loc);
  }

  return OverloadedDeclRefExpr::createWithCopy(Decls, Loc);
}

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU, unsigned StartElem) {
  TypeChecker TC(*TU);

  struct ExprPrePassWalker : private ASTWalker {
    TypeChecker &TC;

    ExprPrePassWalker(TypeChecker &TC) : TC(TC) {}
    
    /// CurDeclContexts - This is the stack of DeclContexts that
    /// we're nested in.
    SmallVector<DeclContext*, 4> CurDeclContexts;

    // FuncExprs - This is a list of all the FuncExprs we need to analyze, in
    // an appropriate order.
    SmallVector<FuncExpr*, 32> FuncExprs;

    virtual bool walkToDeclPre(Decl *D) {
      if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D))
        CurDeclContexts.push_back(NTD);
      else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D))
        CurDeclContexts.push_back(ED);

      if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
        // If this is an instance method with a body, set the type of it's
        // implicit 'this' variable.
        if (FD->getBody())
          if (Type ThisTy = FD->computeThisType()) {
            // References to the 'this' declaration will add back the lvalue
            // type as appropriate.
            if (LValueType *LValue = ThisTy->getAs<LValueType>())
              ThisTy = LValue->getObjectType();
            
            FD->getImplicitThisDecl()->setType(ThisTy);
          }
      }
      return true;
    }
    
    virtual bool walkToDeclPost(Decl *D) {
      if (isa<NominalTypeDecl>(D)) {
        assert(CurDeclContexts.back() == cast<NominalTypeDecl>(D) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      } else if (isa<ExtensionDecl>(D)) {
        assert(CurDeclContexts.back() == cast<ExtensionDecl>(D) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      }

      return true;
    }

    bool walkToExprPre(Expr *E) {
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprs.push_back(FE);

      if (CapturingExpr *CE = dyn_cast<CapturingExpr>(E))
        CurDeclContexts.push_back(CE);

      return true;
    }

    Expr *walkToExprPost(Expr *E) {
      if (UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
        return BindName(UDRE, CurDeclContexts.back(),
                        TC);
      }

      if (isa<CapturingExpr>(E)) {
        assert(CurDeclContexts.back() == cast<CapturingExpr>(E) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      }

      return E;
    }

    Expr *doWalk(Expr *E, DeclContext *DC) {
      CurDeclContexts.push_back(DC);
      E = E->walk(*this);
      CurDeclContexts.pop_back();
      return E;
    }

    void doWalk(Decl *D) {
      CurDeclContexts.push_back(D->getDeclContext());
      D->walk(*this);
      CurDeclContexts.pop_back();
    }
  };
  ExprPrePassWalker prePass(TC);

  if (TU->Kind != TranslationUnit::Library) {
    // FIXME: This loop is duplicated!
    for (auto TypeAndContext : TU->getTypesWithDefaultValues()) {
      TupleType *TT = TypeAndContext.first;
      for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
        const TupleTypeElt& Elt = TT->getFields()[i];
        if (Elt.hasInit()) {
          // Perform global name-binding etc. for all tuple default values.
          // FIXME: This screws up the FuncExprs list for FuncExprs in a
          // default value; conceptually, we should be appending to the list
          // in source order.
          Expr *Init = Elt.getInit();
          Init = prePass.doWalk(Init, TypeAndContext.second);
          TT->updateInitializedElementType(i, Elt.getType(), Init);
        }
      }
    }
  }

  // Type check the top-level elements of the translation unit.
  StmtChecker checker(TC, 0);
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    Decl *D = TU->Decls[i];
    if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      // Immediately perform global name-binding etc.
      prePass.doWalk(TLCD);

      // Tell the statement checker that we have top-level code.
      struct TopLevelCodeRAII {
        StmtChecker &Checker;
        TopLevelCodeDecl *Old;
        
      public:
        TopLevelCodeRAII(StmtChecker &Checker, TopLevelCodeDecl *TLC)
        : Checker(Checker), Old(Checker.TopLevelCode)
        {
          Checker.setTopLevelCode(TLC);
        }
        
        ~TopLevelCodeRAII() {
          Checker.setTopLevelCode(Old);
        }
      } IntroduceTopLevelCode(checker, TLCD);

      auto Elem = TLCD->getBody();
      if (Expr *E = Elem.dyn_cast<Expr*>()) {
        if (checker.typeCheckExpr(E)) continue;
        if (TU->Kind == TranslationUnit::Repl)
          TC.typeCheckTopLevelReplExpr(E, TLCD);
        else
          TC.typeCheckIgnoredExpr(E);
        TLCD->setBody(E);
      } else {
        Stmt *S = Elem.get<Stmt*>();
        if (checker.typeCheckStmt(S)) continue;
        TLCD->setBody(S);
      }
    } else {
      bool isFirstPass = TU->Kind == TranslationUnit::Library;
      if (!isFirstPass) {
        // If we aren't in a library, immediately perform global
        // name-binding etc.
        prePass.doWalk(D);
      }
      TC.typeCheckDecl(D, isFirstPass);
      if (TU->Kind == TranslationUnit::Repl && !TC.Context.hadError())
        if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D))
          REPLCheckPatternBinding(PBD, TC);
    }
  }

  // Check for explicit conformance to protocols and for circularity in
  // protocol definitions.
  {
    // FIXME: This check should be in TypeCheckDecl.
    llvm::SmallPtrSet<ProtocolDecl *, 16> VisitedProtocols;
    for (auto D : TU->Decls) {
      if (auto Protocol = dyn_cast<ProtocolDecl>(D)) {
        // Check for circular protocol definitions.
        llvm::SmallPtrSet<ProtocolDecl *, 16> LocalVisited;
        llvm::SmallVector<ProtocolDecl *, 4> Path;
        if (VisitedProtocols.count(Protocol) == 0) {
          LocalVisited.insert(Protocol);
          if (checkProtocolCircularity(TC, Protocol, VisitedProtocols,
                                       LocalVisited, Path)) {
            llvm::SmallString<128> PathStr;
            PathStr += "'";
            PathStr += Protocol->getName().str();
            PathStr += "'";
            for (unsigned I = Path.size(); I != 0; --I) {
              PathStr += " -> '";
              PathStr += Path[I-1]->getName().str();
              PathStr += "'";
            }
            
            TC.diagnose(Protocol->getLoc(), diag::circular_protocol_def,
                        PathStr);
            for (unsigned I = Path.size(); I != 1; --I) {
              TC.diagnose(Path[I-1]->getLoc(), diag::protocol_here,
                          Path[I-1]->getName());
            }
          }
          
          VisitedProtocols.insert(LocalVisited.begin(), LocalVisited.end());
        }
      }
    }
  }

  // If we're in a library, we don't know the types of all the global
  // declarations in the first pass, which means we can't completely analyze
  // everything. Perform the second pass now.
  if (TU->Kind == TranslationUnit::Library && !TC.Context.hadError()) {
    for (auto TypeAndContext : TU->getTypesWithDefaultValues()) {
      TupleType *TT = TypeAndContext.first;
      for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
        const TupleTypeElt& Elt = TT->getFields()[i];
        if (Elt.hasInit()) {
          // Perform global name-binding etc. for all tuple default values.
          // FIXME: This screws up the FuncExprs list for FuncExprs in a
          // default value; conceptually, we should be appending to the list
          // in source order.
          Expr *Init = Elt.getInit();
          Init = prePass.doWalk(Init, TypeAndContext.second);
          TT->updateInitializedElementType(i, Elt.getType(), Init);

          if (TT->hasCanonicalTypeComputed()) {
            // If we already examined a tuple in the first pass, we didn't
            // get a chance to type-check it; do that now.
            if (!TC.typeCheckExpression(Init, Elt.getType()))
              TT->updateInitializedElementType(i, Elt.getType(), Init);
          }
        }
      }
    }

    for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
      Decl *D = TU->Decls[i];
      prePass.doWalk(D);
      TC.typeCheckDecl(D, /*isFirstPass*/false);
    }
  }
  TU->clearTypesWithDefaultValues();

  // Check overloaded vars/funcs.
  // FIXME: This is quadratic time for TUs with multiple chunks.
  // FIXME: Can we make this more efficient?
  // FIXME: This check should be earlier to avoid ambiguous overload
  // errors etc.
  llvm::DenseMap<Identifier, TinyPtrVector<ValueDecl*>> CheckOverloads;
  for (unsigned i = 0, e = TU->Decls.size(); i != e; ++i) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(TU->Decls[i])) {
      // FIXME: I'm not sure this check is really correct.
      if (VD->getName().empty())
        continue;
      if (VD->getType()->is<ErrorType>() || VD->getType()->isUnresolvedType())
        continue;
      auto &PrevOv = CheckOverloads[VD->getName()];
      if (i >= StartElem) {
        for (ValueDecl *PrevD : PrevOv) {
          if (PrevD->getType()->isEqual(VD->getType())) {
            TC.diagnose(VD->getLocStart(), diag::invalid_redecl);
            TC.diagnose(PrevD->getLocStart(), diag::invalid_redecl_prev,
                        VD->getName());
          }
        }
      }
      PrevOv.push_back(VD);
    }
  }

  // Type check the body of each of the FuncExpr in turn.  Note that outside
  // FuncExprs must be visited before nested FuncExprs for type-checking to
  // work correctly.
  for (FuncExpr *FE : prePass.FuncExprs) {
    TC.semaFunctionSignature(FE);

    PrettyStackTraceExpr StackEntry(TC.Context, "type-checking", FE);

    BraceStmt *S = FE->getBody();
    StmtChecker(TC, FE).typeCheckStmt(S);
    FE->setBody(S);
  }

  // Verify that we've checked types correctly.
  TU->ASTStage = TranslationUnit::TypeChecked;
  verify(TU);
}
