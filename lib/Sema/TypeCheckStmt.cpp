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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/PointerUnion.h"
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
  
  StmtChecker(TypeChecker &TC, FuncExpr *TheFunc) : TC(TC), TheFunc(TheFunc) {
  }

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

    Expr *E = RS->getResult();
    if (typeCheckExpr(E, TheFunc->getBodyResultType()))
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
    // Type check the condition if present.
    if (FS->getCond().isNonNull()) {
      Expr *E = FS->getCond().get();
      if (TC.typeCheckCondition(E)) return 0;
      FS->setCond(E);
    }
    
    PointerUnion<Expr*, AssignStmt*> Tmp = FS->getInitializer();
    if (typeCheck(Tmp)) return 0;
    FS->setInitializer(Tmp);
    
    Tmp = FS->getIncrement();
    if (typeCheck(Tmp)) return 0;
    FS->setIncrement(Tmp);
    
    Stmt *S = FS->getBody();
    if (typeCheckStmt(S)) return 0;
    FS->setBody(S);
    
    return FS;
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
      Decl *D = BS->getElement(i).get<Decl*>();
      TC.typeCheckDecl(D);
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
              SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent) {
  ASTContext &Context = TC.Context;
  TranslationUnit &TU = TC.TU;

  // Lookup the "print" function used for strings.
  SmallVector<ValueDecl*, 4> PrintDecls;
  TU.lookupGlobalValue(Context.getIdentifier("print"),
                       NLKind::UnqualifiedLookup, PrintDecls);

  if (TupleType *TT = dyn_cast<TupleType>(T)) {
    // We print a tuple by printing each element.
    PrintLiteralString("(", Context, Loc, PrintDecls, BodyContent);

    for (unsigned i = 0, e = TT->getFields().size(); i < e; ++i) {
      MemberIndexes.push_back(i);
      CanType SubType = TT->getElementType(i)->getCanonicalType();
      PrintReplExpr(TC, Arg, SubType, Loc, EndLoc, MemberIndexes,
                    BodyContent);
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
      // For each index, we look through a TupleType or transparent OneOfType.
      CanType CurT = ArgRef->getType()->getCanonicalType();
      if (OneOfType *OOT = dyn_cast<OneOfType>(CurT)) {
        CurT = OOT->getTransparentType()->getCanonicalType();
        ArgRef = new (Context) LookThroughOneofExpr(ArgRef, CurT);
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

  if (OneOfType *OOT = dyn_cast<OneOfType>(T)) {
    if (OOT->isTransparentType()) {
      // Print "struct" types as if we are constructing one: the name
      // followed by the underlying tuple.
      PrintLiteralString(OOT->getDecl()->getName().str(), Context, Loc,
                         PrintDecls, BodyContent);
      CanType SubType = OOT->getTransparentType()->getCanonicalType();
      PrintReplExpr(TC, Arg, SubType, Loc, EndLoc,
                    MemberIndexes, BodyContent);
      return;
    }

    // FIXME: We should handle non-transparent OneOfTypes at some point, but
    // it's tricky to represent in the AST without a "match" statement.
  }

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

  // Printing format is "Int = 0\n".
  auto TypeStr = Context.getIdentifier(E->getType()->getString()).str();
  PrintLiteralString(TypeStr, Context, Loc, PrintDecls, BodyContent);
  PrintLiteralString(" = ", Context, Loc, PrintDecls, BodyContent);
  PrintReplExpr(*this, Arg, T, Loc, EndLoc, MemberIndexes, BodyContent);
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
  PrintReplExpr(TC, Arg, T, Loc, EndLoc, MemberIndexes, BodyContent);
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

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU, unsigned StartElem) {
  TypeChecker TC(*TU);
  
  // Find all the FuncExprs in the translation unit and collapse all
  // the sequences.
  struct PrePassWalker : ASTWalker {
    TypeChecker &TC;
    SmallVector<FuncExpr*, 32> FuncExprs;

    PrePassWalker(TypeChecker &TC) : TC(TC) {}

    bool walkToExprPre(Expr *E) {
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprs.push_back(FE);
      return true;
    }

    Expr *walkToExprPost(Expr *E) {
      if (SequenceExpr *SE = dyn_cast<SequenceExpr>(E))
        return TC.foldSequence(SE);
      return E;
    }
  };
  PrePassWalker prePass(TC);
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i)
    TU->Decls[i]->walk(prePass);

  // Type check the top-level elements of the translation unit.
  StmtChecker checker(TC, 0);
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    Decl *D = TU->Decls[i];
    if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      auto Elem = TLCD->getBody();
      if (Expr *E = Elem.dyn_cast<Expr*>()) {
        if (checker.typeCheckExpr(E)) continue;
        if (TU->IsReplModule)
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
      TC.typeCheckDecl(D);
      if (TU->IsReplModule)
        if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D))
          REPLCheckPatternBinding(PBD, TC);
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
