//===--- TypeCheckREPL.cpp - Type Checking for the REPL -----------------===//
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
// This file implements REPL-specific semantic analysis rules.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Expr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Stmt.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

void
PrintLiteralString(StringRef Str, TypeChecker &TC, SourceLoc Loc,
                   SmallVectorImpl<ValueDecl*> &PrintDecls,
                   SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent) {
  ASTContext &Context = TC.Context;
  Expr *PrintStr = new (Context) StringLiteralExpr(Str, Loc);
  Expr *PrintStrFn = TC.buildRefExpr(PrintDecls, Loc);
  BodyContent.push_back(new (Context) CallExpr(PrintStrFn, PrintStr));
}

static void
PrintReplExpr(TypeChecker &TC, VarDecl *Arg,
              Type SugarT, CanType T,
              SourceLoc Loc, SourceLoc EndLoc,
              SmallVectorImpl<unsigned> &MemberIndexes,
              SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent,
              SmallVectorImpl<ValueDecl*> &PrintDecls,
              DeclContext *DC);

static void
PrintStruct(TypeChecker &TC, VarDecl *Arg,
            Type SugarT, StructDecl *SD,
            SourceLoc Loc, SourceLoc EndLoc,
            SmallVectorImpl<unsigned> &MemberIndexes,
            SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent,
            SmallVectorImpl<ValueDecl*> &PrintDecls,
            DeclContext *DC) {
  auto TypeStr
    = TC.Context.getIdentifier(SugarT->getString()).str();
  PrintLiteralString(TypeStr, TC, Loc, PrintDecls, BodyContent);
  
  PrintLiteralString("(", TC, Loc, PrintDecls, BodyContent);
  
  unsigned idx = 0;
  bool isFirstMember = true;
  for (Decl *D : SD->getMembers()) {
    if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
      if (!VD->isProperty()) {
        if (isFirstMember)
          isFirstMember = false;
        else
          PrintLiteralString(", ", TC, Loc, PrintDecls, BodyContent);
        
        MemberIndexes.push_back(idx);
        CanType SubType = VD->getType()->getCanonicalType();
        PrintReplExpr(TC, Arg, VD->getType(), SubType,
                      Loc, EndLoc, MemberIndexes,
                      BodyContent, PrintDecls, DC);
        MemberIndexes.pop_back();
      }
    }
    ++idx;
  }
  PrintLiteralString(")", TC, Loc, PrintDecls, BodyContent);
}

static Expr *
getArgRefExpr(TypeChecker &TC,
              VarDecl *Arg,
              ArrayRef<unsigned> MemberIndexes,
              SourceLoc Loc) {
  ASTContext &Context = TC.Context;

  Expr *ArgRef = new (Context) DeclRefExpr(Arg, Loc,
                                           Arg->getTypeOfReference());
  ArgRef = TC.convertToRValue(ArgRef);
  for (unsigned i : MemberIndexes) {
    // For each index, we look through a TupleType or StructType.
    CanType CurT = ArgRef->getType()->getRValueType()->getCanonicalType();
    if (StructType *ST = dyn_cast<StructType>(CurT)) {
      VarDecl *VD = cast<VarDecl>(ST->getDecl()->getMembers()[i]);
      ArgRef = new (Context) MemberRefExpr(ArgRef, Loc, VD, Loc);
      ArgRef = TC.recheckTypes(ArgRef);
      continue;
    }
    if (BoundGenericStructType *BGST = dyn_cast<BoundGenericStructType>(CurT)) {
      VarDecl *VD = cast<VarDecl>(BGST->getDecl()->getMembers()[i]);
      ArgRef = new (Context) GenericMemberRefExpr(ArgRef, Loc, VD, Loc);
      ArgRef = TC.recheckTypes(ArgRef);
      continue;
    }
    TupleType *TT = cast<TupleType>(CurT);
    ArgRef = new (Context) TupleElementExpr(ArgRef, Loc, i, Loc,
                                            TT->getElementType(i));
  }
  return ArgRef;
}

static void
PrintClass(TypeChecker &TC, VarDecl *Arg,
           Type SugarT, ClassDecl *CD,
           SourceLoc Loc, SourceLoc EndLoc,
           SmallVectorImpl<unsigned> &MemberIndexes,
           SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent,
           SmallVectorImpl<ValueDecl*> &PrintDecls) {

  ASTContext &Context = TC.Context;
  TranslationUnit &TU = TC.TU;

  PrintLiteralString("<", TC, Loc, PrintDecls, BodyContent);
  
  // If the metatype has a className() property, use it to display the dynamic
  // type of the instance.
  bool showedDynamicType = false;
  Identifier MemberName = Context.getIdentifier("className");
  Type MetaT = MetaTypeType::get(SugarT, Context);
  MemberLookup Lookup(MetaT, MemberName, TU);
  
  if (Lookup.isSuccess()) {
    Expr *ArgRef = getArgRefExpr(TC, Arg, MemberIndexes, Loc);
    MetatypeExpr *Meta = new (Context) MetatypeExpr(ArgRef,
                                                    Loc,
                                                    MetaT);
    Expr *Res = TC.recheckTypes(TC.buildMemberRefExpr(Meta, Loc, Lookup,
                                                      EndLoc));
    if (!Res)
      return;
    TupleExpr *CallArgs =
      new (Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, EndLoc,
                              TupleType::getEmpty(Context));
    CallExpr *CE = new (Context) CallExpr(Res, CallArgs, Type());
    Res = TC.semaApplyExpr(CE);
    if (!Res)
      goto dynamicTypeFailed;
    
    Expr *PrintStrFn = TC.buildRefExpr(PrintDecls, Loc);
    Res = TC.semaApplyExpr(new (Context) CallExpr(PrintStrFn, Res));
    if (!Res)
      goto dynamicTypeFailed;
    
    BodyContent.push_back(Res);
    showedDynamicType = true;
  }

dynamicTypeFailed:
  if (!showedDynamicType) {
    auto TypeStr
      = TC.Context.getIdentifier(SugarT->getString()).str();
    PrintLiteralString(TypeStr, TC, Loc, PrintDecls, BodyContent);
  }
  
  PrintLiteralString(" instance>", TC, Loc, PrintDecls, BodyContent);
}

/// \brief Print a slice.
static void
PrintSlice(TypeChecker &TC, VarDecl *Arg, Type ElementTy,
           SourceLoc Loc, SourceLoc EndLoc,
           SmallVectorImpl<unsigned> &MemberIndexes,
           SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent,
           SmallVectorImpl<ValueDecl*> &PrintDecls,
           DeclContext *DC) {
  ASTContext &context = TC.Context;

  // Dig up Bool, true, and false. We'll need them.
  UnqualifiedLookup lookupBool(context.getIdentifier("Bool"), &TC.TU);
  auto boolDecl = lookupBool.getSingleTypeResult();
  UnqualifiedLookup lookupTrue(context.getIdentifier("true"), &TC.TU);
  auto trueDecl = lookupTrue.isSuccess()
                    ? dyn_cast<VarDecl>(lookupTrue.Results[0].getValueDecl())
                    : nullptr;
  UnqualifiedLookup lookupFalse(context.getIdentifier("false"), &TC.TU);
  auto falseDecl = lookupFalse.isSuccess()
                     ? dyn_cast<VarDecl>(lookupFalse.Results[0].getValueDecl())
                     : nullptr;

  // If we have Bool/true/false, create the declaration "first" to capture the
  // first walk through the loop and initialize it to "true".
  VarDecl *firstVar = nullptr;
  if (boolDecl && trueDecl && falseDecl) {
    auto boolTy = boolDecl->getDeclaredType();
    
    firstVar = new (context) VarDecl(Loc, context.getIdentifier("first"),
                                     boolDecl->getDeclaredType(), DC);
    Pattern *pattern = new (context) NamedPattern(firstVar);
    pattern->setType(boolTy);
    pattern = new (context) TypedPattern(pattern, TypeLoc::withoutLoc(boolTy));
    pattern->setType(boolTy);

    Expr *init = new (context) DeclRefExpr(trueDecl, Loc,
                                           trueDecl->getTypeOfReference());
    BodyContent.push_back(new (context) PatternBindingDecl(Loc, pattern, init,
                                                           &TC.TU));
  }

  // Add opening bracket '['.
  PrintLiteralString("[", TC, Loc, PrintDecls, BodyContent);

  // Create the pattern for a variable "elt".
  auto elementVar = new (context) VarDecl(Loc, context.getIdentifier("elt"),
                                          ElementTy, DC);
  Pattern *pattern = new (context) NamedPattern(elementVar);
  pattern->setType(ElementTy);
  pattern = new (context) TypedPattern(pattern, TypeLoc::withoutLoc(ElementTy));
  pattern->setType(ElementTy);

  // Construct the loop body.
  SmallVector<BraceStmt::ExprStmtOrDecl, 4> loopBodyContents;

  // First, print the ", " between elements.
  if (firstVar) {
    // if branch: set first to false
    Expr *firstRef = new (context) DeclRefExpr(firstVar, Loc,
                                               firstVar->getTypeOfReference());
    Expr *falseRef = new (context) DeclRefExpr(falseDecl, Loc,
                                               falseDecl->getTypeOfReference());
    Stmt *setFirstToFalse
      = new (context) AssignStmt(firstRef, Loc, falseRef);

    // else branch: print a comma.
    SmallVector<BraceStmt::ExprStmtOrDecl, 4> elseBodyContents;
    PrintLiteralString(", ", TC, Loc, PrintDecls, elseBodyContents);
    Stmt *elseStmt = BraceStmt::create(context, Loc, elseBodyContents, Loc);

    // if-then-else statement.
    firstRef = new (context) DeclRefExpr(firstVar, Loc,
                                         firstVar->getTypeOfReference());
    loopBodyContents.push_back(new (context) IfStmt(Loc, firstRef,
                                                    setFirstToFalse,
                                                    Loc, elseStmt));
  } else {
    PrintLiteralString(", ", TC, Loc, PrintDecls, loopBodyContents);
  }

  // Print the value
  PrintReplExpr(TC, elementVar, ElementTy, ElementTy->getCanonicalType(), Loc,
                EndLoc, MemberIndexes, loopBodyContents, PrintDecls, DC);

  auto loopBody = BraceStmt::create(context, Loc, loopBodyContents, EndLoc);

  // Construct the loop.
  Expr *argRef = new (context) DeclRefExpr(Arg, Loc, Arg->getTypeOfReference());
  BodyContent.push_back(new (context) ForEachStmt(Loc, pattern, Loc, argRef,
                                                  loopBody));

  // Add closing bracket ']'.
  PrintLiteralString("]", TC, EndLoc, PrintDecls, BodyContent);
}

static void
PrintReplExpr(TypeChecker &TC, VarDecl *Arg,
              Type SugarT, CanType T,
              SourceLoc Loc, SourceLoc EndLoc,
              SmallVectorImpl<unsigned> &MemberIndexes,
              SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent,
              SmallVectorImpl<ValueDecl*> &PrintDecls,
              DeclContext *DC) {
  ASTContext &Context = TC.Context;
  TranslationUnit &TU = TC.TU;

  if (TupleType *TT = dyn_cast<TupleType>(T)) {
    // We print a tuple by printing each element.
    PrintLiteralString("(", TC, Loc, PrintDecls, BodyContent);

    for (unsigned i = 0, e = TT->getFields().size(); i < e; ++i) {
      MemberIndexes.push_back(i);
      CanType SubType = TT->getElementType(i)->getCanonicalType();
      PrintReplExpr(TC, Arg, TT->getElementType(i), SubType, Loc, EndLoc,
                    MemberIndexes, BodyContent, PrintDecls, DC);
      MemberIndexes.pop_back();

      if (i + 1 != e)
        PrintLiteralString(", ", TC, Loc, PrintDecls, BodyContent);
    }

    PrintLiteralString(")", TC, Loc, PrintDecls, BodyContent);
    return;
  }

  Identifier MemberName = Context.getIdentifier("replPrint");
  MemberLookup Lookup(T, MemberName, TU);

  // Don't try to use an instance method to print a metatype. Filter out
  // instance members if our expression evaluated to a metatype.
  if (T->is<MetaTypeType>()) {
    auto staticEnd = std::remove_if(
                        Lookup.Results.begin(), Lookup.Results.end(),
                        [](MemberLookupResult const &r) {
                          return r.Kind != MemberLookupResult::MetatypeMember;
                        });
    Lookup.Results.erase(staticEnd, Lookup.Results.end());
  }
  
  if (Lookup.isSuccess()) {
    Expr *ArgRef = getArgRefExpr(TC, Arg, MemberIndexes, Loc);
    Expr *Res = TC.recheckTypes(TC.buildMemberRefExpr(ArgRef, Loc, Lookup,
                                                      EndLoc));
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

  // We print a struct by printing each non-property member variable.
  if (StructType *ST = dyn_cast<StructType>(T)) {
    PrintStruct(TC, Arg, SugarT, ST->getDecl(), Loc, EndLoc,
                MemberIndexes, BodyContent, PrintDecls, DC);
    return;
  }
  
  if (BoundGenericStructType *BGST = dyn_cast<BoundGenericStructType>(T)) {
    if (!BGST->getParent() && BGST->getDecl()->getName().str() == "Slice") {
      PrintSlice(TC, Arg, BGST->getGenericArgs()[0], Loc, EndLoc, MemberIndexes,
                 BodyContent, PrintDecls, DC);
      return;
    }

    PrintStruct(TC, Arg, SugarT, BGST->getDecl(), Loc, EndLoc,
                MemberIndexes, BodyContent, PrintDecls, DC);
    return;
  }

  if (ClassType *CT = dyn_cast<ClassType>(T)) {
    PrintClass(TC, Arg, SugarT, CT->getDecl(), Loc, EndLoc,
               MemberIndexes, BodyContent, PrintDecls);
    return;
  }

  
  // FIXME: We should handle OneOfTypes at some point, but
  // it's tricky to represent in the AST without a "match" statement.

  PrintLiteralString("<unprintable value>", TC, Loc, PrintDecls,
                     BodyContent);
}

/// If an expression consists only of a DeclRef (with potential implicit
/// conversions such as Load/Materialize), returns the name of the referenced
/// declaration. Otherwise, returns an empty StringRef.
static llvm::StringRef getDisplayNameOfExpr(Expr *E) {
  while (auto *ICE = dyn_cast<ImplicitConversionExpr>(E))
    E = ICE->getSubExpr();
  if (auto *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl()->getName().str();
  return {};
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

  // Skip printing for expressions of void or module type.
  if (isa<TupleType>(T) && cast<TupleType>(T)->getFields().empty())
    return;
  if (isa<ModuleType>(T))
    return;

  // Build a function to call to print the expression.
  VarDecl *Arg = new (Context) VarDecl(Loc, Context.getIdentifier("arg"), T,
                                       nullptr);
  Pattern* ParamPat = new (Context) NamedPattern(Arg);
  ParamPat = new (Context) TypedPattern(ParamPat,
                                        TypeLoc::withoutLoc(Arg->getType()));
  if (!isa<TupleType>(T)) {
    TuplePatternElt elt{ParamPat};
    ParamPat = TuplePattern::create(Context, SourceLoc(), elt, SourceLoc());
  }
  typeCheckPattern(ParamPat, /*isFirstPass*/false, /*allowUnknownTypes*/false);
  FuncExpr *FE = FuncExpr::create(Context, Loc, ParamPat, ParamPat, TypeLoc(),
                                  0, TLCD);
  Type FuncTy = FunctionType::get(ParamPat->getType(),
                                  TupleType::getEmpty(Context), Context);
  FE->setType(FuncTy);
  Arg->setDeclContext(FE);

  // Build the body of the function which prints the expression.
  SmallVector<unsigned, 4> MemberIndexes;
  SmallVector<BraceStmt::ExprStmtOrDecl, 4> BodyContent;
  SmallVector<ValueDecl*, 4> PrintDecls;
  UnqualifiedLookup PrintDeclLookup(Context.getIdentifier("print"), &TU);
  if (!PrintDeclLookup.isSuccess())
    return;
  for (auto Result : PrintDeclLookup.Results)
    PrintDecls.push_back(Result.getValueDecl());

  // Printing format is:
  //   "// Int = 0\n"        for an anonymous expression, or
  //   "// name : Int = 0\n" for an expression bound to a variable 'name'.
  
  auto NameStr = getDisplayNameOfExpr(E);
  // Use an Identifier since PrintLiteralString is building an AST around the
  // string that must persist beyond the lifetime of getString().
  auto TypeStr = Context.getIdentifier(E->getType()->getString()).str();
  PrintLiteralString("// ", *this, Loc, PrintDecls, BodyContent);
  if (!NameStr.empty()) {
    PrintLiteralString(NameStr, *this, Loc, PrintDecls, BodyContent);
    PrintLiteralString(" : ", *this, Loc, PrintDecls, BodyContent);
  }
  PrintLiteralString(TypeStr, *this, Loc, PrintDecls, BodyContent);
  PrintLiteralString(" = ", *this, Loc, PrintDecls, BodyContent);
  PrintReplExpr(*this, Arg, E->getType(), T, Loc, EndLoc,
                MemberIndexes, BodyContent, PrintDecls, FE);
  PrintLiteralString("\n", *this, Loc, PrintDecls, BodyContent);

  // Typecheck the function.
  BraceStmt *Body = BraceStmt::create(Context, Loc, BodyContent, EndLoc);
  FE->setBody(Body);
  typeCheckFunctionBody(FE);

  // Typecheck the call.
  CallExpr *CE = new (Context) CallExpr(FE, E);
  E = semaApplyExpr(CE);
}

struct PatternBindingPrintLHS : public ASTVisitor<PatternBindingPrintLHS> {
  SmallVectorImpl<BraceStmt::ExprStmtOrDecl> &BodyContent;
  SmallVectorImpl<ValueDecl*> &PrintDecls;
  SourceLoc Loc;
  TypeChecker &TC;
  ASTContext &Context;

  PatternBindingPrintLHS(SmallVectorImpl<BraceStmt::ExprStmtOrDecl>&BodyContent,
                         SmallVectorImpl<ValueDecl*> &PrintDecls,
                         SourceLoc Loc,
                         TypeChecker &TC)
    : BodyContent(BodyContent), PrintDecls(PrintDecls), Loc(Loc),
      TC(TC), Context(TC.Context) {}

  void print(StringRef Str) {
    PrintLiteralString(Str, TC, Loc, PrintDecls, BodyContent);
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

static bool isREPLResultPattern(PatternBindingDecl *D) {
  NamedPattern *named = dyn_cast<NamedPattern>(D->getPattern());
  if (!named)
    return false;
  return named->getDecl()->isREPLResult();
}

void TypeChecker::REPLCheckPatternBinding(PatternBindingDecl *D) {
  Expr *E = D->getInit();

  // Don't print the binding if it's a REPL result binding of void type.
  if (isREPLResultPattern(D) &&
      E->getType()->isEqual(TupleType::getEmpty(D->getASTContext())))
    return;
  
  // FIXME: I'm assuming we don't need to bother printing the pattern binding
  // if it's a null initialization.
  if (!E)
    return;

  CanType T = E->getType()->getCanonicalType();
  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();

  SmallVector<ValueDecl*, 4> PrintDecls;
  UnqualifiedLookup PrintDeclLookup(Context.getIdentifier("print"), &TU);
  if (!PrintDeclLookup.isSuccess())
    return;
  for (auto Result : PrintDeclLookup.Results)
    PrintDecls.push_back(Result.getValueDecl());

  // Build function of type T->T which prints the operand.
  VarDecl *Arg = new (Context) VarDecl(Loc, Context.getIdentifier("arg"), T,
                                       nullptr);
  Pattern* ParamPat = new (Context) NamedPattern(Arg);
  ParamPat = new (Context) TypedPattern(ParamPat,
                                        TypeLoc::withoutLoc(Arg->getType()));
  if (!isa<TupleType>(T)) {
    TuplePatternElt elt{ParamPat};
    ParamPat = TuplePattern::create(Context, SourceLoc(), elt, SourceLoc());
  }
  typeCheckPattern(ParamPat, /*isFirstPass*/false, /*allowUnknownTypes*/false);
  FuncExpr *FE = FuncExpr::create(Context, Loc,
                                  ParamPat, ParamPat, TypeLoc(),
                                  0, &TU);
  Type FuncTy = FunctionType::get(ParamPat->getType(), T, Context);
  FE->setType(FuncTy);
  Arg->setDeclContext(FE);
  
  // Fill in body of function.
  SmallVector<BraceStmt::ExprStmtOrDecl, 4> BodyContent;
  PrintLiteralString("// ", *this, Loc, PrintDecls, BodyContent);
  PatternBindingPrintLHS PatPrinter(BodyContent, PrintDecls, Loc, *this);
  PatPrinter.visit(D->getPattern());
  PrintLiteralString(" : ", *this, Loc, PrintDecls, BodyContent);
  
  // Unique the type string into an identifier since PrintLiteralString is
  // building an AST around the string that must persist beyond the lifetime of
  // getString().
  auto TypeStr = Context.getIdentifier(E->getType()->getString()).str();
  PrintLiteralString(TypeStr, *this, Loc, PrintDecls, BodyContent);
  PrintLiteralString(" = ", *this, Loc, PrintDecls, BodyContent);
  SmallVector<unsigned, 4> MemberIndexes;
  PrintReplExpr(*this, Arg, E->getType(), T, Loc, EndLoc, MemberIndexes,
                BodyContent, PrintDecls, FE);
  PrintLiteralString("\n", *this, Loc, PrintDecls, BodyContent);
  Expr *ArgRef = new (Context) DeclRefExpr(Arg, Loc, Arg->getTypeOfReference());
  BodyContent.push_back(new (Context) ReturnStmt(Loc, ArgRef));

  // Typecheck the function.
  BraceStmt *Body = BraceStmt::create(Context, Loc, BodyContent, EndLoc);
  FE->setBody(Body);
  typeCheckFunctionBody(FE);

  CallExpr *CE = new (Context) CallExpr(FE, E);
  D->setInit(semaApplyExpr(CE));
}
