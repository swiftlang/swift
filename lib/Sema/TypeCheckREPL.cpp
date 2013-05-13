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
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
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
  ArgRef = TC.coerceToRValue(ArgRef);
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
    // FIXME: We have to hack Slice into here, because replPrint on Slice isn't
    // implementable yet.  We don't want the T argument of the slice to be
    // constrained to being replPrintable.  We need replPrint to be more
    // dynamically reflective in its implementation.
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

Identifier TypeChecker::getNextResponseVariableName() {
  llvm::SmallString<4> namebuf;
  llvm::raw_svector_ostream names(namebuf);
  Identifier ident;
  
  bool nameUsed = false;
  do {
    names.flush();
    namebuf.clear();
    
    names << "r" << NextResponseVariableIndex++;
    
    ident = Context.getIdentifier(names.str());
    UnqualifiedLookup lookup(ident, &TU);
    nameUsed = lookup.isSuccess();
  } while (nameUsed);

  return ident;
}


static VarDecl *getObviousDECLFromExpr(Expr *E) {
  // Ignore lvalue->rvalue and other implicit conversions.
  while (auto *ICE = dyn_cast<ImplicitConversionExpr>(E))
    E = ICE->getSubExpr();

  // Don't bind REPL metavariables to simple declrefs.
  if (auto DRE = dyn_cast<DeclRefExpr>(E))
    return dyn_cast<VarDecl>(DRE->getDecl());
  return nullptr;
}


/// PatternBindingPrintLHS - This is a lot like Pattern::print, but prints
/// typed patterns and parenthesized patterns a bit differently.
namespace {
struct PatternBindingPrintLHS : public ASTVisitor<PatternBindingPrintLHS> {
  llvm::SmallString<16> &ResultString;

  PatternBindingPrintLHS(llvm::SmallString<16> &ResultString)
    : ResultString(ResultString) {}

  void visitTuplePattern(TuplePattern *P) {
    // We print tuples as "(x, y)".
    ResultString += "(";
    for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
      visit(P->getFields()[i].getPattern());
      if (i + 1 != e)
        ResultString += ", ";
    }
    ResultString += ")";
  }
  void visitNamedPattern(NamedPattern *P) {
    ResultString += P->getBoundName().str();
  }
  void visitAnyPattern(AnyPattern *P) {
    ResultString += "_";
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
} // end anonymous namespace.

/// generatePrintOfExpression - Emit logic to print the specified expression
/// value with the given description of the pattern involved.
static void generatePrintOfExpression(StringRef NameStr, Expr *E,
                                      TypeChecker *TC) {
  ASTContext &C = TC->Context;

  // Always print rvalues, not lvalues.
  E = TC->convertToMaterializable(E);

  CanType T = E->getType()->getCanonicalType();
  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();
  
  SmallVector<ValueDecl*, 4> PrintDecls;
  UnqualifiedLookup PrintDeclLookup(C.getIdentifier("print"), &TC->TU);
  if (!PrintDeclLookup.isSuccess())
    return;
  for (auto Result : PrintDeclLookup.Results)
    PrintDecls.push_back(Result.getValueDecl());
  
  // Build function of type T->() which prints the operand.
  VarDecl *Arg = new (C) VarDecl(Loc,TC->Context.getIdentifier("arg"),
                                 E->getType(), nullptr);
  Pattern *ParamPat = new (C) NamedPattern(Arg);
  ParamPat = new (C) TypedPattern(ParamPat,TypeLoc::withoutLoc(Arg->getType()));
  if (!isa<TupleType>(T)) {
    TuplePatternElt elt{ParamPat};
    ParamPat = TuplePattern::create(C, SourceLoc(), elt, SourceLoc());
  }
  TC->typeCheckPattern(ParamPat, /*isFirstPass*/false,
                       /*allowUnknownTypes*/false);
  FuncExpr *FE = FuncExpr::create(C, Loc, ParamPat, ParamPat, TypeLoc(),
                                  0, &TC->TU);
  Type FuncTy = FunctionType::get(ParamPat->getType(), TupleType::getEmpty(C),
                                  C);
  FE->setType(FuncTy);
  Arg->setDeclContext(FE);
  
  // Convert the pattern to a string we can print.
  llvm::SmallString<16> PrefixString;
  PrefixString += "// ";
  PrefixString += NameStr;
  PrefixString += " : ";
  PrefixString += E->getType()->getString();
  PrefixString += " = ";
  
  // Unique the type string into an identifier since PrintLiteralString is
  // building an AST around the string that must persist beyond the lifetime of
  // PrefixString.
  auto TmpStr = C.getIdentifier(PrefixString).str();
  
  // Fill in body of function.  Start with the string prefix to print.
  SmallVector<BraceStmt::ExprStmtOrDecl, 4> BodyContent;
  PrintLiteralString(TmpStr, *TC, Loc, PrintDecls, BodyContent);
  
  SmallVector<unsigned, 4> MemberIndexes;
  PrintReplExpr(*TC, Arg, E->getType(), T, Loc, EndLoc, MemberIndexes,
                BodyContent, PrintDecls, FE);
  PrintLiteralString("\n", *TC, Loc, PrintDecls, BodyContent);
  
  // Typecheck the function.
  BraceStmt *Body = BraceStmt::create(C, Loc, BodyContent, EndLoc);
  FE->setBody(Body);
  TC->typeCheckFunctionBody(FE);
  
  Expr *TheCall = TC->semaApplyExpr(new (C) CallExpr(FE, E));
  
  // Inject the call into the top level stream by wrapping it with a TLCD.
  auto *BS = BraceStmt::create(C, Loc, BraceStmt::ExprStmtOrDecl(TheCall),
                               EndLoc);
  TC->TU.Decls.push_back(new (C) TopLevelCodeDecl(&TC->TU, BS));
}



/// processREPLTopLevelExpr - When we see an Expression in a TopLevelCodeDecl
/// in the REPL, process it, adding the proper decls back to the top level of
/// the TranslationUnit.
static void processREPLTopLevelExpr(Expr *E, TypeChecker *TC) {
  CanType T = E->getType()->getCanonicalType();
  
  // Don't try to print invalid expressions, module exprs, or void expressions.
  if (isa<ErrorType>(T) || isa<ModuleType>(T) ||
      (isa<TupleType>(T) && cast<TupleType>(T)->getFields().empty()))
    return;
  
  // Okay, we need to print this expression.  We generally do this by creating a
  // REPL metavariable (e.g. r4) to hold the result, so it can be referred to
  // in the future.  However, if this is a direct reference to a decl (e.g. "x")
  // then don't create a repl metavariable.
  if (VarDecl *d = getObviousDECLFromExpr(E)) {
    generatePrintOfExpression(d->getName().str(), E, TC);
    return;
  }
  
  // Remove the expression from being in the list of decls to execute, we're
  // going to reparent it.
  TC->TU.Decls.pop_back();

  E = TC->convertToMaterializable(E);

  // Create the meta-variable, let the typechecker name it.
  VarDecl *vd = new (TC->Context) VarDecl(E->getStartLoc(),
                                          TC->getNextResponseVariableName(),
                                          E->getType(), &TC->TU);
  TC->TU.Decls.push_back(vd);
  
  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (TC->Context) NamedPattern(vd);
  metavarPat->setType(E->getType());
  PatternBindingDecl *metavarBinding
    = new (TC->Context) PatternBindingDecl(E->getStartLoc(), metavarPat, E,
                                           &TC->TU);
  TC->TU.Decls.push_back(metavarBinding);

  // Finally, print the variable's value.
  E = new (TC->Context) DeclRefExpr(vd, E->getStartLoc(),
                                    vd->getTypeOfReference());
  generatePrintOfExpression(vd->getName().str(), E, TC);
}

/// processREPLTopLevelPatternBinding - When we see a new PatternBinding parsed
/// into the REPL, process it by generating code to print it out.
static void processREPLTopLevelPatternBinding(PatternBindingDecl *PBD,
                                              TypeChecker *TC) {
  llvm::SmallString<16> PatternString;
  PatternBindingPrintLHS(PatternString).visit(PBD->getPattern());
  
  // If the bound pattern is a single value, use a DeclRefExpr on the underlying
  // Decl to print it.
  if (auto *NP = dyn_cast<NamedPattern>(PBD->getPattern()->
                                           getSemanticsProvidingPattern())) {
    Expr *E = new (TC->Context) DeclRefExpr(NP->getDecl(), PBD->getStartLoc(),
                                            NP->getDecl()->getTypeOfReference());
    generatePrintOfExpression(PatternString, E, TC);
    return;
  }

  // Otherwise, we may not have a way to name all of the pieces of the pattern.
  // Create a repl metavariable to capture the whole thing so we can reference
  // it, then assign that into the pattern.  For example, translate:
  //   var (x, y, _) = foo()
  // into:
  //   var r123 = foo()
  //   var (x, y, _) = r123
  //   replPrint(r123)
  
  // Remove PBD from the list of Decls so we can insert before it.
  TopLevelCodeDecl *PBTLCD = cast<TopLevelCodeDecl>(TC->TU.Decls.back());
  TC->TU.Decls.pop_back();

  // Create the meta-variable, let the typechecker name it.
  VarDecl *vd = new (TC->Context) VarDecl(PBD->getStartLoc(),
                                          TC->getNextResponseVariableName(),
                                          PBD->getPattern()->getType(),&TC->TU);
  TC->TU.Decls.push_back(vd);

  
  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (TC->Context) NamedPattern(vd);
  metavarPat->setType(vd->getType());
  PatternBindingDecl *metavarBinding
    = new (TC->Context) PatternBindingDecl(PBD->getStartLoc(), metavarPat,
                                           PBD->getInit(), &TC->TU);

  auto MVBrace = BraceStmt::create(TC->Context, metavarBinding->getStartLoc(),
                                   BraceStmt::ExprStmtOrDecl(metavarBinding),
                                   metavarBinding->getEndLoc());

  auto *MVTLCD = new (TC->Context) TopLevelCodeDecl(&TC->TU, MVBrace);
  TC->TU.Decls.push_back(MVTLCD);

  
  // Replace the initializer of PBD with a reference to our repl temporary.
  Expr *E = new (TC->Context) DeclRefExpr(vd, vd->getStartLoc(),
                                          vd->getTypeOfReference());
  E = TC->convertToMaterializable(E);
  PBD->setInit(E);
  TC->TU.Decls.push_back(PBTLCD);

  // Finally, print out the result, by referring to the repl temp.
  E = new (TC->Context) DeclRefExpr(vd, vd->getStartLoc(),
                                    vd->getTypeOfReference());
  generatePrintOfExpression(PatternString, E, TC);
}



/// processREPLTopLevel - This is called after we've parsed and typechecked some
/// new decls at the top level.  We inject code to print out expressions and
/// pattern bindings the are evaluated.
void TypeChecker::processREPLTopLevel(unsigned FirstDecl) {
  // Loop over all of the new decls, moving them out of the Decls list, then
  // adding them back (with modifications) one at a time.
  std::vector<Decl*> NewDecls(TU.Decls.begin()+FirstDecl, TU.Decls.end());
  TU.Decls.resize(FirstDecl);
  
  // Loop over each of the new decls, processing them, adding them back to
  // the TU->Decls list.
  for (Decl *D : NewDecls) {
    TU.Decls.push_back(D);
    
    TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D);
    if (TLCD == 0) continue;

    auto Entry = TLCD->getBody()->getElements()[0];

    // Check to see if the TLCD has an expression that we have to transform.
    if (Expr *E = Entry.dyn_cast<Expr*>())
      processREPLTopLevelExpr(E, this);
    else if (Decl *D = Entry.dyn_cast<Decl*>())
      if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D))
        processREPLTopLevelPatternBinding(PBD, this);
  }
}

