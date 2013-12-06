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

static void
PrintLiteralString(StringRef Str, TypeChecker &TC, SourceLoc Loc,
                   SmallVectorImpl<ValueDecl*> &PrintDecls,
                   SmallVectorImpl<ASTNode> &BodyContent) {
  ASTContext &Context = TC.Context;
  Expr *PrintStr = new (Context) StringLiteralExpr(Str, Loc);
  Expr *PrintStrFn = TC.buildRefExpr(PrintDecls, Loc, /*Implicit=*/true);
  BodyContent.push_back(new (Context) CallExpr(PrintStrFn, PrintStr,
                                               /*Implicit=*/true));
}

static void
PrintReplExpr(TypeChecker &TC, VarDecl *Arg,
              Type SugarT, CanType T,
              SourceLoc Loc, SourceLoc EndLoc,
              SmallVectorImpl<unsigned> &MemberIndexes,
              SmallVectorImpl<ASTNode> &BodyContent,
              SmallVectorImpl<ValueDecl*> &PrintDecls,
              DeclContext *DC);

static void
PrintStruct(TypeChecker &TC, VarDecl *Arg,
            Type SugarT, StructDecl *SD,
            SourceLoc Loc, SourceLoc EndLoc,
            SmallVectorImpl<unsigned> &MemberIndexes,
            SmallVectorImpl<ASTNode> &BodyContent,
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
      if (!VD->isComputed()) {
        if (isFirstMember)
          isFirstMember = false;
        else
          PrintLiteralString(", ", TC, Loc, PrintDecls, BodyContent);
        
        MemberIndexes.push_back(idx);
        
        Type SubstType = VD->getType();
        SubstType = TC.substMemberTypeWithBase(DC->getParentModule(),
                                               SubstType, VD, SugarT);
        
        PrintReplExpr(TC, Arg, SubstType, SubstType->getCanonicalType(),
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

  Expr *ArgRef = TC.buildCheckedRefExpr(Arg, Loc, /*Implicit=*/true);
  ArgRef = TC.coerceToRValue(ArgRef);
  for (unsigned i : MemberIndexes) {
    bool failed = TC.typeCheckExpression(ArgRef, Arg->getDeclContext(), Type(),
                                         /*discardedExpr=*/false);
    assert(!failed);
    (void)failed;

    // For each index, we look through a TupleType or StructType.
    CanType CurT = ArgRef->getType()->getRValueType()->getCanonicalType();
    if (StructType *ST = dyn_cast<StructType>(CurT)) {
      VarDecl *VD = cast<VarDecl>(ST->getDecl()->getMembers()[i]);
      ArgRef = new (Context) MemberRefExpr(ArgRef, Loc, VD, Loc,
                                           /*Implicit=*/true);
      continue;
    }
    if (BoundGenericStructType *BGST = dyn_cast<BoundGenericStructType>(CurT)) {
      VarDecl *VD = cast<VarDecl>(BGST->getDecl()->getMembers()[i]);
      Module *M = Arg->getDeclContext()->getParentModule();
      ArgRef = new (Context) MemberRefExpr(
                               ArgRef, Loc,
                               ConcreteDeclRef(Context, VD,
                                               BGST->getSubstitutions(M, &TC)),
                               Loc, /*Implicit=*/true);
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
           SmallVectorImpl<ASTNode> &BodyContent,
           SmallVectorImpl<ValueDecl*> &PrintDecls) {

  ASTContext &Context = TC.Context;

  PrintLiteralString("<", TC, Loc, PrintDecls, BodyContent);
  
  // If the metatype has a className() property, use it to display the dynamic
  // type of the instance.
  bool showedDynamicType = false;
  Identifier MemberName = Context.getIdentifier("className");
  Type MetaT = MetaTypeType::get(SugarT, Context);
  if (TC.lookupMember(MetaT, MemberName, CD,
                      /*allowDynamicLookup=*/false)){
    Expr *ArgRef = getArgRefExpr(TC, Arg, MemberIndexes, Loc);
    MetatypeExpr *Meta = new (Context) MetatypeExpr(ArgRef,
                                                    Loc,
                                                    MetaT);
    Expr *Res = new (TC.Context) UnresolvedDotExpr(Meta, Loc, MemberName, EndLoc,
                                                   /*Implicit=*/true);
    TupleExpr *CallArgs
      = new (Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, EndLoc,
                                /*hasTrailingClosure=*/false,
                                /*Implicit=*/true,
                                TupleType::getEmpty(Context));
    Expr *CE = new (Context) CallExpr(Res, CallArgs, /*Implicit=*/true, Type());
    Res = CE;

    Expr *PrintStrFn = TC.buildRefExpr(PrintDecls, Loc, /*Implicit=*/true);
    CE = new (Context) CallExpr(PrintStrFn, Res, /*Implicit=*/true);
    if (TC.typeCheckExpression(CE, Arg->getDeclContext(), Type(),
                               /*discardedExpr=*/false))
      goto dynamicTypeFailed;
    Res = CE;

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

/// \brief Print a collection.
static void
PrintCollection(TypeChecker &TC, VarDecl *Arg, Type KeyTy, Type ValueTy,
           SourceLoc Loc, SourceLoc EndLoc,
           SmallVectorImpl<unsigned> &MemberIndexes,
           SmallVectorImpl<ASTNode> &BodyContent,
           SmallVectorImpl<ValueDecl*> &PrintDecls,
           DeclContext *DC) {
  ASTContext &context = TC.Context;

  // Dig up Bool, true, and false. We'll need them.
  Type boolTy = TC.lookupBoolType(DC);
  UnqualifiedLookup lookupTrue(context.getIdentifier("true"),
                               TC.getStdlibModule(DC), nullptr);
  auto trueDecl = lookupTrue.isSuccess()
                    ? dyn_cast<VarDecl>(lookupTrue.Results[0].getValueDecl())
                    : nullptr;
  UnqualifiedLookup lookupFalse(context.getIdentifier("false"),
                                TC.getStdlibModule(DC), nullptr);
  auto falseDecl = lookupFalse.isSuccess()
                     ? dyn_cast<VarDecl>(lookupFalse.Results[0].getValueDecl())
                     : nullptr;

  // If we have Bool/true/false, create the declaration "first" to capture the
  // first walk through the loop and initialize it to "true".
  VarDecl *firstVar = nullptr;
  if (boolTy && trueDecl && falseDecl) {
    firstVar = new (context) VarDecl(/*static*/ false,
                                     Loc,
                                     context.getIdentifier("first"),
                                     boolTy, DC);
    Pattern *pattern = new (context) NamedPattern(firstVar);
    pattern->setType(boolTy);
    pattern = new (context) TypedPattern(pattern, TypeLoc::withoutLoc(boolTy));
    pattern->setType(boolTy);

    Expr *init = TC.buildCheckedRefExpr(trueDecl, Loc, /*Implicit=*/true);
    BodyContent.push_back(new (context) PatternBindingDecl(SourceLoc(),
                                                           Loc, pattern, init,
                                                           DC));
  }

  // Add opening bracket '['.
  PrintLiteralString("[", TC, Loc, PrintDecls, BodyContent);

  // Create the "value" variable and its pattern.
  auto valueId = context.getIdentifier("value");
  VarDecl *valueVar = new (context) VarDecl(/*static*/ false,
                                            Loc, valueId, ValueTy, DC);
  Pattern *valuePattern = new (context) NamedPattern(valueVar);
  valuePattern->setType(ValueTy);
  valuePattern = new (context) TypedPattern(valuePattern,
                                            TypeLoc::withoutLoc(ValueTy));
  valuePattern->setType(ValueTy);

  // Create the pattern. For a dictionary 
  Pattern *pattern = nullptr;
  VarDecl *keyVar = nullptr;
  if (KeyTy) {
    // We have a key type, so the pattern is '(key, value)'.

    // Form the key variable and its pattern.
    auto keyId = context.getIdentifier("key");
    keyVar = new (context) VarDecl(/*static*/ false,
                                   Loc, keyId, KeyTy, DC);
    Pattern *keyPattern = new (context) NamedPattern(keyVar);
    keyPattern->setType(KeyTy);
    keyPattern = new (context) TypedPattern(keyPattern,
                                            TypeLoc::withoutLoc(KeyTy));

    // Form the tuple pattern.
    TuplePatternElt patternElts[2] = {
      TuplePatternElt(keyPattern), TuplePatternElt(valuePattern)
    };
    pattern = TuplePattern::create(context,SourceLoc(),patternElts,SourceLoc());

    // Provide a type for the pattern.
    TupleTypeElt elts[2] = {
      TupleTypeElt(KeyTy, keyId),
      TupleTypeElt(ValueTy, valueId)
    };
    pattern->setType(TupleType::get(elts, context));

  } else {
    // We don't have a key type, so the pattern is just 'value'.
    pattern = valuePattern;
  }


  // Construct the loop body.
  SmallVector<ASTNode, 4> loopBodyContents;

  SmallVector<unsigned, 2> subMemberIndexes;
  
  // First, print the ", " between elements.
  if (firstVar) {
    // if branch: set first to false
    Expr *firstRef = TC.buildCheckedRefExpr(firstVar, Loc, /*Implicit=*/true);
    Expr *falseRef = TC.buildCheckedRefExpr(falseDecl, Loc, /*Implicit=*/true);
    Expr *setFirstToFalse
      = new (context) AssignExpr(firstRef, Loc, falseRef, /*Implicit=*/true);
    Stmt *thenStmt = BraceStmt::create(context, Loc,
                                       ASTNode(setFirstToFalse), Loc);

    // else branch: print a comma.
    SmallVector<ASTNode, 4> elseBodyContents;
    PrintLiteralString(", ", TC, Loc, PrintDecls, elseBodyContents);
    Stmt *elseStmt = BraceStmt::create(context, Loc, elseBodyContents, Loc);

    // if-then-else statement.
    firstRef = TC.buildCheckedRefExpr(firstVar, Loc, /*Implicit=*/true);
    loopBodyContents.push_back(new (context) IfStmt(Loc, firstRef,
                                                    thenStmt,
                                                    Loc, elseStmt));
  } else {
    PrintLiteralString(", ", TC, Loc, PrintDecls, loopBodyContents);
  }

  // If there is a key, print it and the ':'.
  if (keyVar) {
    PrintReplExpr(TC, keyVar, KeyTy, KeyTy->getCanonicalType(), Loc,
                  EndLoc, subMemberIndexes, loopBodyContents, PrintDecls, DC);

    PrintLiteralString(" : ", TC, Loc, PrintDecls, loopBodyContents);
  }

  // Print the value
  PrintReplExpr(TC, valueVar, ValueTy, ValueTy->getCanonicalType(), Loc,
                EndLoc, subMemberIndexes, loopBodyContents, PrintDecls, DC);

  auto loopBody = BraceStmt::create(context, Loc, loopBodyContents, EndLoc);

  // Construct the loop.
  Expr *argRef = getArgRefExpr(TC, Arg, MemberIndexes, Loc);
  
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
              SmallVectorImpl<ASTNode> &BodyContent,
              SmallVectorImpl<ValueDecl*> &PrintDecls,
              DeclContext *DC) {
  ASTContext &Context = TC.Context;

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
  if (TC.lookupMember(T, MemberName, DC,
                      /*allowDynamicLookup=*/false)) {
    Expr *ArgRef = getArgRefExpr(TC, Arg, MemberIndexes, Loc);
    Expr *Res = new (TC.Context) UnresolvedDotExpr(ArgRef, Loc, MemberName, 
                                                   EndLoc, /*Implicit=*/true);
    TupleExpr *CallArgs
      = new (Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, EndLoc,
                                /*hasTrailingClosure=*/false,
                                /*Implicit=*/true,
                                TupleType::getEmpty(Context));
    Expr *CE = new (Context) CallExpr(Res, CallArgs, /*Implicit=*/true, Type());
    if (TC.typeCheckExpression(CE, Arg->getDeclContext(), Type(),
                               /*discardedExpr=*/false))
      return;
    Res = CE;
    BodyContent.push_back(Res);
    return;
  }

  // We print a struct by printing each stored member variable.
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
    if (!BGST->getParent() && BGST->getDecl()->getName().str() == "Array") {
      PrintCollection(TC, Arg, Type(), BGST->getGenericArgs()[0], Loc, EndLoc,
                      MemberIndexes, BodyContent, PrintDecls, DC);
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

  if (BoundGenericClassType *BGCT = dyn_cast<BoundGenericClassType>(T)) {
    // FIXME: We have to hack Slice into here, because replPrint on Dictionary
    // isn't implementable yet.  We don't want the T argument of the dictionary
    // to be constrained to being replPrintable.  We need replPrint to be more
    // dynamically reflective in its implementation.
    if (!BGCT->getParent() && BGCT->getDecl()->getName().str() == "Dictionary"){
      PrintCollection(TC, Arg, BGCT->getGenericArgs()[0],
                      BGCT->getGenericArgs()[1], Loc, EndLoc,
                      MemberIndexes, BodyContent, PrintDecls, DC);
      return;
    }

    PrintClass(TC, Arg, SugarT, BGCT->getDecl(), Loc, EndLoc,
               MemberIndexes, BodyContent, PrintDecls);
    return;
  }

  // FIXME: We should handle EnumTypes at some point.

  PrintLiteralString("<unprintable value>", TC, Loc, PrintDecls,
                     BodyContent);
}

Identifier TypeChecker::getNextResponseVariableName(DeclContext *DC) {
  llvm::SmallString<4> namebuf;
  llvm::raw_svector_ostream names(namebuf);
  Identifier ident;
  
  bool nameUsed = false;
  do {
    names.flush();
    namebuf.clear();
    
    names << "r" << NextResponseVariableIndex++;
    
    ident = Context.getIdentifier(names.str());
    UnqualifiedLookup lookup(ident, DC, this);
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
  
#define INVALID_PATTERN(Id, Parent) \
  void visit##Id##Pattern(Id##Pattern *P) { \
    llvm_unreachable("pattern cannot appear in an LHS!"); \
  }
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) INVALID_PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
};
} // end anonymous namespace.

/// generatePrintOfExpression - Emit logic to print the specified expression
/// value with the given description of the pattern involved.
static void generatePrintOfExpression(StringRef NameStr, Expr *E,
                                      TypeChecker *TC, SourceFile &SF) {
  ASTContext &C = TC->Context;

  // Always print rvalues, not lvalues.
  E = TC->coerceToMaterializable(E);

  CanType T = E->getType()->getCanonicalType();
  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();
  
  SmallVector<ValueDecl*, 4> PrintDecls;
  UnqualifiedLookup PrintDeclLookup(C.getIdentifier("print"),
                                    TC->getStdlibModule(&SF), TC);
  if (!PrintDeclLookup.isSuccess())
    return;
  for (auto Result : PrintDeclLookup.Results)
    PrintDecls.push_back(Result.getValueDecl());
  
  // Build function of type T->() which prints the operand.
  VarDecl *Arg = new (C) VarDecl(/*static*/ false,
                                 Loc,
                                 TC->Context.getIdentifier("arg"),
                                 E->getType(), nullptr);
  Pattern *ParamPat = new (C) NamedPattern(Arg);
  ParamPat = new (C) TypedPattern(ParamPat,TypeLoc::withoutLoc(Arg->getType()));
  if (!isa<TupleType>(T)) {
    TuplePatternElt elt{ParamPat};
    ParamPat = TuplePattern::create(C, SourceLoc(), elt, SourceLoc());
  }
  TC->typeCheckPattern(ParamPat,
                       Arg->getDeclContext(),
                       /*allowUnknownTypes*/false);
  ClosureExpr *CE =
      new (C) ClosureExpr(ParamPat, SourceLoc(), TypeLoc(),
                          /*discriminator*/ 0, &SF);
  Type FuncTy = FunctionType::get(ParamPat->getType(), TupleType::getEmpty(C),
                                  C);
  CE->setType(FuncTy);
  Arg->setDeclContext(CE);
  
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
  SmallVector<ASTNode, 4> BodyContent;
  PrintLiteralString(TmpStr, *TC, Loc, PrintDecls, BodyContent);
  
  SmallVector<unsigned, 4> MemberIndexes;
  PrintReplExpr(*TC, Arg, E->getType(), T, Loc, EndLoc, MemberIndexes,
                BodyContent, PrintDecls, CE);
  PrintLiteralString("\n", *TC, Loc, PrintDecls, BodyContent);
  
  // Typecheck the function.
  BraceStmt *Body = BraceStmt::create(C, Loc, BodyContent, EndLoc);
  CE->setBody(Body, false);
  TC->typeCheckClosureBody(CE);
  
  Expr *TheCall = new (C) CallExpr(CE, E, /*Implicit=*/true);
  if (TC->typeCheckExpressionShallow(TheCall, Arg->getDeclContext()))
    return ;
  
  // Inject the call into the top level stream by wrapping it with a TLCD.
  auto *BS = BraceStmt::create(C, Loc, ASTNode(TheCall),
                               EndLoc);
  SF.Decls.push_back(new (C) TopLevelCodeDecl(&SF, BS));
}



/// When we see an expression in a TopLevelCodeDecl in the REPL, process it,
/// adding the proper decls back to the top level of the file.
static void processREPLTopLevelExpr(Expr *E, TypeChecker *TC, SourceFile &SF) {
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
    generatePrintOfExpression(d->getName().str(), E, TC, SF);
    return;
  }
  
  // Remove the expression from being in the list of decls to execute, we're
  // going to reparent it.
  SF.Decls.pop_back();

  E = TC->coerceToMaterializable(E);

  // Create the meta-variable, let the typechecker name it.
  Identifier name = TC->getNextResponseVariableName(SF.getParentModule());
  VarDecl *vd = new (TC->Context) VarDecl(/*static*/ false,
                                          E->getStartLoc(), name,
                                          E->getType(), &SF);
  SF.Decls.push_back(vd);

  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (TC->Context) NamedPattern(vd);
  metavarPat->setType(E->getType());
  PatternBindingDecl *metavarBinding
    = new (TC->Context) PatternBindingDecl(SourceLoc(),
                                           E->getStartLoc(), metavarPat, E,
                                           &SF);
  SF.Decls.push_back(metavarBinding);

  // Finally, print the variable's value.
  E = TC->buildCheckedRefExpr(vd, E->getStartLoc(), /*Implicit=*/true);
  generatePrintOfExpression(vd->getName().str(), E, TC, SF);
}

/// processREPLTopLevelPatternBinding - When we see a new PatternBinding parsed
/// into the REPL, process it by generating code to print it out.
static void processREPLTopLevelPatternBinding(PatternBindingDecl *PBD,
                                              TypeChecker *TC,
                                              SourceFile &SF) {
  // If there is no initializer for the new variable, don't auto-print it.
  // This would just cause a confusing definite initialization error.  Some
  // day we will do some high level analysis of uninitialized variables
  // (rdar://15157729) but until then, output a specialized error.
  if (!PBD->getInit()) {
    TC->diagnose(PBD->getStartLoc(), diag::repl_must_be_initialized);
    return;
  }
  
  llvm::SmallString<16> PatternString;
  PatternBindingPrintLHS(PatternString).visit(PBD->getPattern());
  
  // If the bound pattern is a single value, use a DeclRefExpr on the underlying
  // Decl to print it.
  if (auto *NP = dyn_cast<NamedPattern>(PBD->getPattern()->
                                           getSemanticsProvidingPattern())) {
    Expr *E = TC->buildCheckedRefExpr(NP->getDecl(), PBD->getStartLoc(),
                                      /*Implicit=*/true);
    generatePrintOfExpression(PatternString, E, TC, SF);
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
  auto PBTLCD = cast<TopLevelCodeDecl>(SF.Decls.back());
  SF.Decls.pop_back();

  // Create the meta-variable, let the typechecker name it.
  Identifier name = TC->getNextResponseVariableName(SF.getParentModule());
  VarDecl *vd = new (TC->Context) VarDecl(/*static*/ false,
                                          PBD->getStartLoc(), name,
                                          PBD->getPattern()->getType(), &SF);
  SF.Decls.push_back(vd);

  
  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (TC->Context) NamedPattern(vd);
  metavarPat->setType(vd->getType());
  PatternBindingDecl *metavarBinding
    = new (TC->Context) PatternBindingDecl(SourceLoc(),
                                           PBD->getStartLoc(), metavarPat,
                                           PBD->getInit(), &SF);

  auto MVBrace = BraceStmt::create(TC->Context, metavarBinding->getStartLoc(),
                                   ASTNode(metavarBinding),
                                   metavarBinding->getEndLoc());

  auto *MVTLCD = new (TC->Context) TopLevelCodeDecl(&SF, MVBrace);
  SF.Decls.push_back(MVTLCD);

  
  // Replace the initializer of PBD with a reference to our repl temporary.
  Expr *E = TC->buildCheckedRefExpr(vd, vd->getStartLoc(), /*Implicit=*/true);
  E = TC->coerceToMaterializable(E);
  PBD->setInit(E, /*checked=*/true);
  SF.Decls.push_back(PBTLCD);

  // Finally, print out the result, by referring to the repl temp.
  E = TC->buildCheckedRefExpr(vd, vd->getStartLoc(), /*Implicit=*/true);
  generatePrintOfExpression(PatternString, E, TC, SF);
}



/// processREPLTopLevel - This is called after we've parsed and typechecked some
/// new decls at the top level.  We inject code to print out expressions and
/// pattern bindings the are evaluated.
void TypeChecker::processREPLTopLevel(SourceFile &SF, unsigned FirstDecl) {
  // Loop over all of the new decls, moving them out of the Decls list, then
  // adding them back (with modifications) one at a time.
  std::vector<Decl*> NewDecls(SF.Decls.begin()+FirstDecl, SF.Decls.end());
  SF.Decls.resize(FirstDecl);

  // Loop over each of the new decls, processing them, adding them back to
  // the Decls list.
  for (Decl *D : NewDecls) {
    SF.Decls.push_back(D);

    TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D);
    if (!TLCD || TLCD->getBody()->getElements().empty())
      continue;

    auto Entry = TLCD->getBody()->getElements()[0];

    // Check to see if the TLCD has an expression that we have to transform.
    if (Expr *E = Entry.dyn_cast<Expr*>())
      processREPLTopLevelExpr(E, this, SF);
    else if (Decl *D = Entry.dyn_cast<Decl*>())
      if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D))
        processREPLTopLevelPatternBinding(PBD, this, SF);
  }
}

