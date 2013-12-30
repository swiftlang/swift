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
#include "swift/Parse/LocalContext.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

namespace {
  struct REPLContext {
    TypeChecker &TC;
    ASTContext &Context;
    SourceFile &SF;
    SmallVector<ValueDecl*, 4> PrintDecls;

    REPLContext(TypeChecker &TC, SourceFile &SF)
      : TC(TC), Context(TC.Context), SF(SF) {}

    /// Look up the identifier "print".
    bool requirePrintDecls() {
      if (!PrintDecls.empty()) return false;

      UnqualifiedLookup lookup(Context.getIdentifier("print"),
                               TC.getStdlibModule(&SF), &TC);
      if (!lookup.isSuccess())
        return true;
      for (auto result : lookup.Results)
        PrintDecls.push_back(result.getValueDecl());
      return false;
    }
  };

  class StmtBuilder {
    REPLContext &C;
    TypeChecker &TC;
    ASTContext &Context;
    DeclContext *DC;
    SmallVector<ASTNode, 8> Body;

  public:
    StmtBuilder(REPLContext &C, DeclContext *DC)
      : C(C), TC(C.TC), Context(C.Context), DC(DC) { assert(DC); }
    StmtBuilder(StmtBuilder &parent)
      : C(parent.C), TC(C.TC), Context(C.Context), DC(parent.DC) {}

    ~StmtBuilder() {
      assert(Body.empty() && "statements remain in builder?");
    }

    BraceStmt *createBodyStmt(SourceLoc loc, SourceLoc endLoc) {
      auto result = BraceStmt::create(Context, loc, Body, endLoc);
      Body.clear();
      return result;
    }

    void printLiteralString(StringRef str, SourceLoc loc);
    void printReplExpr(VarDecl *arg, Type sugarType, CanType type,
                       SourceLoc loc, SourceLoc endLoc,
                       SmallVectorImpl<unsigned> &memberIndexes);
    void printStruct(VarDecl *arg, Type sugarType, StructDecl *SD,
                     SourceLoc loc, SourceLoc endLoc,
                     SmallVectorImpl<unsigned> &memberIndexes);
    void printClass(VarDecl *arg, Type sugarType, ClassDecl *CD,
                    SourceLoc loc, SourceLoc endLoc,
                    SmallVectorImpl<unsigned> &memberIndexes);
    void printCollection(VarDecl *arg, Type keyType, Type valueType,
                         SourceLoc loc, SourceLoc endLoc,
                         SmallVectorImpl<unsigned> &memberIndexes);

    Expr *getArgRefExpr(VarDecl *arg, ArrayRef<unsigned> memberIndexes,
                        SourceLoc loc) const;

    void addToBody(ASTNode node) {
      Body.push_back(node);
    }

    Expr *buildPrintRefExpr(SourceLoc loc) {
      assert(!C.PrintDecls.empty());
      return TC.buildRefExpr(C.PrintDecls, loc, /*Implicit=*/true);
    }
  };
}

void StmtBuilder::printLiteralString(StringRef Str, SourceLoc Loc) {
  Expr *PrintStr = new (Context) StringLiteralExpr(Str, Loc);
  Expr *PrintStrFn = buildPrintRefExpr(Loc);
  addToBody(new (Context) CallExpr(PrintStrFn, PrintStr, /*Implicit=*/true));
}

void StmtBuilder::printStruct(VarDecl *Arg, Type SugarT, StructDecl *SD,
                              SourceLoc Loc, SourceLoc EndLoc,
                              SmallVectorImpl<unsigned> &MemberIndexes) {
  auto TypeStr
    = Context.getIdentifier(SugarT->getString()).str();
  printLiteralString(TypeStr, Loc);
  printLiteralString("(", Loc);
  
  unsigned idx = 0;
  bool isFirstMember = true;
  for (Decl *D : SD->getMembers()) {
    if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
      if (!VD->isComputed()) {
        if (isFirstMember)
          isFirstMember = false;
        else
          printLiteralString(", ", Loc);
        
        MemberIndexes.push_back(idx);
        
        Type SubstType = VD->getType();
        SubstType = TC.substMemberTypeWithBase(DC->getParentModule(),
                                               SubstType, VD, SugarT);
        
        printReplExpr(Arg, SubstType, SubstType->getCanonicalType(),
                      Loc, EndLoc, MemberIndexes);

        MemberIndexes.pop_back();
      }
    }
    ++idx;
  }
  printLiteralString(")", Loc);
}

Expr *StmtBuilder::getArgRefExpr(VarDecl *Arg, ArrayRef<unsigned> MemberIndexes,
                                 SourceLoc Loc) const {
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

void StmtBuilder::printClass(VarDecl *Arg, Type SugarT, ClassDecl *CD,
                             SourceLoc Loc, SourceLoc EndLoc,
                             SmallVectorImpl<unsigned> &MemberIndexes) {
  printLiteralString("<", Loc);
  
  // If the metatype has a className() property, use it to display the dynamic
  // type of the instance.
  bool showedDynamicType = false;
  Identifier MemberName = Context.getIdentifier("className");
  Type MetaT = MetatypeType::get(SugarT, Context);
  if (TC.lookupMember(MetaT, MemberName, CD,
                      /*allowDynamicLookup=*/false)){
    Expr *ArgRef = getArgRefExpr(Arg, MemberIndexes, Loc);
    MetatypeExpr *Meta = new (Context) MetatypeExpr(ArgRef,
                                                    Loc,
                                                    MetaT);
    Expr *Res = new (Context) UnresolvedDotExpr(Meta, Loc, MemberName, EndLoc,
                                                /*Implicit=*/true);
    TupleExpr *CallArgs
      = new (Context) TupleExpr(Loc, MutableArrayRef<Expr *>(), 0, EndLoc,
                                /*hasTrailingClosure=*/false,
                                /*Implicit=*/true,
                                TupleType::getEmpty(Context));
    Expr *CE = new (Context) CallExpr(Res, CallArgs, /*Implicit=*/true, Type());
    Res = CE;

    Expr *PrintStrFn = buildPrintRefExpr(Loc);
    CE = new (Context) CallExpr(PrintStrFn, Res, /*Implicit=*/true);
    if (TC.typeCheckExpression(CE, Arg->getDeclContext(), Type(),
                               /*discardedExpr=*/false))
      goto dynamicTypeFailed;
    Res = CE;

    addToBody(Res);
    showedDynamicType = true;
  }

dynamicTypeFailed:
  if (!showedDynamicType) {
    auto TypeStr
      = TC.Context.getIdentifier(SugarT->getString()).str();
    printLiteralString(TypeStr, Loc);
  }
  
  printLiteralString(" instance>", Loc);
}

/// \brief Print a collection.
void StmtBuilder::printCollection(VarDecl *Arg, Type KeyTy, Type ValueTy,
                                  SourceLoc Loc, SourceLoc EndLoc,
                                  SmallVectorImpl<unsigned> &MemberIndexes) {
  // Dig up Bool, true, and false. We'll need them.
  Type boolTy = TC.lookupBoolType(DC);
  UnqualifiedLookup lookupTrue(Context.getIdentifier("true"),
                               TC.getStdlibModule(DC), nullptr);
  auto trueDecl = lookupTrue.isSuccess()
                    ? dyn_cast<VarDecl>(lookupTrue.Results[0].getValueDecl())
                    : nullptr;
  UnqualifiedLookup lookupFalse(Context.getIdentifier("false"),
                                TC.getStdlibModule(DC), nullptr);
  auto falseDecl = lookupFalse.isSuccess()
                     ? dyn_cast<VarDecl>(lookupFalse.Results[0].getValueDecl())
                     : nullptr;

  // If we have Bool/true/false, create the declaration "first" to capture the
  // first walk through the loop and initialize it to "true".
  VarDecl *firstVar = nullptr;
  if (boolTy && trueDecl && falseDecl) {
    firstVar = new (Context) VarDecl(/*static*/ false, /*IsLet*/false,
                                     Loc,
                                     Context.getIdentifier("first"),
                                     boolTy, DC);
    Pattern *pattern = new (Context) NamedPattern(firstVar);
    pattern->setType(boolTy);
    pattern = new (Context) TypedPattern(pattern, TypeLoc::withoutLoc(boolTy));
    pattern->setType(boolTy);

    Expr *init = TC.buildCheckedRefExpr(trueDecl, Loc, /*Implicit=*/true);
    addToBody(new (Context) PatternBindingDecl(SourceLoc(),
                                               Loc, pattern, init,
                                               DC));
  }

  // Add opening bracket '['.
  printLiteralString("[", Loc);

  // Create the "value" variable and its pattern.
  auto valueId = Context.getIdentifier("value");
  VarDecl *valueVar = new (Context) VarDecl(/*static*/ false, /*IsLet*/false,
                                            Loc, valueId, ValueTy, DC);
  Pattern *valuePattern = new (Context) NamedPattern(valueVar);
  valuePattern->setType(ValueTy);
  valuePattern = new (Context) TypedPattern(valuePattern,
                                            TypeLoc::withoutLoc(ValueTy));
  valuePattern->setType(ValueTy);

  // Create the pattern. For a dictionary 
  Pattern *pattern = nullptr;
  VarDecl *keyVar = nullptr;
  if (KeyTy) {
    // We have a key type, so the pattern is '(key, value)'.

    // Form the key variable and its pattern.
    auto keyId = Context.getIdentifier("key");
    keyVar = new (Context) VarDecl(/*static*/ false, /*IsLet*/false,
                                   Loc, keyId, KeyTy, DC);
    Pattern *keyPattern = new (Context) NamedPattern(keyVar);
    keyPattern->setType(KeyTy);
    keyPattern = new (Context) TypedPattern(keyPattern,
                                            TypeLoc::withoutLoc(KeyTy));

    // Form the tuple pattern.
    TuplePatternElt patternElts[2] = {
      TuplePatternElt(keyPattern), TuplePatternElt(valuePattern)
    };
    pattern = TuplePattern::create(Context,SourceLoc(),patternElts,SourceLoc());

    // Provide a type for the pattern.
    TupleTypeElt elts[2] = {
      TupleTypeElt(KeyTy, keyId),
      TupleTypeElt(ValueTy, valueId)
    };
    pattern->setType(TupleType::get(elts, Context));

  } else {
    // We don't have a key type, so the pattern is just 'value'.
    pattern = valuePattern;
  }


  // Construct the loop body.
  StmtBuilder loopBuilder(*this);

  SmallVector<unsigned, 2> subMemberIndexes;
  
  // First, print the ", " between elements.
  if (firstVar) {
    // if branch: set first to false
    Expr *firstRef = TC.buildCheckedRefExpr(firstVar, Loc, /*Implicit=*/true);
    Expr *falseRef = TC.buildCheckedRefExpr(falseDecl, Loc, /*Implicit=*/true);
    Expr *setFirstToFalse
      = new (Context) AssignExpr(firstRef, Loc, falseRef, /*Implicit=*/true);
    Stmt *thenStmt = BraceStmt::create(Context, Loc,
                                       ASTNode(setFirstToFalse), Loc);

    // else branch: print a comma.
    StmtBuilder elseBuilder(*this);
    elseBuilder.printLiteralString(", ", Loc);

    Stmt *elseStmt = elseBuilder.createBodyStmt(Loc, Loc);

    // if-then-else statement.
    firstRef = TC.buildCheckedRefExpr(firstVar, Loc, /*Implicit=*/true);
    loopBuilder.addToBody(new (Context) IfStmt(Loc, firstRef, thenStmt,
                                               Loc, elseStmt));
  } else {
    loopBuilder.printLiteralString(", ", Loc);
  }

  // If there is a key, print it and the ':'.
  if (keyVar) {
    loopBuilder.printReplExpr(keyVar, KeyTy, KeyTy->getCanonicalType(), Loc,
                              EndLoc, subMemberIndexes);

    loopBuilder.printLiteralString(" : ", Loc);
  }

  // Print the value
  loopBuilder.printReplExpr(valueVar, ValueTy, ValueTy->getCanonicalType(),
                            Loc, EndLoc, subMemberIndexes);

  auto loopBody = loopBuilder.createBodyStmt(Loc, EndLoc);

  // Construct the loop.
  Expr *argRef = getArgRefExpr(Arg, MemberIndexes, Loc);
  
  addToBody(new (Context) ForEachStmt(Loc, pattern, Loc, argRef, loopBody));

  // Add closing bracket ']'.
  printLiteralString("]", EndLoc);
}

void StmtBuilder::printReplExpr(VarDecl *Arg, Type SugarT, CanType T,
                                SourceLoc Loc, SourceLoc EndLoc,
                                SmallVectorImpl<unsigned> &MemberIndexes) {
  if (TupleType *TT = dyn_cast<TupleType>(T)) {
    // We print a tuple by printing each element.
    printLiteralString("(", Loc);

    for (unsigned i = 0, e = TT->getFields().size(); i < e; ++i) {
      MemberIndexes.push_back(i);
      CanType SubType = TT->getElementType(i)->getCanonicalType();
      printReplExpr(Arg, TT->getElementType(i), SubType, Loc, EndLoc,
                    MemberIndexes);
      MemberIndexes.pop_back();

      if (i + 1 != e)
        printLiteralString(", ", Loc);
    }

    printLiteralString(")", Loc);
    return;
  }

  Identifier MemberName = Context.getIdentifier("replPrint");
  if (TC.lookupMember(T, MemberName, DC,
                      /*allowDynamicLookup=*/false)) {
    Expr *ArgRef = getArgRefExpr(Arg, MemberIndexes, Loc);
    Expr *Res = new (Context) UnresolvedDotExpr(ArgRef, Loc, MemberName, 
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
    addToBody(Res);
    return;
  }

  // We print a struct by printing each stored member variable.
  if (StructType *ST = dyn_cast<StructType>(T)) {
    printStruct(Arg, SugarT, ST->getDecl(), Loc, EndLoc, MemberIndexes);
    return;
  }
  
  if (BoundGenericStructType *BGST = dyn_cast<BoundGenericStructType>(T)) {
    // FIXME: We have to hack Slice into here, because replPrint on Slice isn't
    // implementable yet.  We don't want the T argument of the slice to be
    // constrained to being replPrintable.  We need replPrint to be more
    // dynamically reflective in its implementation.
    if (!BGST->getParent() && BGST->getDecl()->getName().str() == "Array") {
      printCollection(Arg, Type(), BGST->getGenericArgs()[0], Loc, EndLoc,
                      MemberIndexes);
      return;
    }

    printStruct(Arg, SugarT, BGST->getDecl(), Loc, EndLoc, MemberIndexes);
    return;
  }

  if (ClassType *CT = dyn_cast<ClassType>(T)) {
    printClass(Arg, SugarT, CT->getDecl(), Loc, EndLoc, MemberIndexes);
    return;
  }

  if (BoundGenericClassType *BGCT = dyn_cast<BoundGenericClassType>(T)) {
    // FIXME: We have to hack Slice into here, because replPrint on Dictionary
    // isn't implementable yet.  We don't want the T argument of the dictionary
    // to be constrained to being replPrintable.  We need replPrint to be more
    // dynamically reflective in its implementation.
    if (!BGCT->getParent() && BGCT->getDecl()->getName().str() == "Dictionary"){
      printCollection(Arg, BGCT->getGenericArgs()[0],
                      BGCT->getGenericArgs()[1], Loc, EndLoc,
                      MemberIndexes);
      return;
    }

    printClass(Arg, SugarT, BGCT->getDecl(), Loc, EndLoc, MemberIndexes);
    return;
  }

  // FIXME: We should handle EnumTypes at some point.

  printLiteralString("<unprintable value>", Loc);
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
  void visitVarPattern(VarPattern *P) {
    visit(P->getSubPattern());
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

namespace {
  class REPLChecker : public REPLContext {
    TopLevelContext &TLC;
  public:
    REPLChecker(TypeChecker &TC, SourceFile &SF, TopLevelContext &TLC)
      : REPLContext(TC, SF), TLC(TLC) {}

    void processREPLTopLevelExpr(Expr *E);
    void processREPLTopLevelPatternBinding(PatternBindingDecl *PBD);
  private:
    void generatePrintOfExpression(StringRef name, Expr *E);
  };
}

/// generatePrintOfExpression - Emit logic to print the specified expression
/// value with the given description of the pattern involved.
void REPLChecker::generatePrintOfExpression(StringRef NameStr, Expr *E) {
  // Always print rvalues, not lvalues.
  E = TC.coerceToMaterializable(E);

  CanType T = E->getType()->getCanonicalType();
  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();

  // Require a non-trivial set of print functions.
  if (requirePrintDecls())
    return;
  
  // Build function of type T->() which prints the operand.
  VarDecl *Arg = new (Context) VarDecl(/*static*/ false, /*IsLet*/false,
                                       Loc,
                                       Context.getIdentifier("arg"),
                                       E->getType(), /*DC*/ nullptr);
  Pattern *ParamPat = new (Context) NamedPattern(Arg);
  ParamPat = new (Context) TypedPattern(ParamPat,
                                        TypeLoc::withoutLoc(Arg->getType()));
  if (!isa<TupleType>(T)) {
    TuplePatternElt elt{ParamPat};
    ParamPat = TuplePattern::create(Context, SourceLoc(), elt, SourceLoc());
  }
  TC.typeCheckPattern(ParamPat,
                      Arg->getDeclContext(),
                      /*allowUnknownTypes*/false);

  TopLevelCodeDecl *newTopLevel = new (Context) TopLevelCodeDecl(&SF);
  unsigned discriminator = TLC.claimNextClosureDiscriminator();

  ClosureExpr *CE =
      new (Context) ClosureExpr(ParamPat, SourceLoc(), TypeLoc(),
                                discriminator, newTopLevel);
  Type FuncTy = FunctionType::get(ParamPat->getType(),
                                  TupleType::getEmpty(Context));
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
  auto TmpStr = Context.getIdentifier(PrefixString).str();

  StmtBuilder builder(*this, CE);
  
  builder.printLiteralString(TmpStr, Loc);
  
  SmallVector<unsigned, 4> MemberIndexes;
  builder.printReplExpr(Arg, E->getType(), T, Loc, EndLoc, MemberIndexes);
  builder.printLiteralString("\n", Loc);

  // Typecheck the function.
  BraceStmt *Body = builder.createBodyStmt(Loc, EndLoc);
  CE->setBody(Body, false);
  TC.typeCheckClosureBody(CE);

  Expr *TheCall = new (Context) CallExpr(CE, E, /*Implicit=*/true);
  if (TC.typeCheckExpressionShallow(TheCall, Arg->getDeclContext()))
    return ;
  
  // Inject the call into the top level stream by wrapping it with a TLCD.
  auto *BS = BraceStmt::create(Context, Loc, ASTNode(TheCall),
                               EndLoc);
  newTopLevel->setBody(BS);
  SF.Decls.push_back(newTopLevel);
}

/// When we see an expression in a TopLevelCodeDecl in the REPL, process it,
/// adding the proper decls back to the top level of the file.
void REPLChecker::processREPLTopLevelExpr(Expr *E) {
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
    generatePrintOfExpression(d->getName().str(), E);
    return;
  }
  
  // Remove the expression from being in the list of decls to execute, we're
  // going to reparent it.
  SF.Decls.pop_back();

  E = TC.coerceToMaterializable(E);

  // Create the meta-variable, let the typechecker name it.
  Identifier name = TC.getNextResponseVariableName(SF.getParentModule());
  VarDecl *vd = new (Context) VarDecl(/*static*/ false, /*IsLet*/true,
                                      E->getStartLoc(), name,
                                      E->getType(), &SF);
  SF.Decls.push_back(vd);

  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (Context) NamedPattern(vd);
  metavarPat->setType(E->getType());
  PatternBindingDecl *metavarBinding
    = new (Context) PatternBindingDecl(SourceLoc(),
                                       E->getStartLoc(), metavarPat, E,
                                       &SF);
  SF.Decls.push_back(metavarBinding);

  // Finally, print the variable's value.
  E = TC.buildCheckedRefExpr(vd, E->getStartLoc(), /*Implicit=*/true);
  generatePrintOfExpression(vd->getName().str(), E);
}

/// processREPLTopLevelPatternBinding - When we see a new PatternBinding parsed
/// into the REPL, process it by generating code to print it out.
void REPLChecker::processREPLTopLevelPatternBinding(PatternBindingDecl *PBD) {
  // If there is no initializer for the new variable, don't auto-print it.
  // This would just cause a confusing definite initialization error.  Some
  // day we will do some high level analysis of uninitialized variables
  // (rdar://15157729) but until then, output a specialized error.
  if (!PBD->getInit()) {
    TC.diagnose(PBD->getStartLoc(), diag::repl_must_be_initialized);
    return;
  }
  
  llvm::SmallString<16> PatternString;
  PatternBindingPrintLHS(PatternString).visit(PBD->getPattern());
  
  // If the bound pattern is a single value, use a DeclRefExpr on the underlying
  // Decl to print it.
  if (auto *NP = dyn_cast<NamedPattern>(PBD->getPattern()->
                                           getSemanticsProvidingPattern())) {
    Expr *E = TC.buildCheckedRefExpr(NP->getDecl(), PBD->getStartLoc(),
                                     /*Implicit=*/true);
    generatePrintOfExpression(PatternString, E);
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
  Identifier name = TC.getNextResponseVariableName(SF.getParentModule());
  VarDecl *vd = new (Context) VarDecl(/*static*/ false, /*IsLet*/true,
                                      PBD->getStartLoc(), name,
                                      PBD->getPattern()->getType(), &SF);
  SF.Decls.push_back(vd);

  
  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (Context) NamedPattern(vd);
  metavarPat->setType(vd->getType());
  PatternBindingDecl *metavarBinding
    = new (Context) PatternBindingDecl(SourceLoc(),
                                       PBD->getStartLoc(), metavarPat,
                                       PBD->getInit(), &SF);

  auto MVBrace = BraceStmt::create(Context, metavarBinding->getStartLoc(),
                                   ASTNode(metavarBinding),
                                   metavarBinding->getEndLoc());

  auto *MVTLCD = new (Context) TopLevelCodeDecl(&SF, MVBrace);
  SF.Decls.push_back(MVTLCD);

  
  // Replace the initializer of PBD with a reference to our repl temporary.
  Expr *E = TC.buildCheckedRefExpr(vd, vd->getStartLoc(), /*Implicit=*/true);
  E = TC.coerceToMaterializable(E);
  PBD->setInit(E, /*checked=*/true);
  SF.Decls.push_back(PBTLCD);

  // Finally, print out the result, by referring to the repl temp.
  E = TC.buildCheckedRefExpr(vd, vd->getStartLoc(), /*Implicit=*/true);
  generatePrintOfExpression(PatternString, E);
}



/// processREPLTopLevel - This is called after we've parsed and typechecked some
/// new decls at the top level.  We inject code to print out expressions and
/// pattern bindings the are evaluated.
void TypeChecker::processREPLTopLevel(SourceFile &SF, TopLevelContext &TLC,
                                      unsigned FirstDecl) {
  // Loop over all of the new decls, moving them out of the Decls list, then
  // adding them back (with modifications) one at a time.
  std::vector<Decl*> NewDecls(SF.Decls.begin()+FirstDecl, SF.Decls.end());
  SF.Decls.resize(FirstDecl);

  REPLChecker RC(*this, SF, TLC);

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
      RC.processREPLTopLevelExpr(E);
    else if (Decl *D = Entry.dyn_cast<Decl*>())
      if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D))
        RC.processREPLTopLevelPatternBinding(PBD);
  }

  contextualizeTopLevelCode(TLC, llvm::makeArrayRef(SF.Decls).slice(FirstDecl));
}

