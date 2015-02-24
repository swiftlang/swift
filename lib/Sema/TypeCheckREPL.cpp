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
#include "swift/AST/DiagnosticsFrontend.h"
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
  Identifier Id_print;
  SmallVector<ValueDecl *, 4> PrintDecls;
  Identifier Id_debugDebugPrintln;
  SmallVector<ValueDecl *, 4> DebugPrintlnDecls;

  REPLContext(TypeChecker &TC, SourceFile &SF)
      : TC(TC), Context(TC.Context), SF(SF),
        Id_print(Context.getIdentifier("print")),
        Id_debugDebugPrintln(Context.getIdentifier("debugPrintln")) {}

  bool requirePrintDecls() {
    if (!PrintDecls.empty() && !DebugPrintlnDecls.empty())
      return false;

    {
      UnqualifiedLookup Lookup(Id_print, TC.getStdlibModule(&SF), &TC);
      if (!Lookup.isSuccess())
        return true;
      for (auto Result : Lookup.Results)
        PrintDecls.push_back(Result.getValueDecl());
    }
    {
      UnqualifiedLookup Lookup(Id_debugDebugPrintln, TC.getStdlibModule(&SF),
                               &TC);
      if (!Lookup.isSuccess())
        return true;
      for (auto Result : Lookup.Results)
        DebugPrintlnDecls.push_back(Result.getValueDecl());
    }

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
      : C(C), TC(C.TC), Context(C.Context), DC(DC) {
    assert(DC);
  }
  StmtBuilder(StmtBuilder &parent)
      : C(parent.C), TC(C.TC), Context(C.Context), DC(parent.DC) {}

  ~StmtBuilder() { assert(Body.empty() && "statements remain in builder?"); }

  BraceStmt *createBodyStmt(SourceLoc loc, SourceLoc endLoc) {
    auto result = BraceStmt::create(Context, loc, Body, endLoc);
    Body.clear();
    return result;
  }

  void printLiteralString(StringRef str, SourceLoc loc);
  void printReplExpr(VarDecl *Arg, SourceLoc Loc);

  void addToBody(ASTNode node) { Body.push_back(node); }

  Expr *buildPrintRefExpr(SourceLoc loc) {
    assert(!C.PrintDecls.empty());
    return TC.buildRefExpr(C.PrintDecls, DC, loc, /*Implicit=*/true);
  }

  Expr *buildDebugPrintlnRefExpr(SourceLoc loc) {
    assert(!C.DebugPrintlnDecls.empty());
    return TC.buildRefExpr(C.DebugPrintlnDecls, DC, loc, /*Implicit=*/true);
  }
};
} // unnamed namespace

void StmtBuilder::printLiteralString(StringRef Str, SourceLoc Loc) {
  Expr *PrintFn = buildPrintRefExpr(Loc);
  Expr *PrintStr = new (Context) StringLiteralExpr(Str, Loc);
  addToBody(new (Context) CallExpr(PrintFn, PrintStr, /*Implicit=*/true));
}

void StmtBuilder::printReplExpr(VarDecl *Arg, SourceLoc Loc) {
  Expr *DebugPrintlnFn = buildDebugPrintlnRefExpr(Loc);
  Expr *ArgRef = TC.buildRefExpr(Arg, DC, Loc, /*Implicit=*/true);
  addToBody(new (Context) CallExpr(DebugPrintlnFn, ArgRef, /*Implicit=*/true));
}

Identifier TypeChecker::getNextResponseVariableName(DeclContext *DC) {
  llvm::SmallString<4> namebuf;
  Identifier ident;

  bool nameUsed = false;
  do {
    namebuf.clear();
    llvm::raw_svector_ostream names(namebuf);
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

namespace {
/// This is a lot like Pattern::print, but prints typed patterns and
/// parenthesized patterns a bit differently.
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
    ResultString += P->getBodyName().str();
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

/// Emit logic to print the specified expression value with the given
/// description of the pattern involved.
void REPLChecker::generatePrintOfExpression(StringRef NameStr, Expr *E) {
  // Always print rvalues, not lvalues.
  E = TC.coerceToMaterializable(E);

  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();

  // Require a non-trivial set of print functions.
  if (requirePrintDecls())
    return;

  // Build function of type T->() which prints the operand.
  VarDecl *Arg = new (Context) ParamDecl(/*isLet=*/true,
                                         SourceLoc(), Identifier(),
                                         Loc, Context.getIdentifier("arg"),
                                         E->getType(), /*DC*/ nullptr);
  Pattern *ParamPat = new (Context) NamedPattern(Arg);
  ParamPat = new (Context) TypedPattern(ParamPat,
                                        TypeLoc::withoutLoc(Arg->getType()));
  TuplePatternElt elt{ParamPat};
  ParamPat = TuplePattern::create(Context, SourceLoc(), elt, SourceLoc());
  TC.typeCheckPattern(ParamPat, Arg->getDeclContext(), None);

  TopLevelCodeDecl *newTopLevel = new (Context) TopLevelCodeDecl(&SF);
  unsigned discriminator = TLC.claimNextClosureDiscriminator();

  ClosureExpr *CE =
      new (Context) ClosureExpr(ParamPat, SourceLoc(), SourceLoc(), TypeLoc(),
                                discriminator, newTopLevel);

  Type ParamTy = ParamPat->getType();
  ParamTy = ParamTy->getRelabeledType(TC.Context, { Identifier() });
  Type FuncTy = FunctionType::get(ParamTy, TupleType::getEmpty(Context));
  CE->setType(FuncTy);

  // Convert the pattern to a string we can print.
  llvm::SmallString<16> PrefixString;
  PrefixString += "// ";
  PrefixString += NameStr;
  PrefixString += " : ";
  PrefixString += E->getType()->getWithoutParens()->getString();
  PrefixString += " = ";

  // Unique the type string into an identifier since PrintLiteralString is
  // building an AST around the string that must persist beyond the lifetime of
  // PrefixString.
  auto TmpStr = Context.getIdentifier(PrefixString).str();

  StmtBuilder builder(*this, CE);

  builder.printLiteralString(TmpStr, Loc);

  builder.printReplExpr(Arg, Loc);

  // Typecheck the function.
  BraceStmt *Body = builder.createBodyStmt(Loc, EndLoc);
  CE->setBody(Body, false);
  TC.typeCheckClosureBody(CE);

  // If the caller didn't wrap the argument in parentheses or make it a tuple,
  // add the extra parentheses now.
  Expr *TheArg = E;
  Type Ty = ParenType::get(TC.Context, TheArg->getType());
  TheArg = new (TC.Context) ParenExpr(SourceLoc(), TheArg, SourceLoc(), false,
                                      Ty);

  Expr *TheCall = new (Context) CallExpr(CE, TheArg, /*Implicit=*/true);
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
  if (isa<ErrorType>(T) || isa<ModuleType>(T) || T->isVoid())
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
                                       StaticSpellingKind::None,
                                       E->getStartLoc(), metavarPat, E,
                                       /*conditional*/ false,
                                       &SF);
  SF.Decls.push_back(metavarBinding);

  // Finally, print the variable's value.
  E = TC.buildCheckedRefExpr(vd, &SF, E->getStartLoc(), /*Implicit=*/true);
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
    Expr *E = TC.buildCheckedRefExpr(NP->getDecl(), &SF, PBD->getStartLoc(),
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
                                       StaticSpellingKind::None,
                                       PBD->getStartLoc(), metavarPat,
                                       PBD->getInit(), /*conditional*/ false,
                                       &SF);

  auto MVBrace = BraceStmt::create(Context, metavarBinding->getStartLoc(),
                                   ASTNode(metavarBinding),
                                   metavarBinding->getEndLoc());

  auto *MVTLCD = new (Context) TopLevelCodeDecl(&SF, MVBrace);
  SF.Decls.push_back(MVTLCD);


  // Replace the initializer of PBD with a reference to our repl temporary.
  Expr *E = TC.buildCheckedRefExpr(vd, &SF,
                                   vd->getStartLoc(), /*Implicit=*/true);
  E = TC.coerceToMaterializable(E);
  PBD->setInit(E, /*checked=*/true);
  SF.Decls.push_back(PBTLCD);

  // Finally, print out the result, by referring to the repl temp.
  E = TC.buildCheckedRefExpr(vd, &SF, vd->getStartLoc(), /*Implicit=*/true);
  generatePrintOfExpression(PatternString, E);
}



/// processREPLTopLevel - This is called after we've parsed and typechecked some
/// new decls at the top level.  We inject code to print out expressions and
/// pattern bindings the are evaluated.
void TypeChecker::processREPLTopLevel(SourceFile &SF, TopLevelContext &TLC,
                                      unsigned FirstDecl) {
  // Move new declarations out.
  std::vector<Decl *> NewDecls(SF.Decls.begin()+FirstDecl, SF.Decls.end());
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

