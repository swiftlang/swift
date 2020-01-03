//===--- TypeCheckREPL.cpp - Type Checking for the REPL -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements REPL-specific semantic analysis rules.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Expr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/Parse/LocalContext.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {

/// Find available closure discriminators.
///
/// The parser typically takes care of assigning unique discriminators to
/// closures, but the parser is unavailable to this transform.
class DiscriminatorFinder : public ASTWalker {
  unsigned NextDiscriminator = 0;

public:
  Expr *walkToExprPost(Expr *E) override {
    auto *ACE = dyn_cast<AbstractClosureExpr>(E);
    if (!ACE)
      return E;

    unsigned Discriminator = ACE->getDiscriminator();
    assert(Discriminator != AbstractClosureExpr::InvalidDiscriminator &&
           "Existing closures should have valid discriminators");
    if (Discriminator >= NextDiscriminator)
      NextDiscriminator = Discriminator + 1;
    return E;
  }

  // Get the next available closure discriminator.
  unsigned getNextDiscriminator() {
    if (NextDiscriminator == AbstractClosureExpr::InvalidDiscriminator)
      llvm::report_fatal_error("Out of valid closure discriminators");
    return NextDiscriminator++;
  }
};

struct REPLContext {
  ASTContext &Context;
  SourceFile &SF;
  SmallVector<ValueDecl *, 4> PrintDecls;
  SmallVector<ValueDecl *, 4> DebugPrintlnDecls;

  REPLContext(SourceFile &SF) : Context(SF.getASTContext()), SF(SF) {}

  bool requirePrintDecls() {
    if (!PrintDecls.empty() && !DebugPrintlnDecls.empty())
      return false;

    auto *stdlib = TypeChecker::getStdlibModule(&SF);
    {
      DeclNameRef Id(Context.getIdentifier("_replPrintLiteralString"));
      auto lookup = TypeChecker::lookupUnqualified(stdlib, Id, SourceLoc());
      if (!lookup)
        return true;
      for (auto result : lookup)
        PrintDecls.push_back(result.getValueDecl());
    }
    {
      DeclNameRef Id(Context.getIdentifier("_replDebugPrintln"));
      auto lookup = TypeChecker::lookupUnqualified(stdlib, Id, SourceLoc());
      if (!lookup)
        return true;
      for (auto result : lookup)
        DebugPrintlnDecls.push_back(result.getValueDecl());
    }

    return false;
  }
};

class StmtBuilder {
  REPLContext &C;
  ASTContext &Context;
  DeclContext *DC;
  SmallVector<ASTNode, 8> Body;

public:
  StmtBuilder(REPLContext &C, DeclContext *DC)
      : C(C), Context(C.Context), DC(DC) {
    assert(DC);
  }
  StmtBuilder(StmtBuilder &parent)
      : C(parent.C), Context(C.Context), DC(parent.DC) {}

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
    return TypeChecker::buildRefExpr(C.PrintDecls, DC, DeclNameLoc(loc),
                                     /*Implicit=*/true,
                                     FunctionRefKind::Compound);
  }

  Expr *buildDebugPrintlnRefExpr(SourceLoc loc) {
    assert(!C.DebugPrintlnDecls.empty());
    return TypeChecker::buildRefExpr(C.DebugPrintlnDecls, DC, DeclNameLoc(loc),
                                     /*Implicit=*/true,
                                     FunctionRefKind::Compound);
  }
};
} // unnamed namespace

void StmtBuilder::printLiteralString(StringRef Str, SourceLoc Loc) {
  Expr *PrintFn = buildPrintRefExpr(Loc);
  Expr *PrintStr = new (Context) StringLiteralExpr(Str, Loc);
  addToBody(CallExpr::createImplicit(Context, PrintFn, { PrintStr }, { }));
}

void StmtBuilder::printReplExpr(VarDecl *Arg, SourceLoc Loc) {
  Expr *DebugPrintlnFn = buildDebugPrintlnRefExpr(Loc);
  Expr *ArgRef = TypeChecker::buildRefExpr(
      Arg, DC, DeclNameLoc(Loc), /*Implicit=*/true, FunctionRefKind::Compound);
  addToBody(CallExpr::createImplicit(Context, DebugPrintlnFn, { ArgRef }, { }));
}

static VarDecl *getObviousDeclFromExpr(Expr *E) {
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
    for (unsigned i = 0, e = P->getNumElements(); i != e; ++i) {
      visit(P->getElement(i).getPattern());
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
} // end anonymous namespace

namespace {
  class REPLChecker : public REPLContext {
    DiscriminatorFinder &DF;

    /// The index of the next response metavariable to bind to a REPL result.
    unsigned NextResponseVariableIndex = 0;

  public:
    REPLChecker(SourceFile &SF, DiscriminatorFinder &DF)
        : REPLContext(SF), DF(DF) {}

    void processREPLTopLevelExpr(Expr *E);
    void processREPLTopLevelPatternBinding(PatternBindingDecl *PBD);
  private:
    void generatePrintOfExpression(StringRef name, Expr *E);
    Identifier getNextResponseVariableName(DeclContext *DC);
  };
} // end anonymous namespace

/// Emit logic to print the specified expression value with the given
/// description of the pattern involved.
void REPLChecker::generatePrintOfExpression(StringRef NameStr, Expr *E) {
  // Always print rvalues, not lvalues.
  E = TypeChecker::coerceToRValue(Context, E);

  SourceLoc Loc = E->getStartLoc();
  SourceLoc EndLoc = E->getEndLoc();

  // Require a non-trivial set of print functions.
  if (requirePrintDecls())
    return;

  TopLevelCodeDecl *newTopLevel = new (Context) TopLevelCodeDecl(&SF);

  // Build function of type T->() which prints the operand.
  auto *Arg = new (Context) ParamDecl(
      SourceLoc(), SourceLoc(), Identifier(), Loc,
      Context.getIdentifier("arg"), /*DC*/ newTopLevel);
  Arg->setInterfaceType(E->getType());
  Arg->setSpecifier(ParamSpecifier::Default);
  auto params = ParameterList::createWithoutLoc(Arg);

  unsigned discriminator = DF.getNextDiscriminator();

  ClosureExpr *CE =
      new (Context) ClosureExpr(SourceRange(), nullptr, params, SourceLoc(),
                                SourceLoc(), SourceLoc(), TypeLoc(),
                                discriminator, newTopLevel);

  SmallVector<AnyFunctionType::Param, 1> args;
  params->getParams(args);
  CE->setType(FunctionType::get(args, TupleType::getEmpty(Context)));

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

  TypeChecker::typeCheckClosureBody(CE);
  TypeChecker::computeCaptures(CE);

  auto *TheCall = CallExpr::createImplicit(Context, CE, { E }, { });
  TheCall->getArg()->setType(AnyFunctionType::composeInput(Context, args, false));
  TheCall->setType(Context.TheEmptyTupleType);

  // Inject the call into the top level stream by wrapping it with a TLCD.
  auto *BS = BraceStmt::create(Context, Loc, ASTNode(TheCall),
                               EndLoc);
  newTopLevel->setBody(BS);
  TypeChecker::checkTopLevelErrorHandling(newTopLevel);

  SF.Decls.push_back(newTopLevel);
}

/// When we see an expression in a TopLevelCodeDecl in the REPL, process it,
/// adding the proper decls back to the top level of the file.
void REPLChecker::processREPLTopLevelExpr(Expr *E) {
  CanType T = E->getType()->getCanonicalType();

  // Don't try to print invalid expressions, module exprs, or void expressions.
  if (T->hasError() || isa<ModuleType>(T) || T->isVoid())
    return;

  // Okay, we need to print this expression.  We generally do this by creating a
  // REPL metavariable (e.g. r4) to hold the result, so it can be referred to
  // in the future.  However, if this is a direct reference to a decl (e.g. "x")
  // then don't create a repl metavariable.
  if (VarDecl *d = getObviousDeclFromExpr(E)) {
    generatePrintOfExpression(d->getName().str(), E);
    return;
  }

  // Remove the expression from being in the list of decls to execute, we're
  // going to reparent it.
  auto TLCD = cast<TopLevelCodeDecl>(SF.Decls.back());

  E = TypeChecker::coerceToRValue(Context, E);

  // Create the meta-variable, let the typechecker name it.
  Identifier name = getNextResponseVariableName(&SF);
  VarDecl *vd = new (Context) VarDecl(/*IsStatic*/false, VarDecl::Introducer::Let,
                                      /*IsCaptureList*/false, E->getStartLoc(),
                                      name, &SF);
  vd->setInterfaceType(E->getType());
  SF.Decls.push_back(vd);

  // Create a PatternBindingDecl to bind the expression into the decl.
  Pattern *metavarPat = new (Context) NamedPattern(vd);
  metavarPat->setType(E->getType());

  PatternBindingDecl *metavarBinding = PatternBindingDecl::create(
      Context, /*StaticLoc*/ SourceLoc(), StaticSpellingKind::None,
      /*VarLoc*/ E->getStartLoc(), metavarPat, /*EqualLoc*/ SourceLoc(), E,
      TLCD);

  // Overwrite the body of the existing TopLevelCodeDecl.
  TLCD->setBody(BraceStmt::create(Context,
                                  metavarBinding->getStartLoc(),
                                  ASTNode(metavarBinding),
                                  metavarBinding->getEndLoc(),
                                  /*implicit*/true));

  // Finally, print the variable's value.
  E = TypeChecker::buildCheckedRefExpr(vd, &SF, DeclNameLoc(E->getStartLoc()),
                                       /*Implicit=*/true);
  generatePrintOfExpression(vd->getName().str(), E);
}

/// processREPLTopLevelPatternBinding - When we see a new PatternBinding parsed
/// into the REPL, process it by generating code to print it out.
void REPLChecker::processREPLTopLevelPatternBinding(PatternBindingDecl *PBD) {
  // If there is no initializer for the new variable, don't auto-print it.
  // This would just cause a confusing definite initialization error.  Some
  // day we will do some high level analysis of uninitialized variables
  // (rdar://15157729) but until then, output a specialized error.
  for (auto entryIdx : range(PBD->getNumPatternEntries())) {
    auto *entryInit = PBD->getInit(entryIdx);
    if (!entryInit) {
      PBD->diagnose(diag::repl_must_be_initialized);
      continue;
    }

    auto *pattern = PBD->getPattern(entryIdx);
    
    llvm::SmallString<16> PatternString;
    PatternBindingPrintLHS(PatternString).visit(pattern);

    // If the bound pattern is a single value, use a DeclRefExpr on the
    // underlying Decl to print it.
    if (auto *NP = dyn_cast<NamedPattern>(pattern->
                                          getSemanticsProvidingPattern())) {
      Expr *E = TypeChecker::buildCheckedRefExpr(
          NP->getDecl(), &SF, DeclNameLoc(PBD->getStartLoc()),
          /*Implicit=*/true);
      generatePrintOfExpression(PatternString, E);
      continue;
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
    Identifier name = getNextResponseVariableName(SF.getParentModule());
    VarDecl *vd = new (Context) VarDecl(/*IsStatic*/false,
                                        VarDecl::Introducer::Let,
                                        /*IsCaptureList*/false,
                                        PBD->getStartLoc(), name, &SF);
    vd->setInterfaceType(pattern->getType());
    SF.Decls.push_back(vd);

    // Create a PatternBindingDecl to bind the expression into the decl.
    Pattern *metavarPat = new (Context) NamedPattern(vd);
    metavarPat->setType(vd->getType());
    auto *metavarBinding = PatternBindingDecl::create(
        Context, /*StaticLoc*/ SourceLoc(), StaticSpellingKind::None,
        /*VarLoc*/ PBD->getStartLoc(), metavarPat, /*EqualLoc*/ SourceLoc(),
        entryInit, &SF);

    auto MVBrace = BraceStmt::create(Context, metavarBinding->getStartLoc(),
                                     ASTNode(metavarBinding),
                                     metavarBinding->getEndLoc());
    
    auto *MVTLCD = new (Context) TopLevelCodeDecl(&SF, MVBrace);
    SF.Decls.push_back(MVTLCD);
    
    
    // Replace the initializer of PBD with a reference to our repl temporary.
    Expr *E = TypeChecker::buildCheckedRefExpr(vd, &SF,
                                               DeclNameLoc(vd->getStartLoc()),
                                               /*Implicit=*/true);
    E = TypeChecker::coerceToRValue(Context, E);
    PBD->setInit(entryIdx, E);
    SF.Decls.push_back(PBTLCD);
    
    // Finally, print out the result, by referring to the repl temp.
    E = TypeChecker::buildCheckedRefExpr(vd, &SF,
                                         DeclNameLoc(vd->getStartLoc()),
                                         /*Implicit=*/true);
    generatePrintOfExpression(PatternString, E);
  }
}

Identifier REPLChecker::getNextResponseVariableName(DeclContext *DC) {
  llvm::SmallString<4> namebuf;
  Identifier ident;

  bool nameUsed = false;
  do {
    namebuf.clear();
    llvm::raw_svector_ostream names(namebuf);
    names << "r" << NextResponseVariableIndex++;

    ident = Context.getIdentifier(names.str());
    nameUsed = (bool)TypeChecker::lookupUnqualified(DC, DeclNameRef(ident),
                                                    SourceLoc());
  } while (nameUsed);

  return ident;
}

/// processREPLTopLevel - This is called after we've parsed and typechecked some
/// new decls at the top level.  We inject code to print out expressions and
/// pattern bindings the are evaluated.
void TypeChecker::processREPLTopLevel(SourceFile &SF, unsigned FirstDecl) {
  // Walk over all decls in the file to find the next available closure
  // discriminator.
  DiscriminatorFinder DF;
  for (Decl *D : SF.Decls)
    D->walk(DF);

  // Move new declarations out.
  std::vector<Decl *> NewDecls(SF.Decls.begin()+FirstDecl, SF.Decls.end());
  SF.Decls.resize(FirstDecl);

  REPLChecker RC(SF, DF);

  // Loop over each of the new decls, processing them, adding them back to
  // the Decls list.
  for (Decl *D : NewDecls) {
    SF.Decls.push_back(D);

    auto *TLCD = dyn_cast<TopLevelCodeDecl>(D);
    if (!TLCD || TLCD->getBody()->getElements().empty())
      continue;

    auto Entry = TLCD->getBody()->getFirstElement();

    // Check to see if the TLCD has an expression that we have to transform.
    if (auto *E = Entry.dyn_cast<Expr*>())
      RC.processREPLTopLevelExpr(E);
    else if (auto *D = Entry.dyn_cast<Decl*>())
      if (auto *PBD = dyn_cast<PatternBindingDecl>(D))
        RC.processREPLTopLevelPatternBinding(PBD);

    TypeChecker::contextualizeTopLevelCode(TLCD);
  }

  SF.clearLookupCache();
}
