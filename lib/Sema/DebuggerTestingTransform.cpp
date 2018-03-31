//===--- DebuggerTestingTransform.cpp - Transform for debugger testing ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file This transform inserts instrumentation which the debugger can use to
/// test its expression evaluation facilities.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Stmt.h"
#include "swift/Subsystems.h"

#include "TypeChecker.h"

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

/// Instrument decls with sanity-checks which the debugger can evaluate.
class DebuggerTestingTransform : public ASTWalker {
  ASTContext &Ctx;
  DiscriminatorFinder &DF;
  std::vector<DeclContext *> LocalDeclContextStack;

public:
  DebuggerTestingTransform(ASTContext &Ctx, DiscriminatorFinder &DF)
      : Ctx(Ctx), DF(DF) {}

  bool walkToDeclPre(Decl *D) override {
    pushLocalDeclContext(D);

    // Whitelist the kinds of decls to transform.
    // TODO: Expand the set of decls visited here.
    if (auto *FD = dyn_cast<AbstractFunctionDecl>(D))
      return !FD->isImplicit() && FD->getBody();
    if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D))
      return !TLCD->isImplicit() && TLCD->getBody();
    return false;
  }

  bool walkToDeclPost(Decl *D) override {
    popLocalDeclContext(D);
    return true;
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    pushLocalDeclContext(E);

    // Whitelist the kinds of exprs to transform.
    // TODO: Expand the set of exprs visited here.
    if (auto *AE = dyn_cast<AssignExpr>(E))
      return insertCheckExpect(AE, AE->getDest());

    return {true, E};
  }

  Expr *walkToExprPost(Expr *E) override {
    popLocalDeclContext(E);
    return E;
  }

private:
  /// Return N as a local DeclContext if possible, or return nullptr.
  DeclContext *getLocalDeclContext(ASTNode N) const {
    DeclContext *DC = N.getAsDeclContext();
    return (DC && DC->isLocalContext()) ? DC : nullptr;
  }

  /// If N is a local DeclContext, push it onto the context stack.
  void pushLocalDeclContext(ASTNode N) {
    if (auto *LDC = getLocalDeclContext(N))
      LocalDeclContextStack.push_back(LDC);
  }

  /// If N is a local DeclContext, pop it off the context stack.
  void popLocalDeclContext(ASTNode N) {
    if (getLocalDeclContext(N))
      LocalDeclContextStack.pop_back();
  }

  /// Get the current local DeclContext. This is used to create closures in the
  /// right context.
  DeclContext *getCurrentDeclContext() const {
    assert(!LocalDeclContextStack.empty() && "Missing decl context");
    return LocalDeclContextStack.back();
  }

  /// Try to extract a DeclRefExpr from the expression.
  DeclRefExpr *extractDecl(Expr *E) {
    while (!isa<DeclRefExpr>(E)) {
      // TODO: Try more ways to extract interesting decl refs.
      if (auto *Subscript = dyn_cast<SubscriptExpr>(E))
        E = Subscript->getBase();
      else if (auto *InOut = dyn_cast<InOutExpr>(E))
        E = InOut->getSubExpr();
      else
        return nullptr;
    }
    return cast<DeclRefExpr>(E);
  }

  /// Attempt to create a functionally-equivalent replacement for OriginalExpr,
  /// given that DstExpr identifies the target of some mutating update, and that
  /// DstExpr is a subexpression of OriginalExpr.
  ///
  /// The return value contains 1) a flag indicating whether or not to
  /// recursively transform the children of the transformed expression, and 2)
  /// the transformed expression itself.
  std::pair<bool, Expr *> insertCheckExpect(Expr *OriginalExpr, Expr *DstExpr) {
    auto *DstDRE = extractDecl(DstExpr);
    if (!DstDRE)
      return {true, OriginalExpr};

    ValueDecl *DstDecl = DstDRE->getDecl();
    if (!DstDecl->hasName())
      return {true, OriginalExpr};

    // Don't capture variables which aren't default-initialized.
    if (auto *VD = dyn_cast<VarDecl>(DstDecl))
      if (!VD->getParentInitializer() && !VD->isInOut())
        return {true, OriginalExpr};

    // Rewrite the original expression into this:
    // call
    //   closure {
    //     $OriginalExpr
    //     checkExpect("$Varname", _stringForPrintObject($Varname))
    //   }

    // Create "$Varname".
    llvm::SmallString<256> DstNameBuf;
    DeclName DstDN = DstDecl->getFullName();
    StringRef DstName = Ctx.AllocateCopy(DstDN.getString(DstNameBuf));
    assert(!DstName.empty() && "Varname must be non-empty");
    Expr *Varname = new (Ctx) StringLiteralExpr(DstName, SourceRange());
    Varname->setImplicit(true);

    // Create _stringForPrintObject($Varname).
    auto *PODeclRef = new (Ctx)
        UnresolvedDeclRefExpr(Ctx.getIdentifier("_stringForPrintObject"),
                              DeclRefKind::Ordinary, DeclNameLoc());
    Expr *POArgs[] = {DstDRE};
    Identifier POLabels[] = {Identifier()};
    auto *POCall = CallExpr::createImplicit(Ctx, PODeclRef, POArgs, POLabels);
    POCall->setThrows(false);

    // Create the call to checkExpect.
    Identifier CheckExpectLabels[] = {Identifier(), Identifier()};
    Expr *CheckExpectArgs[] = {Varname, POCall};
    UnresolvedDeclRefExpr *CheckExpectDRE = new (Ctx)
        UnresolvedDeclRefExpr(Ctx.getIdentifier("_debuggerTestingCheckExpect"),
                              DeclRefKind::Ordinary, DeclNameLoc());
    auto *CheckExpectExpr = CallExpr::createImplicit(
        Ctx, CheckExpectDRE, CheckExpectArgs, CheckExpectLabels);
    CheckExpectExpr->setThrows(false);

    // Create the closure.
    TypeChecker TC{Ctx};
    auto *Params = ParameterList::createEmpty(Ctx);
    auto *Closure = new (Ctx)
        ClosureExpr(Params, SourceLoc(), SourceLoc(), SourceLoc(), TypeLoc(),
                    DF.getNextDiscriminator(), getCurrentDeclContext());
    Closure->setImplicit(true);

    // TODO: Save and return the value of $OriginalExpr.
    ASTNode ClosureElements[] = {OriginalExpr, CheckExpectExpr};
    auto *ClosureBody = BraceStmt::create(Ctx, SourceLoc(), ClosureElements,
                                          SourceLoc(), /*Implicit=*/true);
    Closure->setBody(ClosureBody, /*isSingleExpression=*/false);

    // Call the closure.
    auto *ClosureCall = CallExpr::createImplicit(Ctx, Closure, {}, {});
    ClosureCall->setThrows(false);

    // TODO: typeCheckExpression() seems to assign types to everything here,
    // but may not be sufficient in some cases.
    Expr *FinalExpr = ClosureCall;
    if (!TC.typeCheckExpression(FinalExpr, getCurrentDeclContext()))
      llvm::report_fatal_error("Could not type-check instrumentation");

    // Captures have to be computed after the closure is type-checked. This
    // ensures that the type checker can infer <noescape> for captured values.
    TC.computeCaptures(Closure);

    return {false, FinalExpr};
  }
};

} // end anonymous namespace

void swift::performDebuggerTestingTransform(SourceFile &SF) {
  // Walk over all decls in the file to find the next available closure
  // discriminator.
  DiscriminatorFinder DF;
  for (Decl *D : SF.Decls)
    D->walk(DF);

  // Instrument the decls with checkExpect() sanity-checks.
  for (Decl *D : SF.Decls) {
    DebuggerTestingTransform Transform{D->getASTContext(), DF};
    D->walk(Transform);
    swift::verify(D);
  }
}
