//===--- TypeRefinementContext.cpp - Swift Refinement Context ---*- C++ -*-===//
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
// This file implements the TypeRefinementContext class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;

TypeRefinementContext::TypeRefinementContext(ASTContext &Ctx, IntroNode Node,
                                             TypeRefinementContext *Parent,
                                             SourceRange SrcRange,
                                             const VersionRange &Versions)
    : Node(Node), SrcRange(SrcRange), PotentialVersions(Versions) {
  if (Parent) {
    assert(SrcRange.isValid());
    Parent->addChild(this);
  }
  Ctx.addDestructorCleanup(Children);
}

TypeRefinementContext *
TypeRefinementContext::createRoot(SourceFile *SF,
                                  const VersionRange &Versions) {
  assert(SF);

  ASTContext &Ctx = SF->getASTContext();
  return new (Ctx)
      TypeRefinementContext(Ctx, SF,
                            /*Parent=*/nullptr, SourceRange(), Versions);
}

TypeRefinementContext *
TypeRefinementContext::createForDecl(ASTContext &Ctx, Decl *D,
                                     TypeRefinementContext *Parent,
                                     const VersionRange &Versions,
                                     SourceRange SrcRange) {
  assert(D);
  assert(Parent);
  return new (Ctx)
      TypeRefinementContext(Ctx, D, Parent, SrcRange, Versions);
}

TypeRefinementContext *
TypeRefinementContext::createForIfStmtThen(ASTContext &Ctx, IfStmt *S,
                                           TypeRefinementContext *Parent,
                                           const VersionRange &Versions) {
  assert(S);
  assert(Parent);
  return new (Ctx) TypeRefinementContext(
      Ctx, S, Parent, S->getThenStmt()->getSourceRange(), Versions);
}

TypeRefinementContext *
TypeRefinementContext::createForConditionFollowingQuery(ASTContext &Ctx,
                                 AvailabilityQueryExpr *QE,
                                 const StmtConditionElement &LastElement,
                                 TypeRefinementContext *Parent,
                                 const VersionRange &Versions) {
  assert(QE);
  assert(Parent);
  SourceRange Range(QE->getEndLoc(), LastElement.getEndLoc());
  return new (Ctx) TypeRefinementContext(Ctx, QE, Parent, Range, Versions);
}

TypeRefinementContext *
TypeRefinementContext::createForRequireStmtFallthrough(ASTContext &Ctx,
                                  RequireStmt *RS,
                                  BraceStmt *ContainingBraceStmt,
                                  TypeRefinementContext *Parent,
                                  const VersionRange &Versions) {
  assert(RS);
  assert(ContainingBraceStmt);
  assert(Parent);
  SourceRange Range(RS->getEndLoc(), ContainingBraceStmt->getEndLoc());
  return new (Ctx) TypeRefinementContext(Ctx, RS, Parent, Range, Versions);
}

// Only allow allocation of TypeRefinementContext using the allocator in
// ASTContext.
void *TypeRefinementContext::operator new(size_t Bytes, ASTContext &C,
                                          unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

TypeRefinementContext *
TypeRefinementContext::findMostRefinedSubContext(SourceLoc Loc,
                                                 SourceManager &SM) {
  assert(Loc.isValid());
  
  if (SrcRange.isValid() && !SM.rangeContainsTokenLoc(SrcRange, Loc))
    return nullptr;

  // For the moment, we perform a linear search here, but we can and should
  // do something more efficient.
  for (TypeRefinementContext *Child : Children) {
    if (auto *Found = Child->findMostRefinedSubContext(Loc, SM)) {
      return Found;
    }
  }

  // Loc is in this context's range but not in any child's, so this context
  // must be the inner-most context.
  return this;
}

void TypeRefinementContext::dump(SourceManager &SrcMgr) const {
  dump(llvm::errs(), SrcMgr);
}

void TypeRefinementContext::dump(raw_ostream &OS, SourceManager &SrcMgr) const {
  print(OS, SrcMgr, 0);
  OS << '\n';
}

SourceLoc TypeRefinementContext::getIntroductionLoc() const {
  switch (getReason()) {
  case Reason::Decl:
    return Node.get<Decl *>()->getLoc();

  case Reason::IfStmtThenBranch:
    return cast<IfStmt>(Node.get<Stmt *>())->getIfLoc();

  case Reason::ConditionFollowingAvailabilityQuery:
    return cast<AvailabilityQueryExpr>(Node.get<Expr *>())->getLoc();

  case Reason::RequireStmtFallthrough:
    return cast<RequireStmt>(Node.get<Stmt *>())->getRequireLoc();

  case Reason::Root:
    return SourceLoc();
  }
}

void TypeRefinementContext::print(raw_ostream &OS, SourceManager &SrcMgr,
                                  unsigned Indent) const {
  OS.indent(Indent);
  OS << "(" << getReasonName(getReason());

  OS << " versions=" << PotentialVersions.getAsString();

  if (getReason() == Reason::Decl) {
    Decl *D = getIntroductionNode().get<Decl *>();
    OS << " decl=";
    if (auto VD = dyn_cast<ValueDecl>(D)) {
      VD->dumpRef(OS);
    } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
      OS << "extension." << ED->getExtendedType().getString();
    }
  }

  auto R = getSourceRange();
  if (R.isValid()) {
    OS << " src_range=";
    R.print(OS, SrcMgr, /*PrintText=*/false);
  }

  for (TypeRefinementContext *Child : Children) {
    OS << '\n';
    Child->print(OS, SrcMgr, Indent + 2);
  }
  OS.indent(Indent);
  OS << ")";
}

TypeRefinementContext::Reason TypeRefinementContext::getReason() const {
  if (Node.is<Decl *>()) {
    return Reason::Decl;
  } else if (Node.is<Expr *>()) {
    Expr *E = Node.get<Expr *>();
    if (isa<AvailabilityQueryExpr>(E)) {
      return Reason::ConditionFollowingAvailabilityQuery;
    }
  } else if (Node.is<Stmt *>()) {
    Stmt *S = Node.get<Stmt *>();
    if (isa<IfStmt>(S)) {
      // We will need an additional bit to discriminate when we add
      // refinement contexts for Else branches.
      return Reason::IfStmtThenBranch;
    }

    if (isa<RequireStmt>(S)) {
      return Reason::RequireStmtFallthrough;
    }
  } else if (Node.is<SourceFile *>()) {
    return Reason::Root;
  }
  llvm_unreachable("Unhandled introduction node");
}

StringRef TypeRefinementContext::getReasonName(Reason R) {
  switch (R) {
  case Reason::Root:
    return "root";

  case Reason::Decl:
    return "decl";

  case Reason::IfStmtThenBranch:
    return "if_then";

  case Reason::ConditionFollowingAvailabilityQuery:
    return "condition_following_availability";

  case Reason::RequireStmtFallthrough:
    return "require_fallthrough";
  }
}
