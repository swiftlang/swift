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
TypeRefinementContext::createRoot(ASTContext &Ctx, SourceFile *SF,
                                  const VersionRange &Versions) {
  assert(SF);
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
