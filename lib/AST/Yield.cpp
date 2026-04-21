//===--- Yield.cpp - Functions & closures yields --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Yield class, the YieldList class and support
// logic.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/AST/YieldList.h"
#include "swift/Basic/Assertions.h"
#include <iterator>
using namespace swift;

YieldList *YieldList::create(const ASTContext &C, SourceLoc LParenLoc,
                             ArrayRef<TypeRepr *> yieldTypes,
                             SourceLoc RParenLoc) {
  ASSERT(LParenLoc.isValid() == RParenLoc.isValid() &&
         "Either both paren locs are valid or neither are");
  ASSERT(!yieldTypes.empty());

  auto byteSize = totalSizeToAlloc<Yield>(yieldTypes.size());
  auto rawMem = C.Allocate(byteSize, alignof(YieldList));

  // Placement initialize the YieldList and the Yield's.
  auto YL = ::new (rawMem) YieldList(LParenLoc, yieldTypes.size(), RParenLoc);

  std::uninitialized_copy(yieldTypes.begin(), yieldTypes.end(),
                          YL->getArray().begin());

  return YL;
}

YieldList *YieldList::create(const ASTContext &C,
                             ArrayRef<AnyFunctionType::Yield> yields) {
  ASSERT(!yields.empty());

  auto byteSize = totalSizeToAlloc<Yield>(yields.size());
  auto rawMem = C.Allocate(byteSize, alignof(YieldList));

  // Placement initialize the YieldList and the Yield's.
  auto YL = ::new (rawMem) YieldList(SourceLoc(), yields.size(), SourceLoc());

  std::uninitialized_copy(yields.begin(), yields.end(), YL->getArray().begin());

  return YL;
}

YieldList *YieldList::create(const ASTContext &C, ArrayRef<Yield> yields) {
  ASSERT(!yields.empty());

  auto byteSize = totalSizeToAlloc<Yield>(yields.size());
  auto rawMem = C.Allocate(byteSize, alignof(YieldList));

  // Placement initialize the YieldList and the Yield's.
  auto YL = ::new (rawMem) YieldList(SourceLoc(), yields.size(), SourceLoc());

  std::uninitialized_copy(yields.begin(), yields.end(), YL->getArray().begin());

  return YL;
}

/// Return the full source range of this yield list.
SourceRange YieldList::getSourceRange() const {
  // If we have locations for the parens, then they define our range.
  if (LParenLoc.isValid())
    return {LParenLoc, RParenLoc};

  // Otherwise, try the first and last yields.
  if (size() != 0 && get(0).getTypeRepr() && getArray().back().getTypeRepr()) {
    auto Start = get(0).getTypeRepr()->getStartLoc();
    auto End = getArray().back().getTypeRepr()->getEndLoc();
    if (Start.isValid() && End.isValid())
      return {Start, End};
  }

  return SourceRange();
}

Type Yield::getInterfaceType(const FuncDecl *parent) const {
  // Figure out our position in parent's yield list
  size_t idx = std::distance(parent->getYields()->getArray().data(), this);
  ASSERT(idx < parent->getYields()->size());

  auto mutableParent = const_cast<FuncDecl *>(parent);

  auto &ctx = mutableParent->getASTContext();
  return ctx.evaluator(YieldsTypeRequest{mutableParent, unsigned(idx)},
                       [&ctx]() { return ErrorType::get(ctx); });
}
