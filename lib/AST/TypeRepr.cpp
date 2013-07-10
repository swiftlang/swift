//===--- TypeRepr.h - Swift Language Type Representation --------*- C++ -*-===//
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
// This file implements the TypeRepr and related classes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/TypeRepr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/Expr.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;

SourceLoc TypeRepr::getStartLoc() const {
  switch (getKind()) {
#define TYPEREPR(CLASS, PARENT) \
  case TypeReprKind::CLASS: \
    return static_cast<const CLASS##TypeRepr*>(this)->getStartLocImpl();
#include "swift/AST/TypeReprNodes.def"
  }
  llvm_unreachable("unknown kind!");
}
SourceLoc TypeRepr::getEndLoc() const {
  switch (getKind()) {
#define TYPEREPR(CLASS, PARENT) \
  case TypeReprKind::CLASS: \
    return static_cast<const CLASS##TypeRepr*>(this)->getEndLocImpl();
#include "swift/AST/TypeReprNodes.def"
  }
  llvm_unreachable("unknown kind!");
}
SourceRange TypeRepr::getSourceRange() const {
  switch (getKind()) {
#define TYPEREPR(CLASS, PARENT) \
  case TypeReprKind::CLASS: { \
    auto Ty = static_cast<const CLASS##TypeRepr*>(this); \
    return SourceRange(Ty->getStartLocImpl(), Ty->getEndLocImpl()); \
  }
#include "swift/AST/TypeReprNodes.def"
  }
  llvm_unreachable("unknown kind!");
}

/// Standard allocator for TypeReprs.
void *TypeRepr::operator new(size_t Bytes, ASTContext &C, unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

IdentTypeRepr *IdentTypeRepr::create(ASTContext &C,
                                     ArrayRef<Component> Components) {
  return new (C) IdentTypeRepr(C.AllocateCopy(Components));
}

TupleTypeRepr *TupleTypeRepr::create(ASTContext &C,
                                     ArrayRef<TypeRepr *> Elements,
                                     SourceRange Parens,
                                     SourceLoc Ellipsis) {
  return new (C) TupleTypeRepr(C.AllocateCopy(Elements),
                               Parens, Ellipsis);
}

CompositeTypeRepr *
CompositeTypeRepr::create(ASTContext &C,
                          ArrayRef<IdentTypeRepr *> Protocols,
                          SourceLoc ProtocolLoc,
                          SourceRange AngleBrackets) {
  return new (C) CompositeTypeRepr(C.AllocateCopy(Protocols),
                                   ProtocolLoc, AngleBrackets);
}

SourceLoc NamedTypeRepr::getEndLocImpl() const {
  if (Init)
    return Init->getExpr()->getEndLoc();
  return Ty->getEndLoc();
}
