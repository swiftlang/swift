//===--- TypeRepr.cpp - Swift Language Type Representation ------*- C++ -*-===//
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
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
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

void TypeRepr::print(raw_ostream &OS) const {
  if (this == nullptr) {
    OS << "<null>";
    return;
  }

  switch (getKind()) {
#define TYPEREPR(CLASS, PARENT) \
  case TypeReprKind::CLASS: { \
    auto Ty = static_cast<const CLASS##TypeRepr*>(this); \
    return Ty->printImpl(OS); \
  }
#include "swift/AST/TypeReprNodes.def"
  }
  llvm_unreachable("unknown kind!");
}

void ErrorTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << "<<error type>>";
}

void AttributedTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  printAttrs(OS);
  OS << ' ' << Ty;
}

void AttributedTypeRepr::printAttrs(llvm::raw_ostream &OS) const {
  const TypeAttributes &Attrs = getAttrs();
  if (Attrs.has(TAK_inout))        OS << "@inout ";
  if (Attrs.has(TAK_auto_closure)) OS << "@auto_closure ";
  if (Attrs.has(TAK_thin))         OS << "@thin ";
  if (Attrs.has(TAK_noreturn))     OS << "@noreturn ";
  if (Attrs.has(TAK_objc_block))   OS << "@objc_block ";
  if (Attrs.cc.hasValue()) {
    switch (Attrs.cc.getValue()) {
    case AbstractCC::C:            OS << "@cc(cdecl)"; break;
    case AbstractCC::ObjCMethod:   OS << "@cc(objc_method)"; break;
    case AbstractCC::Freestanding: OS << "@cc(freestanding)"; break;
    case AbstractCC::Method:       OS << "@cc(method)"; break;
    }
  }
}

IdentTypeRepr *IdentTypeRepr::create(ASTContext &C,
                                     ArrayRef<Component> Components) {
  return new (C) IdentTypeRepr(C.AllocateCopy(Components));
}

IdentTypeRepr *IdentTypeRepr::createSimple(ASTContext &C, SourceLoc Loc,
                                           Identifier Id) {
  IdentTypeRepr::Component IdTypeComponent(Loc, Id, {});
  return create(C, llvm::makeArrayRef(IdTypeComponent));
}

static void printGenericArgs(raw_ostream &OS, ArrayRef<TypeRepr *> Args) {
  if (Args.empty())
    return;

  OS << '<';
  bool First = true;
  for (auto Arg : Args) {
    if (First)
      First = false;
    else
      OS << ", ";
    OS << Arg;
  }
  OS << '>';
}

void IdentTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << Components[0].getIdentifier();
  printGenericArgs(OS, Components[0].getGenericArgs());

  for (const Component &C : Components.slice(1, Components.size()-1)) {
    OS << '.' << C.getIdentifier().get();
    printGenericArgs(OS, C.getGenericArgs());
  }
}

void FunctionTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << ArgsTy << " -> " << RetTy;
}

void ArrayTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << Base << '[';
  if (Size)
    Size->getExpr()->print(OS);
  OS << ']';
}

void OptionalTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << Base << '?';
}

TupleTypeRepr *TupleTypeRepr::create(ASTContext &C,
                                     ArrayRef<TypeRepr *> Elements,
                                     SourceRange Parens,
                                     SourceLoc Ellipsis) {
  return new (C) TupleTypeRepr(C.AllocateCopy(Elements),
                               Parens, Ellipsis);
}

void TupleTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << '(';

  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (i) OS << ", ";
    OS << Elements[i];
  }
  if (hasEllipsis())
    OS << "...";
  OS << ')';
}

void NamedTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  if (!Id.empty())
    OS << Id << " : ";
  OS << Ty;
}

ProtocolCompositionTypeRepr *
ProtocolCompositionTypeRepr::create(ASTContext &C,
                                    ArrayRef<IdentTypeRepr *> Protocols,
                                    SourceLoc ProtocolLoc,
                                    SourceRange AngleBrackets) {
  return new (C) ProtocolCompositionTypeRepr(C.AllocateCopy(Protocols),
                                             ProtocolLoc, AngleBrackets);
}

void ProtocolCompositionTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << "protocol<";
  bool First = true;
  for (auto Proto : Protocols) {
    if (First)
      First = false;
    else
      OS << ", ";
    OS << Proto;
  }
  OS << '>';
}

void MetaTypeTypeRepr::printImpl(llvm::raw_ostream &OS) const {
  OS << Base << ".metatype";
}
