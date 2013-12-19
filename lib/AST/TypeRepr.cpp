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

#include "swift/AST/ASTPrinter.h"
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

void TypeRepr::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

void TypeRepr::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  if (this == nullptr) {
    Printer << "<null>";
    return;
  }

  switch (getKind()) {
#define TYPEREPR(CLASS, PARENT) \
  case TypeReprKind::CLASS: { \
    auto Ty = static_cast<const CLASS##TypeRepr*>(this); \
    return Ty->printImpl(Printer, Opts); \
  }
#include "swift/AST/TypeReprNodes.def"
  }
  llvm_unreachable("unknown kind!");
}

void ErrorTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer << "<<error type>>";
}

void AttributedTypeRepr::printImpl(ASTPrinter &Printer,
                                   const PrintOptions &Opts) const {
  printAttrs(Printer);
  Printer << " ";
  Ty->print(Printer, Opts);
}

void AttributedTypeRepr::printAttrs(llvm::raw_ostream &OS) const {
  StreamPrinter Printer(OS);
  printAttrs(Printer);
}

void AttributedTypeRepr::printAttrs(ASTPrinter &Printer) const {
  const TypeAttributes &Attrs = getAttrs();
  if (Attrs.has(TAK_inout))        Printer << "@inout ";
  if (Attrs.has(TAK_auto_closure)) Printer << "@auto_closure ";
  if (Attrs.has(TAK_thin))         Printer << "@thin ";
  if (Attrs.has(TAK_noreturn))     Printer << "@noreturn ";
  if (Attrs.has(TAK_objc_block))   Printer << "@objc_block ";
  if (Attrs.cc.hasValue()) {
    switch (Attrs.cc.getValue()) {
    case AbstractCC::C:             Printer << "@cc(cdecl)"; break;
    case AbstractCC::ObjCMethod:    Printer << "@cc(objc_method)"; break;
    case AbstractCC::Freestanding:  Printer << "@cc(freestanding)"; break;
    case AbstractCC::Method:        Printer << "@cc(method)"; break;
    case AbstractCC::WitnessMethod: Printer << "@cc(witness_method)"; break;
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

static void printGenericArgs(ASTPrinter &Printer, const PrintOptions &Opts,
                             ArrayRef<TypeRepr *> Args) {
  if (Args.empty())
    return;

  Printer << "<";
  bool First = true;
  for (auto Arg : Args) {
    if (First)
      First = false;
    else
      Printer << ", ";
    Arg->print(Printer, Opts);
  }
  Printer << ">";
}

void IdentTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  bool isFirst = true;
  for (const Component &C : Components) {
    if (!isFirst)
      Printer << ".";
    else
      isFirst = false;

    if (Module *Mod = C.getBoundModule()) {
      Printer.printModuleRef(Mod, C.getIdentifier().str());
    } else if (Type Ty = C.getBoundType()) {
      if (auto NTD = Ty->getAnyNominal())
        Printer.printTypeRef(NTD, C.getIdentifier().str());
      else
        Printer << C.getIdentifier().str();
    } else {
      Printer << C.getIdentifier().str();
    }
    printGenericArgs(Printer, Opts, C.getGenericArgs());
  }
}

void FunctionTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  ArgsTy->print(Printer, Opts);
  Printer << " -> ";
  RetTy->print(Printer, Opts);
}

void ArrayTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Base->print(Printer, Opts);
  Printer << "[";
  if (Size)
    Size->getExpr()->print(Printer, Opts);
  Printer << "]";
}

void OptionalTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  Base->print(Printer, Opts);
  Printer << "?";
}

TupleTypeRepr *TupleTypeRepr::create(ASTContext &C,
                                     ArrayRef<TypeRepr *> Elements,
                                     SourceRange Parens,
                                     SourceLoc Ellipsis) {
  return new (C) TupleTypeRepr(C.AllocateCopy(Elements),
                               Parens, Ellipsis);
}

void TupleTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer << "(";

  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (i) Printer << ", ";
    Elements[i]->print(Printer, Opts);
  }
  if (hasEllipsis())
    Printer << "...";
  Printer << ")";
}

void NamedTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  if (!Id.empty())
    Printer << Id.str() << " : ";
  Ty->print(Printer, Opts);
}

ProtocolCompositionTypeRepr *
ProtocolCompositionTypeRepr::create(ASTContext &C,
                                    ArrayRef<IdentTypeRepr *> Protocols,
                                    SourceLoc ProtocolLoc,
                                    SourceRange AngleBrackets) {
  return new (C) ProtocolCompositionTypeRepr(C.AllocateCopy(Protocols),
                                             ProtocolLoc, AngleBrackets);
}

void ProtocolCompositionTypeRepr::printImpl(ASTPrinter &Printer,
                                            const PrintOptions &Opts) const {
  Printer << "protocol<";
  bool First = true;
  for (auto Proto : Protocols) {
    if (First)
      First = false;
    else
      Printer << ", ";
    Proto->print(Printer, Opts);
  }
  Printer << ">";
}

void MetatypeTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  Base->print(Printer, Opts);
  Printer << ".metatype";
}
