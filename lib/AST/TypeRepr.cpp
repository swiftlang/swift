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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
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

static void printTypeRepr(const TypeRepr *TyR, ASTPrinter &Printer,
                          const PrintOptions &Opts) {
  if (TyR == nullptr)
    Printer << "<null>";
  else
    TyR->print(Printer, Opts);
}

void TypeRepr::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

void TypeRepr::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
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

namespace {
  class CloneVisitor : public TypeReprVisitor<CloneVisitor, TypeRepr *> {
    ASTContext &Ctx;

  public:
    explicit CloneVisitor(ASTContext &ctx) : Ctx(ctx) { }

#define TYPEREPR(CLASS, PARENT) \
    TypeRepr *visit##CLASS##TypeRepr(CLASS##TypeRepr* type);
#include "swift/AST/TypeReprNodes.def"
  };
}

TypeRepr *CloneVisitor::visitErrorTypeRepr(ErrorTypeRepr *T) {
  return new (Ctx) ErrorTypeRepr(T->getSourceRange());
}

TypeRepr *CloneVisitor::visitAttributedTypeRepr(AttributedTypeRepr *T) {
  return new (Ctx) AttributedTypeRepr(T->getAttrs(), visit(T->getTypeRepr()));
}

TypeRepr *CloneVisitor::visitSimpleIdentTypeRepr(SimpleIdentTypeRepr *T) {
  return new (Ctx) SimpleIdentTypeRepr(T->getIdLoc(), T->getIdentifier());
}

TypeRepr *CloneVisitor::visitGenericIdentTypeRepr(GenericIdentTypeRepr *T) {
  // Clone the generic arguments.
  auto genericArgs = Ctx.Allocate<TypeRepr*>(T->getGenericArgs().size());
  for (unsigned argI : indices(genericArgs)) {
    genericArgs[argI] = visit(T->getGenericArgs()[argI]);
  }
  return new (Ctx) GenericIdentTypeRepr(T->getIdLoc(), T->getIdentifier(),
                                        genericArgs, T->getAngleBrackets());
}

TypeRepr *CloneVisitor::visitCompoundIdentTypeRepr(CompoundIdentTypeRepr *T) {
  // Clone the components.
  auto components = Ctx.Allocate<ComponentIdentTypeRepr*>(T->Components.size());
  for (unsigned I : indices(components)) {
    components[I] = cast<ComponentIdentTypeRepr>(visit(T->Components[I]));
  }
  return new (Ctx) CompoundIdentTypeRepr(components);
}

TypeRepr *CloneVisitor::visitFunctionTypeRepr(FunctionTypeRepr *T) {
  return new (Ctx) FunctionTypeRepr(/*FIXME: Clone?*/T->getGenericParams(),
                                    visit(T->getArgsTypeRepr()),
                                    T->getThrowsLoc(),
                                    T->getArrowLoc(),
                                    visit(T->getResultTypeRepr()));
}

TypeRepr *CloneVisitor::visitArrayTypeRepr(ArrayTypeRepr *T) {
  return new (Ctx) ArrayTypeRepr(visit(T->getBase()), T->getSize(),
                                 T->getBrackets(), T->usesOldSyntax());
}

TypeRepr *CloneVisitor::visitDictionaryTypeRepr(DictionaryTypeRepr *T) {
  return new (Ctx) DictionaryTypeRepr(visit(T->getKey()), visit(T->getValue()),
                                      T->getColonLoc(), T->getBrackets());
}

TypeRepr *CloneVisitor::visitOptionalTypeRepr(OptionalTypeRepr *T) {
  return new (Ctx) OptionalTypeRepr(visit(T->getBase()), T->getQuestionLoc());
}

TypeRepr *
CloneVisitor::visitImplicitlyUnwrappedOptionalTypeRepr(ImplicitlyUnwrappedOptionalTypeRepr *T) {
  return new (Ctx) ImplicitlyUnwrappedOptionalTypeRepr(visit(T->getBase()),
                                             T->getExclamationLoc());
}

TypeRepr *CloneVisitor::visitTupleTypeRepr(TupleTypeRepr *T) {
  return new (Ctx) TupleTypeRepr(Ctx.AllocateCopy(T->getElements()),
                                 T->getParens(), T->getEllipsisLoc());
}

TypeRepr *CloneVisitor::visitNamedTypeRepr(NamedTypeRepr *T) {
  return new (Ctx) NamedTypeRepr(T->getName(), visit(T->getTypeRepr()),
                                 T->getNameLoc());
}

TypeRepr *CloneVisitor::visitProtocolCompositionTypeRepr(
                          ProtocolCompositionTypeRepr *T) {
  return new (Ctx) ProtocolCompositionTypeRepr(
                     Ctx.AllocateCopy(T->getProtocols()),
                     T->getProtocolLoc(),
                     T->getAngleBrackets());
}

TypeRepr *CloneVisitor::visitMetatypeTypeRepr(MetatypeTypeRepr *T) {
  return new (Ctx) MetatypeTypeRepr(visit(T->getBase()), T->getMetaLoc());
}

TypeRepr *CloneVisitor::visitProtocolTypeRepr(ProtocolTypeRepr *T) {
  return new (Ctx) ProtocolTypeRepr(visit(T->getBase()), T->getProtocolLoc());
}

TypeRepr *CloneVisitor::visitInOutTypeRepr(InOutTypeRepr *T) {
  return new (Ctx) InOutTypeRepr(visit(T->getBase()), T->getInOutLoc());
}


TypeRepr *TypeRepr::clone(ASTContext &ctx) const {
  CloneVisitor visitor(ctx);
  return visitor.visit(const_cast<TypeRepr *>(this));
}


void ErrorTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer << "<<error type>>";
}

void AttributedTypeRepr::printImpl(ASTPrinter &Printer,
                                   const PrintOptions &Opts) const {
  printAttrs(Printer);
  Printer << " ";
  printTypeRepr(Ty, Printer, Opts);
}

void AttributedTypeRepr::printAttrs(llvm::raw_ostream &OS) const {
  StreamPrinter Printer(OS);
  printAttrs(Printer);
}

void AttributedTypeRepr::printAttrs(ASTPrinter &Printer) const {
  const TypeAttributes &Attrs = getAttrs();
  if (Attrs.has(TAK_noreturn))     Printer << "@noreturn ";
  if (Attrs.has(TAK_objc_block))   Printer << "@objc_block ";
  if (Attrs.has(TAK_thin))         Printer << "@thin ";
  if (Attrs.has(TAK_thick))        Printer << "@thick ";
  if (Attrs.deprecatedCC.hasValue()) {
    Printer << "@cc(" << Attrs.deprecatedCC.getValue() << ")";
  }
  if (Attrs.convention.hasValue()) {
    Printer << "@convention(" << Attrs.convention.getValue() << ")";
  }
}

IdentTypeRepr *IdentTypeRepr::create(ASTContext &C,
                                ArrayRef<ComponentIdentTypeRepr *> Components) {
  assert(!Components.empty());
  if (Components.size() == 1)
    return Components.front();

  return new (C) CompoundIdentTypeRepr(C.AllocateCopy(Components));
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
    printTypeRepr(Arg, Printer, Opts);
  }
  Printer << ">";
}

void ComponentIdentTypeRepr::printImpl(ASTPrinter &Printer,
                                       const PrintOptions &Opts) const {
  if (Module *Mod = getBoundModule()) {
    Printer.printModuleRef(Mod, getIdentifier());
  } else if (Type Ty = getBoundType()) {
    if (auto NTD = Ty->getAnyNominal())
      Printer.printTypeRef(NTD, getIdentifier());
    else
      Printer << getIdentifier().str();
  } else {
    Printer << getIdentifier().str();
  }
  if (auto GenIdT = dyn_cast<GenericIdentTypeRepr>(this))
    printGenericArgs(Printer, Opts, GenIdT->getGenericArgs());
}

void CompoundIdentTypeRepr::printImpl(ASTPrinter &Printer,
                                      const PrintOptions &Opts) const {
  printTypeRepr(Components.front(), Printer, Opts);
  for (auto C : Components.slice(1)) {
    Printer << ".";
    printTypeRepr(C, Printer, Opts);
  }
}

void FunctionTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  printTypeRepr(ArgsTy, Printer, Opts);
  Printer << " -> ";
  printTypeRepr(RetTy, Printer, Opts);
}

void ArrayTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  if (usesOldSyntax()) {
    printTypeRepr(Base, Printer, Opts);
    Printer << "[";
    if (auto size = getSize())
      size->getExpr()->print(Printer, Opts);
    Printer << "]";
  } else {
    Printer << "[";
    printTypeRepr(Base, Printer, Opts);
    Printer << "]";
  }
}

void DictionaryTypeRepr::printImpl(ASTPrinter &Printer,
                                   const PrintOptions &Opts) const {
  Printer << "[";
  printTypeRepr(Key, Printer, Opts);
  Printer << " : ";
  printTypeRepr(Value, Printer, Opts);
  Printer << "]";
}

void OptionalTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  printTypeRepr(Base, Printer, Opts);
  Printer << "?";
}

void ImplicitlyUnwrappedOptionalTypeRepr::printImpl(ASTPrinter &Printer,
                                          const PrintOptions &Opts) const {
  printTypeRepr(Base, Printer, Opts);
  Printer << "!";
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
    printTypeRepr(Elements[i], Printer, Opts);
  }
  if (hasEllipsis())
    Printer << "...";
  Printer << ")";
}

void NamedTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  if (!Id.empty()) {
    Printer.printName(Id);
    Printer << ": ";
  }
  printTypeRepr(Ty, Printer, Opts);
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
    printTypeRepr(Proto, Printer, Opts);
  }
  Printer << ">";
}

void MetatypeTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  printTypeRepr(Base, Printer, Opts);
  Printer << ".Type";
}

void ProtocolTypeRepr::printImpl(ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  printTypeRepr(Base, Printer, Opts);
  Printer << ".Protocol";
}


void InOutTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer << "inout ";
  printTypeRepr(Base, Printer, Opts);
}
