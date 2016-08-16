//===--- TypeRepr.cpp - Swift Language Type Representation ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

SourceLoc TypeRepr::getLoc() const {
  switch (getKind()) {
#define TYPEREPR(CLASS, PARENT) \
case TypeReprKind::CLASS: \
return static_cast<const CLASS##TypeRepr*>(this)->getLocImpl();
#include "swift/AST/TypeReprNodes.def"
  }
  llvm_unreachable("unknown kind!");
}

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
void *TypeRepr::operator new(size_t Bytes, const ASTContext &C,
                             unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

Identifier ComponentIdentTypeRepr::getIdentifier() const {
  if (IdOrDecl.is<Identifier>())
    return IdOrDecl.get<Identifier>();

  return IdOrDecl.get<ValueDecl *>()->getName();
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
  // The type part of a NamedTypeRepr will get the callback.
  if (!isa<NamedTypeRepr>(this))
    Printer.printTypePre(TypeLoc(const_cast<TypeRepr *>(this)));
  SWIFT_DEFER {
    if (!isa<NamedTypeRepr>(this))
      Printer.printTypePost(TypeLoc(const_cast<TypeRepr *>(this)));
  };

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
    const ASTContext &Ctx;

  public:
    explicit CloneVisitor(const ASTContext &ctx) : Ctx(ctx) { }

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
  return new (Ctx) ArrayTypeRepr(visit(T->getBase()), T->getBrackets());
}

TypeRepr *CloneVisitor::visitDictionaryTypeRepr(DictionaryTypeRepr *T) {
  return new (Ctx) DictionaryTypeRepr(visit(T->getKey()), visit(T->getValue()),
                                      T->getColonLoc(), T->getBrackets());
}

TypeRepr *CloneVisitor::visitOptionalTypeRepr(OptionalTypeRepr *T) {
  return new (Ctx) OptionalTypeRepr(visit(T->getBase()), T->getQuestionLoc());
}

TypeRepr * CloneVisitor::visitImplicitlyUnwrappedOptionalTypeRepr(
             ImplicitlyUnwrappedOptionalTypeRepr *T) {
  return new (Ctx) ImplicitlyUnwrappedOptionalTypeRepr(visit(T->getBase()),
                                                       T->getExclamationLoc());
}

TypeRepr *CloneVisitor::visitTupleTypeRepr(TupleTypeRepr *T) {
  // Clone the tuple elements.
  auto elements = Ctx.Allocate<TypeRepr*>(T->getElements().size());
  for (unsigned argI : indices(elements)) {
    elements[argI] = visit(T->getElements()[argI]);
  }

  return new (Ctx) TupleTypeRepr(elements, T->getParens(), T->getEllipsisLoc(),
                                 T->getEllipsisIndex());
}

TypeRepr *CloneVisitor::visitNamedTypeRepr(NamedTypeRepr *T) {
  return new (Ctx) NamedTypeRepr(T->getName(), visit(T->getTypeRepr()),
                                 T->getNameLoc(), T->getUnderscoreLoc());
}

TypeRepr *CloneVisitor::visitProtocolCompositionTypeRepr(
                          ProtocolCompositionTypeRepr *T) {
  // Clone the protocols.
  auto protocols = Ctx.Allocate<IdentTypeRepr*>(T->getProtocols().size());
  for (unsigned argI : indices(protocols)) {
    protocols[argI] = cast<IdentTypeRepr>(visit(T->getProtocols()[argI]));
  }

  return new (Ctx) ProtocolCompositionTypeRepr(protocols,
                                               T->getStartLoc(),
                                               T->getCompositionRange());
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

TypeRepr *CloneVisitor::visitFixedTypeRepr(FixedTypeRepr *T) {
  return new (Ctx) FixedTypeRepr(T->getType(), T->getLoc());
}

TypeRepr *TypeRepr::clone(const ASTContext &ctx) const {
  CloneVisitor visitor(ctx);
  return visitor.visit(const_cast<TypeRepr *>(this));
}

void TypeRepr::visitTopLevelTypeReprs(
       llvm::function_ref<void(IdentTypeRepr *)> visitor) {
  TypeRepr *typeRepr = this;

  // Look through attributed type representations.
  while (auto attr = dyn_cast<AttributedTypeRepr>(typeRepr))
    typeRepr = attr->getTypeRepr();

  // Handle identifier type representations.
  if (auto ident = dyn_cast<IdentTypeRepr>(typeRepr)) {
    visitor(ident);
    return;
  }

  // Recurse into protocol compositions.
  if (auto composition = dyn_cast<ProtocolCompositionTypeRepr>(typeRepr)) {
    for (auto ident : composition->getProtocols())
      ident->visitTopLevelTypeReprs(visitor);
    return;
  }
}

void ErrorTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer << "<<error type>>";
}

void AttributedTypeRepr::printImpl(ASTPrinter &Printer,
                                   const PrintOptions &Opts) const {
  printAttrs(Printer, Opts);
  printTypeRepr(Ty, Printer, Opts);
}

void AttributedTypeRepr::printAttrs(llvm::raw_ostream &OS) const {
  StreamPrinter Printer(OS);
  printAttrs(Printer, PrintOptions());
}

void AttributedTypeRepr::printAttrs(ASTPrinter &Printer,
                                    const PrintOptions &Options) const {
  const TypeAttributes &Attrs = getAttrs();

  auto hasAttr = [&](TypeAttrKind K) -> bool {
    if (Options.excludeAttrKind(K))
      return false;
    return Attrs.has(K);
  };

  if (hasAttr(TAK_autoclosure)) {
    Printer.printAttrName("@autoclosure");
    Printer << " ";
  }
  if (hasAttr(TAK_escaping)) {
    Printer.printAttrName("@escaping");
    Printer << " ";
  }

  if (hasAttr(TAK_thin)) {
    Printer.printAttrName("@thin");
    Printer << " ";
  }
  if (hasAttr(TAK_thick)) {
    Printer.printAttrName("@thick");
    Printer << " ";
  }
  if (hasAttr(TAK_convention) && Attrs.convention.hasValue()) {
    Printer.printAttrName("@convention");
    Printer << "(" << Attrs.convention.getValue() << ") ";
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
  if (TypeDecl *TD = dyn_cast_or_null<TypeDecl>(getBoundDecl())) {
    if (auto MD = dyn_cast<ModuleDecl>(TD))
      Printer.printModuleRef(MD, getIdentifier());
    else
      Printer.printTypeRef(Type(), TD, getIdentifier());
  } else {
    Printer.printName(getIdentifier());
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
  Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
  printTypeRepr(ArgsTy, Printer, Opts);
  if (throws()) {
    Printer << " ";
    Printer.printKeyword("throws");
  }
  Printer << " -> ";
  Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
  printTypeRepr(RetTy, Printer, Opts);
  Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  Printer.printStructurePost(PrintStructureKind::FunctionType);
}

void ArrayTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer << "[";
  printTypeRepr(getBase(), Printer, Opts);
  Printer << "]";
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
                                     SourceLoc Ellipsis,
                                     unsigned EllipsisIdx) {
  assert(Ellipsis.isValid() ? EllipsisIdx < Elements.size()
                            : EllipsisIdx == Elements.size());
  return new (C) TupleTypeRepr(C.AllocateCopy(Elements),
                               Parens, Ellipsis, EllipsisIdx);
}

void TupleTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer.callPrintStructurePre(PrintStructureKind::TupleType);
  SWIFT_DEFER { Printer.printStructurePost(PrintStructureKind::TupleType); };

  Printer << "(";

  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (i) Printer << ", ";
    Printer.callPrintStructurePre(PrintStructureKind::TupleElement);
    printTypeRepr(Elements[i], Printer, Opts);
    Printer.printStructurePost(PrintStructureKind::TupleElement);

    if (hasEllipsis() && getEllipsisIndex() == i)
      Printer << "...";
  }
  Printer << ")";
}

void NamedTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  if (isNamedParameter()) {
    // Printing empty Identifier is same as printing '_'.
    Printer.printName(Identifier(), PrintNameContext::FunctionParameterExternal);
    if (!Id.empty()) {
      Printer << " ";
      Printer.printName(Id, PrintNameContext::FunctionParameterLocal);
    }
    Printer << ": ";
  } else {
    if (!Id.empty()) {
      Printer.printName(Id, PrintNameContext::TupleElement);
      Printer << ": ";
    }
  }
  printTypeRepr(Ty, Printer, Opts);
}

ProtocolCompositionTypeRepr *
ProtocolCompositionTypeRepr::create(ASTContext &C,
                                    ArrayRef<IdentTypeRepr *> Protocols,
                                    SourceLoc FirstTypeLoc,
                                    SourceRange CompositionRange) {
  return new (C) ProtocolCompositionTypeRepr(C.AllocateCopy(Protocols),
                                             FirstTypeLoc, CompositionRange);
}

void ProtocolCompositionTypeRepr::printImpl(ASTPrinter &Printer,
                                            const PrintOptions &Opts) const {
  if (Protocols.empty()) {
    Printer << "Any";
  } else {
    bool First = true;
    for (auto Proto : Protocols) {
      if (First)
        First = false;
      else
        Printer << " & ";
      printTypeRepr(Proto, Printer, Opts);
    }
  }
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
  Printer.printKeyword("inout");
  Printer << " ";
  printTypeRepr(Base, Printer, Opts);
}

void FixedTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  getType().print(Printer, Opts);
}
