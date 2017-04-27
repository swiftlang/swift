//===--- TypeRepr.cpp - Swift Language Type Representation ----------------===//
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
// This file implements the TypeRepr and related classes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
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
  Printer.printTypePre(TypeLoc(const_cast<TypeRepr *>(this)));
  SWIFT_DEFER {
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
} // end anonymous namespace

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
  // FIXME: Avoid this stash vector.
  SmallVector<TypeRepr *, 8> elements;
  elements.reserve(T->getNumElements());
  for (auto *arg : T->getElements())
    elements.push_back(visit(arg));
  return TupleTypeRepr::create(Ctx, elements, T->getParens(),
                               T->getElementNames(), T->getElementNameLocs(),
                               T->getUnderscoreLocs(),
                               T->getEllipsisLoc(), T->getEllipsisIndex());
}

TypeRepr *CloneVisitor::visitCompositionTypeRepr(CompositionTypeRepr *T) {
  // Clone the protocols.
  auto types = Ctx.Allocate<TypeRepr*>(T->getTypes().size());
  for (unsigned argI : indices(types)) {
    types[argI] = cast<TypeRepr>(visit(T->getTypes()[argI]));
  }

  return new (Ctx) CompositionTypeRepr(types,
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

TypeRepr *CloneVisitor::visitSILBoxTypeRepr(SILBoxTypeRepr *type) {
  SmallVector<SILBoxTypeRepr::Field, 4> cloneFields;
  SmallVector<TypeRepr *, 4> cloneArgs;
  
  for (auto &field : type->getFields())
    cloneFields.push_back({field.VarOrLetLoc, field.Mutable,
                           visit(field.FieldType)});
  for (auto *arg : type->getGenericArguments())
    cloneArgs.push_back(visit(arg));
  
  return new (Ctx) SILBoxTypeRepr(/*FIXME: Clone?*/type->getGenericParams(),
                                type->getLBraceLoc(),
                                Ctx.AllocateCopy(cloneFields),
                                type->getRBraceLoc(),
                                type->getArgumentLAngleLoc(),
                                Ctx.AllocateCopy(cloneArgs),
                                type->getArgumentRAngleLoc());
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
  if (auto composition = dyn_cast<CompositionTypeRepr>(typeRepr)) {
    for (auto type: composition->getTypes())
      type->visitTopLevelTypeReprs(visitor);
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

  if (!Options.SkipParameterTypeAttributes) {
    if (hasAttr(TAK_autoclosure))
      Printer.printSimpleAttr("@autoclosure") << " ";
    if (hasAttr(TAK_escaping))
      Printer.printSimpleAttr("@escaping") << " ";
  }

  if (hasAttr(TAK_thin))
    Printer.printSimpleAttr("@thin") << " ";
  if (hasAttr(TAK_thick))
    Printer.printSimpleAttr("@thick") << " ";

  if (hasAttr(TAK_convention) && Attrs.convention.hasValue()) {
    Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
    Printer.printAttrName("@convention");
    Printer << "(" << Attrs.convention.getValue() << ")";
    Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
    Printer << " ";
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
  interleave(Args, [&](TypeRepr *Arg) { printTypeRepr(Arg, Printer, Opts); },
             [&] { Printer << ", "; });
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

TupleTypeRepr::TupleTypeRepr(ArrayRef<TypeRepr *> Elements, SourceRange Parens,
                             ArrayRef<Identifier> ElementNames,
                             ArrayRef<SourceLoc> ElementNameLocs,
                             ArrayRef<SourceLoc> underscoreLocs,
                             SourceLoc Ellipsis, unsigned EllipsisIdx)
    : TypeRepr(TypeReprKind::Tuple), NumElements(Elements.size()),
      Parens(Parens) {

  TupleTypeReprBits.NameStatus = ElementNames.empty() ? NotNamed
                               : underscoreLocs.empty() ? HasNames : HasLabels;
  TupleTypeReprBits.HasEllipsis = Ellipsis.isValid();

  // Copy elements.
  std::uninitialized_copy(Elements.begin(), Elements.end(),
                          getTrailingObjects<TypeRepr *>());

  // Copy elements names.
  std::uninitialized_copy(ElementNames.begin(), ElementNames.end(),
                          getTrailingObjects<Identifier>());

  // Copy elements names locations.
  std::uninitialized_copy(ElementNameLocs.begin(), ElementNameLocs.end(),
                          getTrailingObjects<SourceLoc>());

  // Copy elements underscore locations.
  std::uninitialized_copy(underscoreLocs.begin(), underscoreLocs.end(),
                          getTrailingObjects<SourceLoc>() +
                              ElementNameLocs.size());

  // Set ellipsis location and index.
  if (Ellipsis.isValid())
    getTrailingObjects<SourceLocAndIdx>()[0] = {Ellipsis, EllipsisIdx};
}

TupleTypeRepr *TupleTypeRepr::create(const ASTContext &C,
                                     ArrayRef<TypeRepr *> Elements,
                                     SourceRange Parens,
                                     ArrayRef<Identifier> ElementNames,
                                     ArrayRef<SourceLoc> ElementNameLocs,
                                     ArrayRef<SourceLoc> underscoreLocs,
                                     SourceLoc Ellipsis, unsigned EllipsisIdx) {
  assert(ElementNames.size() == 0 ||
         ElementNames.size() == Elements.size());
  assert(ElementNameLocs.size() == ElementNames.size());
  assert(underscoreLocs.size() == 0 ||
         underscoreLocs.size() == Elements.size());
  assert(Ellipsis.isValid() ? EllipsisIdx < Elements.size()
                            : EllipsisIdx == Elements.size());

  size_t size =
    totalSizeToAlloc<TypeRepr *, Identifier, SourceLoc, SourceLocAndIdx>(
      Elements.size(),
      ElementNames.size(),
      ElementNameLocs.size() + underscoreLocs.size(),
      Ellipsis.isValid() ? 1 : 0);
  void *mem = C.Allocate(size, alignof(TupleTypeRepr));
  return new (mem) TupleTypeRepr(Elements, Parens, ElementNames,
                                 ElementNameLocs, underscoreLocs,
                                 Ellipsis, EllipsisIdx);
}


TupleTypeRepr *TupleTypeRepr::createEmpty(const ASTContext &C,
                                          SourceRange Parens) {
  return create(C, {}, Parens,
      /*ElementNames=*/{}, /*ElementNameLocs=*/{}, /*underscoreLocs=*/{},
      /*Ellipsis=*/SourceLoc(), /*EllipsisIdx=*/0);
}

SILBoxTypeRepr *SILBoxTypeRepr::create(ASTContext &C,
                      GenericParamList *GenericParams,
                      SourceLoc LBraceLoc, ArrayRef<Field> Fields,
                      SourceLoc RBraceLoc,
                      SourceLoc ArgLAngleLoc, ArrayRef<TypeRepr *> GenericArgs,
                      SourceLoc ArgRAngleLoc) {
  return new (C) SILBoxTypeRepr(GenericParams,
                                LBraceLoc, C.AllocateCopy(Fields), RBraceLoc,
                                ArgLAngleLoc, C.AllocateCopy(GenericArgs),
                                ArgRAngleLoc);
}

SourceLoc SILBoxTypeRepr::getStartLocImpl() const {
  if (GenericParams && GenericParams->getSourceRange().isValid())
    return GenericParams->getSourceRange().Start;
  return LBraceLoc;
}
SourceLoc SILBoxTypeRepr::getEndLocImpl() const {
  if (ArgRAngleLoc.isValid())
    return ArgRAngleLoc;
  return RBraceLoc;
}
SourceLoc SILBoxTypeRepr::getLocImpl() const {
  return LBraceLoc;
}

void TupleTypeRepr::printImpl(ASTPrinter &Printer,
                              const PrintOptions &Opts) const {
  Printer.callPrintStructurePre(PrintStructureKind::TupleType);
  SWIFT_DEFER { Printer.printStructurePost(PrintStructureKind::TupleType); };

  Printer << "(";

  for (unsigned i = 0, e = NumElements; i != e; ++i) {
    if (i) Printer << ", ";
    Printer.callPrintStructurePre(PrintStructureKind::TupleElement);
    auto name = getElementName(i);
    if (isNamedParameter(i)) {
      // Printing empty Identifier is same as printing '_'.
      Printer.printName(Identifier(),
                        PrintNameContext::FunctionParameterExternal);
      if (!name.empty()) {
        Printer << " ";
        Printer.printName(name, PrintNameContext::FunctionParameterLocal);
      }
      Printer << ": ";
    } else {
      if (!name.empty()) {
        Printer.printName(name, PrintNameContext::TupleElement);
        Printer << ": ";
      }
    }
    printTypeRepr(getElement(i), Printer, Opts);
    Printer.printStructurePost(PrintStructureKind::TupleElement);

    if (hasEllipsis() && getEllipsisIndex() == i)
      Printer << "...";
  }

  Printer << ")";
}

CompositionTypeRepr *
CompositionTypeRepr::create(ASTContext &C,
                            ArrayRef<TypeRepr *> Types,
                            SourceLoc FirstTypeLoc,
                            SourceRange CompositionRange) {
  return new (C) CompositionTypeRepr(C.AllocateCopy(Types),
                                     FirstTypeLoc, CompositionRange);
}

void CompositionTypeRepr::printImpl(ASTPrinter &Printer,
                                    const PrintOptions &Opts) const {
  if (Types.empty()) {
    Printer << "Any";
  } else {
    interleave(Types, [&](TypeRepr *T) { printTypeRepr(T, Printer, Opts); },
               [&] { Printer << " & "; });
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

void SILBoxTypeRepr::printImpl(ASTPrinter &Printer,
                               const PrintOptions &Opts) const {
  // TODO
  Printer.printKeyword("sil_box");
}
