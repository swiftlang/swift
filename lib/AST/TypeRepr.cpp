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
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

#define TYPEREPR(Id, _) \
  static_assert(IsTriviallyDestructible<Id##TypeRepr>::value, \
                "TypeReprs are BumpPtrAllocated; the d'tor is never called");
#include "swift/AST/TypeReprNodes.def"

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

  return IdOrDecl.get<TypeDecl *>()->getName();
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
  SmallVector<TypeRepr*, 8> genericArgs;
  genericArgs.reserve(T->getGenericArgs().size());
  for (auto &arg : T->getGenericArgs()) {
    genericArgs.push_back(visit(arg));
  }
  return GenericIdentTypeRepr::create(Ctx, T->getIdLoc(), T->getIdentifier(),
                                        genericArgs, T->getAngleBrackets());
}

TypeRepr *CloneVisitor::visitCompoundIdentTypeRepr(CompoundIdentTypeRepr *T) {
  // Clone the components.
  SmallVector<ComponentIdentTypeRepr*, 8> components;
  components.reserve(T->getComponents().size());
  for (auto &component : T->getComponents()) {
    components.push_back(cast<ComponentIdentTypeRepr>(visit(component)));
  }
  return CompoundIdentTypeRepr::create(Ctx, components);
}

TypeRepr *CloneVisitor::visitFunctionTypeRepr(FunctionTypeRepr *T) {
  return new (Ctx) FunctionTypeRepr(
                              /*FIXME: Clone?*/T->getGenericParams(),
                              cast<TupleTypeRepr>(visit(T->getArgsTypeRepr())),
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
  SmallVector<TupleTypeReprElement, 8> elements;
  elements.reserve(T->getNumElements());
  for (auto arg : T->getElements()) {
    arg.Type = visit(arg.Type);
    elements.push_back(arg);
  }
  return TupleTypeRepr::create(Ctx, elements,
                               T->getParens(),
                               T->getEllipsisLoc(),
                               T->getEllipsisIndex());
}

TypeRepr *CloneVisitor::visitCompositionTypeRepr(CompositionTypeRepr *T) {
  // Clone the protocols.
  SmallVector<TypeRepr*, 8> types;
  types.reserve(T->getTypes().size());
  for (auto &type : T->getTypes()) {
    types.push_back(cast<TypeRepr>(visit(type)));
  }

  return CompositionTypeRepr::create(Ctx, types, T->getStartLoc(),
                                     T->getCompositionRange());
}

TypeRepr *CloneVisitor::visitMetatypeTypeRepr(MetatypeTypeRepr *T) {
  return new (Ctx) MetatypeTypeRepr(visit(T->getBase()), T->getMetaLoc());
}

TypeRepr *CloneVisitor::visitProtocolTypeRepr(ProtocolTypeRepr *T) {
  return new (Ctx) ProtocolTypeRepr(visit(T->getBase()), T->getProtocolLoc());
}

TypeRepr *CloneVisitor::visitInOutTypeRepr(InOutTypeRepr *T) {
  return new (Ctx) InOutTypeRepr(visit(T->getBase()), T->getSpecifierLoc());
}

TypeRepr *CloneVisitor::visitSharedTypeRepr(SharedTypeRepr *T) {
  return new (Ctx) SharedTypeRepr(visit(T->getBase()), T->getSpecifierLoc());
}

TypeRepr *CloneVisitor::visitOwnedTypeRepr(OwnedTypeRepr *T) {
  return new (Ctx) OwnedTypeRepr(visit(T->getBase()), T->getSpecifierLoc());
}

TypeRepr *CloneVisitor::visitFixedTypeRepr(FixedTypeRepr *T) {
  return new (Ctx) FixedTypeRepr(T->getType(), T->getLoc());
}

TypeRepr *CloneVisitor::visitSILBoxTypeRepr(SILBoxTypeRepr *type) {
  SmallVector<SILBoxTypeRepr::Field, 4> cloneFields;
  SmallVector<TypeRepr *, 4> cloneArgs;
  
  for (auto &field : type->getFields())
    cloneFields.push_back({field.getLoc(), field.isMutable(),
                           visit(field.getFieldType())});
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

  if (hasAttr(TAK_autoclosure))
    Printer.printSimpleAttr("@autoclosure") << " ";
  if (hasAttr(TAK_escaping))
    Printer.printSimpleAttr("@escaping") << " ";

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

  return CompoundIdentTypeRepr::create(C, Components);
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
  if (auto *TD = dyn_cast_or_null<TypeDecl>(getBoundDecl())) {
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
  printTypeRepr(getComponents().front(), Printer, Opts);
  for (auto C : getComponents().slice(1)) {
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

TupleTypeRepr::TupleTypeRepr(ArrayRef<TupleTypeReprElement> Elements,
                             SourceRange Parens,
                             SourceLoc Ellipsis, unsigned EllipsisIdx)
    : TypeRepr(TypeReprKind::Tuple), Parens(Parens) {
  Bits.TupleTypeRepr.HasEllipsis = Ellipsis.isValid();
  Bits.TupleTypeRepr.NumElements = Elements.size();

  // Copy elements.
  std::uninitialized_copy(Elements.begin(), Elements.end(),
                          getTrailingObjects<TupleTypeReprElement>());

  // Set ellipsis location and index.
  if (Ellipsis.isValid()) {
    getTrailingObjects<SourceLocAndIdx>()[0] = {Ellipsis, EllipsisIdx};
  }
}

TupleTypeRepr *TupleTypeRepr::create(const ASTContext &C,
                                     ArrayRef<TupleTypeReprElement> Elements,
                                     SourceRange Parens,
                                     SourceLoc Ellipsis, unsigned EllipsisIdx) {
  assert(Ellipsis.isValid() ? EllipsisIdx < Elements.size()
                            : EllipsisIdx == Elements.size());

  size_t size =
    totalSizeToAlloc<TupleTypeReprElement, SourceLocAndIdx>(
      Elements.size(), Ellipsis.isValid() ? 1 : 0);
  void *mem = C.Allocate(size, alignof(TupleTypeRepr));
  return new (mem) TupleTypeRepr(Elements, Parens,
                                 Ellipsis, EllipsisIdx);
}

TupleTypeRepr *TupleTypeRepr::createEmpty(const ASTContext &C,
                                          SourceRange Parens) {
  return create(C, {}, Parens,
      /*Ellipsis=*/SourceLoc(), /*EllipsisIdx=*/0);
}

GenericIdentTypeRepr *GenericIdentTypeRepr::create(const ASTContext &C,
                                                   SourceLoc Loc,
                                                   Identifier Id,
                                                ArrayRef<TypeRepr*> GenericArgs,
                                                   SourceRange AngleBrackets) {
  auto size = totalSizeToAlloc<TypeRepr*>(GenericArgs.size());
  auto mem = C.Allocate(size, alignof(GenericIdentTypeRepr));
  return new (mem) GenericIdentTypeRepr(Loc, Id, GenericArgs, AngleBrackets);
}

CompoundIdentTypeRepr *CompoundIdentTypeRepr::create(const ASTContext &C,
                                 ArrayRef<ComponentIdentTypeRepr*> Components) {
  auto size = totalSizeToAlloc<ComponentIdentTypeRepr*>(Components.size());
  auto mem = C.Allocate(size, alignof(CompoundIdentTypeRepr));
  return new (mem) CompoundIdentTypeRepr(Components);
}

SILBoxTypeRepr *SILBoxTypeRepr::create(ASTContext &C,
                      GenericParamList *GenericParams,
                      SourceLoc LBraceLoc, ArrayRef<Field> Fields,
                      SourceLoc RBraceLoc,
                      SourceLoc ArgLAngleLoc, ArrayRef<TypeRepr *> GenericArgs,
                      SourceLoc ArgRAngleLoc) {
  auto size = totalSizeToAlloc<Field, TypeRepr*>(Fields.size(),
                                                 GenericArgs.size());
  auto mem = C.Allocate(size, alignof(SILBoxTypeRepr));
  return new (mem) SILBoxTypeRepr(GenericParams, LBraceLoc, Fields, RBraceLoc,
                                  ArgLAngleLoc, GenericArgs, ArgRAngleLoc);
}

SourceLoc FunctionTypeRepr::getStartLocImpl() const {
  return ArgsTy->getStartLoc();
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

  for (unsigned i = 0, e = Bits.TupleTypeRepr.NumElements; i != e; ++i) {
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
    printTypeRepr(getElementType(i), Printer, Opts);
    Printer.printStructurePost(PrintStructureKind::TupleElement);

    if (hasEllipsis() && getEllipsisIndex() == i)
      Printer << "...";
  }

  Printer << ")";
}

CompositionTypeRepr *CompositionTypeRepr::create(const ASTContext &C,
                                                 ArrayRef<TypeRepr *> Types,
                                                 SourceLoc FirstTypeLoc,
                                                 SourceRange CompositionRange) {
  auto size = totalSizeToAlloc<TypeRepr*>(Types.size());
  auto mem = C.Allocate(size, alignof(CompositionTypeRepr));
  return new (mem) CompositionTypeRepr(Types, FirstTypeLoc, CompositionRange);
}

void CompositionTypeRepr::printImpl(ASTPrinter &Printer,
                                    const PrintOptions &Opts) const {
  if (getTypes().empty()) {
    Printer << "Any";
  } else {
    interleave(getTypes(), [&](TypeRepr *T) { printTypeRepr(T, Printer, Opts);},
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


void SpecifierTypeRepr::printImpl(ASTPrinter &Printer,
                                  const PrintOptions &Opts) const {
  switch (getKind()) {
  case TypeReprKind::InOut:
    Printer.printKeyword("inout");
    break;
  case TypeReprKind::Shared:
    Printer.printKeyword("__shared");
    break;
  case TypeReprKind::Owned:
    Printer.printKeyword("__owned");
    break;
  default:
    llvm_unreachable("unknown specifier type repr");
    break;
  }
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

// See swift/Basic/Statistic.h for declaration: this enables tracing
// TypeReprs, is defined here to avoid too much layering violation / circular
// linkage dependency.

struct TypeReprTraceFormatter : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const {
    if (!Entity)
      return;
    const TypeRepr *TR = static_cast<const TypeRepr *>(Entity);
    TR->print(OS);
  }
  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const {
    if (!Entity)
      return;
    const TypeRepr *TR = static_cast<const TypeRepr *>(Entity);
    TR->getSourceRange().print(OS, *SM, false);
  }
};

static TypeReprTraceFormatter TF;

template<>
const UnifiedStatsReporter::TraceFormatter*
FrontendStatsTracer::getTraceFormatter<const TypeRepr *>() {
  return &TF;
}
