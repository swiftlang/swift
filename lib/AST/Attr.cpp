//===--- Attr.cpp - Swift Language Attr ASTs ------------------------------===//
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
//  This file implements routines relating to declaration attributes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Attr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;


// Only allow allocation of attributes using the allocator in ASTContext.
void *AttributeBase::operator new(size_t Bytes, ASTContext &C,
                                  unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

/// A statically-allocated empty set of attributes.
const DeclAttributes Decl::EmptyAttrs;

DeclAttributes &Decl::getMutableAttrs() {
  // If we don't have mutable attribute storage yet, allocate some.
  if (&getAttrs() == &EmptyAttrs)
    Attrs = getASTContext().Allocate<DeclAttributes>();
  return *const_cast<DeclAttributes*>(&getAttrs());
}

const AvailabilityAttr *DeclAttributes::getUnavailable() const {
  for (auto Attr : *this)
    if (auto AvAttr = dyn_cast<AvailabilityAttr>(Attr)) {
      // FIXME: Unify unavailabilty checking with that in MiscDiagnostics.cpp.
      if (AvAttr->Platform.empty())
        if (AvAttr->IsUnvailable)
          return AvAttr;
    }
  return nullptr;
}

void DeclAttributes::print(llvm::raw_ostream &OS) const {
  StreamPrinter P(OS);
  PrintOptions PO = PrintOptions::printEverything();
  print(P, PO);
}

void DeclAttributes::print(ASTPrinter &Printer,
                           const PrintOptions &Options) const {
  if (NumAttrsSet == 0 && !DeclAttrs)
    return;

  for (auto DA : *this) {
    if (!Options.PrintImplicitAttrs && DA->isImplicit())
      continue;
    if (!Options.PrintAttrExported && isa<ExportedAttr>(DA))
      continue;

    DA->print(Printer);
  }

  if (isConversion())
    Printer << "@conversion ";
  if (isTransparent())
    Printer << "@transparent ";
  if (isInfix())
    Printer << "@infix ";
  if (isPostfix())
    Printer << "@postfix ";
  if (requiresStoredPropertyInits())
    Printer << "@requires_stored_property_inits ";
  if (isIBOutlet())
    Printer << "@IBOutlet ";
  if (isIBAction())
    Printer << "@IBAction ";
  if (isOptional())
    Printer << "@optional ";
  Optional<bool> MutatingAttr = getMutating();
  if (MutatingAttr && MutatingAttr.getValue())
    Printer << "@mutating ";
  if (MutatingAttr && !MutatingAttr.getValue())
    Printer << "@!mutating ";
}

void DeclAttribute::print(ASTPrinter &Printer) const {
  switch (getKind()) {
  case DAK_asmname:
    Printer << "@asmname(\"" << cast<AsmnameAttr>(this)->Name << "\")";
    break;
  case DAK_assignment:
    Printer << "@assignment";
    break;
  case DAK_availability: {
    Printer << "@availability(";
    auto Attr = cast<AvailabilityAttr>(this);
    if (!Attr->hasPlatform())
      Printer << "*";
    else
      Printer << Attr->Platform;

    Printer << ", unavailable";

    if (!Attr->Message.empty()) {
      Printer << ", message=\"" << Attr->Message << "\"";
    }
    Printer << ")";
    break;
  }
  case DAK_class_protocol:
    Printer << "@class_protocol";
    break;
  case DAK_exported:
    Printer << "@exported";
    break;
  case DAK_final:
    Printer << "@final";
    break;
  case DAK_noreturn:
    Printer << "@noreturn";
    break;
  case DAK_objc: {
    Printer << "@objc";
    llvm::SmallString<32> scratch;
    if (auto Name = cast<ObjCAttr>(this)->getName()) {
      Printer << "(" << Name->getString(scratch) << ")";
    }
    break;
  }
  case DAK_override:
    // A virtual attribute should be handled elsewhere.
    return;
  case DAK_required:
    if (isImplicit())
      Printer << "/* @required(inferred) */";
    else
      Printer << "@required";
    break;
  case DAK_Count:
    llvm_unreachable("exceed declaration attribute kinds");
  }
  Printer << " ";
}

void DeclAttribute::print(llvm::raw_ostream &OS) const {
  StreamPrinter P(OS);
  print(P);
}

unsigned DeclAttribute::getOptions(DeclAttrKind DK) {
  switch (DK) {
  case DAK_Count:
    llvm_unreachable("getOptions needs a valid attribute");
    break;
#define DECL_ATTR(NAME, CLASS, OPTIONS, ...)\
  case DAK_##NAME: return OPTIONS;
#include "swift/AST/Attr.def"
  }
}

StringRef DeclAttribute::getAttrName(DeclAttrKind DK) {
  switch (DK) {
  case DAK_Count:
    llvm_unreachable("getAttrName needs a valid attribute");
    break;
#define DECL_ATTR(NAME, CLASS, OPTIONS, ...)\
  case DAK_##NAME: return #NAME;
#include "swift/AST/Attr.def"
  }
}

ObjCAttr::ObjCAttr(SourceLoc atLoc, SourceRange baseRange,
                   Optional<ObjCSelector> name, SourceRange parenRange,
                   ArrayRef<SourceLoc> nameLocs)
  : DeclAttribute(DAK_objc, atLoc, baseRange, /*Implicit=*/false),
    NameData(nullptr)
{
  if (name) {
    // Store the name.
    assert(name->getNumSelectorPieces() == nameLocs.size());
    NameData = name->getOpaqueValue();

    // Store location information.
    ObjCAttrBits.HasTrailingLocationInfo = true;
    getTrailingLocations()[0] = parenRange.Start;
    getTrailingLocations()[1] = parenRange.End;
    std::memcpy(getTrailingLocations().slice(2).data(), nameLocs.data(),
                nameLocs.size() * sizeof(SourceLoc));
  } else {
    ObjCAttrBits.HasTrailingLocationInfo = false;
  }
}

ObjCAttr *ObjCAttr::create(ASTContext &Ctx, Optional<ObjCSelector> name) {
  return new (Ctx) ObjCAttr(name);
}

ObjCAttr *ObjCAttr::createUnnamed(ASTContext &Ctx, SourceLoc AtLoc,
                                  SourceLoc ObjCLoc) {
  return new (Ctx) ObjCAttr(AtLoc, SourceRange(ObjCLoc), Nothing,
                            SourceRange(), { });
}

ObjCAttr *ObjCAttr::createUnnamedImplicit(ASTContext &Ctx) {
  return new (Ctx) ObjCAttr(Nothing);
}

ObjCAttr *ObjCAttr::createNullary(ASTContext &Ctx, SourceLoc AtLoc, 
                                  SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                  SourceLoc NameLoc, Identifier Name,
                                  SourceLoc RParenLoc) {
  unsigned size = sizeof(ObjCAttr) + 3 * sizeof(SourceLoc);
  void *mem = Ctx.Allocate(size, alignof(ObjCAttr));
  return new (mem) ObjCAttr(AtLoc, SourceRange(ObjCLoc),
                            ObjCSelector(Ctx, 0, Name),
                            SourceRange(LParenLoc, RParenLoc),
                            NameLoc);
}

ObjCAttr *ObjCAttr::createNullary(ASTContext &Ctx, Identifier Name) {
  return new (Ctx) ObjCAttr(ObjCSelector(Ctx, 0, Name));
}

ObjCAttr *ObjCAttr::createSelector(ASTContext &Ctx, SourceLoc AtLoc, 
                                   SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                   ArrayRef<SourceLoc> NameLocs,
                                   ArrayRef<Identifier> Names,
                                   SourceLoc RParenLoc) {
  assert(NameLocs.size() == Names.size());
  unsigned size = sizeof(ObjCAttr) + (NameLocs.size() + 2) * sizeof(SourceLoc);
  void *mem = Ctx.Allocate(size, alignof(ObjCAttr));
  return new (mem) ObjCAttr(AtLoc, SourceRange(ObjCLoc),
                            ObjCSelector(Ctx, Names.size(), Names),
                            SourceRange(LParenLoc, RParenLoc),
                            NameLocs);
}

ObjCAttr *ObjCAttr::createSelector(ASTContext &Ctx, 
                                   ArrayRef<Identifier> Names) {
  return new (Ctx) ObjCAttr(ObjCSelector(Ctx, Names.size(), Names));
}

ArrayRef<SourceLoc> ObjCAttr::getNameLocs() const {
  if (!hasTrailingLocationInfo())
    return { };

  return getTrailingLocations().slice(2);
}

SourceLoc ObjCAttr::getLParenLoc() const {
  if (!hasTrailingLocationInfo())
    return SourceLoc();

  return getTrailingLocations()[0];
}

SourceLoc ObjCAttr::getRParenLoc() const {
  if (!hasTrailingLocationInfo())
    return SourceLoc();

  return getTrailingLocations()[1];
}

ObjCAttr *ObjCAttr::clone(ASTContext &context) const {
  return new (context) ObjCAttr(getName());
}

AvailabilityAttr *
AvailabilityAttr::createImplicitUnavailableAttr(ASTContext &C,
                                                StringRef Message) {
  return new (C) AvailabilityAttr(SourceLoc(), SourceRange(),
                                  "", Message, /* isUnavailable */ true,
                                  /* isImplicit */ true);
}
