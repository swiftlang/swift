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
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringSwitch.h"

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
      if (!AvAttr->hasPlatform())
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
    if (isa<AbstractAccessibilityAttr>(DA))
      continue;
    if (std::find(Options.ExcludeAttrList.begin(),
                  Options.ExcludeAttrList.end(),
                  DA->getKind()) != Options.ExcludeAttrList.end())
      continue;
    if (!Options.ExclusiveAttrList.empty()) {
      if (std::find(Options.ExclusiveAttrList.begin(),
                    Options.ExclusiveAttrList.end(),
                    DA->getKind()) == Options.ExclusiveAttrList.end())
        continue;
    }

    DA->print(Printer);
  }

  if (!Options.ExclusiveAttrList.empty())
    return;

  if (isConversion())
    Printer << "@conversion ";
  if (Options.PrintAttrTransparent && isTransparent())
    Printer << "@transparent ";
  if (isInfix())
    Printer << "@infix ";
  if (isPostfix())
    Printer << "@postfix ";
  if (requiresStoredPropertyInits())
    Printer << "@requires_stored_property_inits ";

  if (auto accessAttr = getAttribute<AccessibilityAttr>())
    Printer << accessAttr->getAttrName() << " ";
  if (auto setterAccessAttr = getAttribute<SetterAccessibilityAttr>())
    Printer << setterAccessAttr->getAttrName() << " ";

  Optional<bool> MutatingAttr = getMutating();
  if (MutatingAttr)
    Printer << (MutatingAttr.getValue() ? "mutating " : "nonmutating ");
}

void DeclAttribute::print(ASTPrinter &Printer) const {
  // This switch is not using Attr.def because some simple attributes have
  // custom behavior.
  switch (getKind()) {
  case DAK_IBAction:
  case DAK_IBDesignable:
  case DAK_IBInspectable:
  case DAK_IBOutlet:
  case DAK_Assignment:
  case DAK_ClassProtocol:
  case DAK_Exported:
  case DAK_Final:
  case DAK_Optional:
  case DAK_NoReturn:
  case DAK_UnsafeNoObjCTaggedPointer:
  case DAK_NSCopying:
  case DAK_NSManaged:
  case DAK_UIApplicationMain:
  case DAK_Lazy:
  case DAK_LLDBDebuggerFunction:
  case DAK_Inline:
    Printer << "@" << getAttrName();
    break;

  case DAK_Semantics:
    Printer << "@semantics(\"" << cast<SemanticsAttr>(this)->Value << "\")";
    break;

  case DAK_Asmname:
    Printer << "@asmname(\"" << cast<AsmnameAttr>(this)->Name << "\")";
    break;

  case DAK_Availability: {
    Printer << "@availability(";
    auto Attr = cast<AvailabilityAttr>(this);
    Printer << Attr->platformString() << ", unavailable";
    if (!Attr->Message.empty()) {
      Printer << ", message=\"" << Attr->Message << "\"";
    }
    Printer << ")";
    break;
  }

  case DAK_ObjC: {
    Printer << "@objc";
    llvm::SmallString<32> scratch;
    if (auto Name = cast<ObjCAttr>(this)->getName()) {
      Printer << "(" << Name->getString(scratch) << ")";
    }
    break;
  }

  case DAK_Accessibility:
    Printer << getAttrName();
    break;

  case DAK_SetterAccessibility:
    Printer << getAttrName() << "(set)";
    break;

  case DAK_Override:
    // A virtual attribute should be handled elsewhere.
    return;
  case DAK_RawDocComment:
    // Not printed.
    return;
  case DAK_Required:
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
#define DECL_ATTR(_, CLASS, OPTIONS, ...)\
  case DAK_##CLASS: return OPTIONS;
#include "swift/AST/Attr.def"
  }
}

StringRef DeclAttribute::getAttrName() const {
  switch (getKind()) {
  case DAK_Count:
    llvm_unreachable("getAttrName needs a valid attribute");
#define SIMPLE_DECL_ATTR(NAME, CLASS, ...) \
  case DAK_##CLASS: \
    return #NAME;
#include "swift/AST/Attr.def"
  case DAK_Asmname:
    return "asmname";
  case DAK_Semantics:
    return "semantics";
  case DAK_Availability:
    return "availability";
  case DAK_ObjC:
    return "objc";
  case DAK_Inline:
    switch (cast<InlineAttr>(this)->getKind()) {
    case InlineKind::Never:
      return "inline(never)";
    }
  case DAK_Accessibility:
  case DAK_SetterAccessibility:
    switch (cast<AbstractAccessibilityAttr>(this)->getAccess()) {
    case Accessibility::Private:
      return "private";
    case Accessibility::Internal:
      return "internal";
    case Accessibility::Public:
      return "public";
    }
  case DAK_Override:
  case DAK_RawDocComment:
    llvm_unreachable("cannot get the name of a virtual attribute");
  }
}


ObjCAttr::ObjCAttr(SourceLoc atLoc, SourceRange baseRange,
                   Optional<ObjCSelector> name, SourceRange parenRange,
                   ArrayRef<SourceLoc> nameLocs)
  : DeclAttribute(DAK_ObjC, atLoc, baseRange, /*Implicit=*/false),
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
  clang::VersionTuple NoVersion;
  return new (C) AvailabilityAttr(
    SourceLoc(), SourceRange(), PlatformKind::none, Message,
    NoVersion, NoVersion, NoVersion,
    /* isUnavailable */ true,
    /* isImplicit */ true);
}

StringRef
AvailabilityAttr::platformString(AvailabilityAttr::PlatformKind platform) {
  switch (platform) {
    case none: return "*";
#define AVAILABILITY_PLATFORM(X) case X: return #X;
#include "swift/AST/Attr.def"
  }
}

Optional<AvailabilityAttr::PlatformKind>
AvailabilityAttr::platformFromString(StringRef Name) {
  if (Name == "*")
    return PlatformKind::none;
  return
    llvm::StringSwitch<Optional<AvailabilityAttr::PlatformKind>>(Name)
#define AVAILABILITY_PLATFORM(X) .Case(#X, X)
#include "swift/AST/Attr.def"
    .Default(Optional<AvailabilityAttr::PlatformKind>());
}

bool AvailabilityAttr::isUnavailable(const Decl *D) {
  for (auto Attr : D->getAttrs())
    if (auto AvailAttr = dyn_cast<AvailabilityAttr>(Attr))
      if (AvailAttr->IsUnvailable)
        return true;

  return false;
}


