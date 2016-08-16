//===--- Attr.cpp - Swift Language Attr ASTs ------------------------------===//
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
//  This file implements routines relating to declaration attributes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Attr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/StringSwitch.h"
using namespace swift;


// Only allow allocation of attributes using the allocator in ASTContext.
void *AttributeBase::operator new(size_t Bytes, ASTContext &C,
                                  unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

/// Given a name like "autoclosure", return the type attribute ID that
/// corresponds to it.  This returns TAK_Count on failure.
///
TypeAttrKind TypeAttributes::getAttrKindFromString(StringRef Str) {
  return llvm::StringSwitch<TypeAttrKind>(Str)
#define TYPE_ATTR(X) .Case(#X, TAK_##X)
#include "swift/AST/Attr.def"
  .Default(TAK_Count);
}

/// Return the name (like "autoclosure") for an attribute ID.
const char *TypeAttributes::getAttrName(TypeAttrKind kind) {
  switch (kind) {
  default: llvm_unreachable("Invalid attribute ID");
#define TYPE_ATTR(X) case TAK_##X: return #X;
#include "swift/AST/Attr.def"
  }
}



/// Given a name like "inline", return the decl attribute ID that corresponds
/// to it.  Note that this is a many-to-one mapping, and that the identifier
/// passed in may only be the first portion of the attribute (e.g. in the case
/// of the 'unowned(unsafe)' attribute, the string passed in is 'unowned'.
///
/// Also note that this recognizes both attributes like '@inline' (with no @)
/// and decl modifiers like 'final'.  This returns DAK_Count on failure.
///
DeclAttrKind DeclAttribute::getAttrKindFromString(StringRef Str) {
  return llvm::StringSwitch<DeclAttrKind>(Str)
#define DECL_ATTR(X, CLASS, ...) .Case(#X, DAK_##CLASS)
#define DECL_ATTR_ALIAS(X, CLASS) .Case(#X, DAK_##CLASS)
#include "swift/AST/Attr.def"
  .Default(DAK_Count);
}

/// Returns true if this attribute can appear on the specified decl.
bool DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind DK, const Decl *D) {
  return canAttributeAppearOnDeclKind(DK, D->getKind());
}

bool DeclAttribute::canAttributeAppearOnDeclKind(DeclAttrKind DAK, DeclKind DK) {
  unsigned Options = getOptions(DAK);
  switch (DK) {
#define DECL(Id, Parent) case DeclKind::Id: return (Options & On##Id) != 0;
#include "swift/AST/DeclNodes.def"
  }
  llvm_unreachable("bad DeclKind");
}

bool DeclAttributes::isUnavailableInCurrentSwift() const {
  for (auto attr : *this) {
    if (auto available = dyn_cast<AvailableAttr>(attr)) {
      if (available->isInvalid())
        continue;

      if (available->getUnconditionalAvailability() ==
            UnconditionalAvailabilityKind::UnavailableInCurrentSwift)
        return true;
    }
  }

  return false;
}

const AvailableAttr *DeclAttributes::getUnavailable(
                          const ASTContext &ctx) const {
  const AvailableAttr *conditional = nullptr;

  for (auto Attr : *this)
    if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
      if (AvAttr->isInvalid())
        continue;

      // If this attribute doesn't apply to the active platform, we're done.
      if (!AvAttr->isActivePlatform(ctx))
        continue;

      // Unconditional unavailable.
      if (AvAttr->isUnconditionallyUnavailable())
        return AvAttr;

      auto MinVersion = ctx.LangOpts.getMinPlatformVersion();
      switch (AvAttr->getMinVersionAvailability(MinVersion)) {
      case MinVersionComparison::Available:
      case MinVersionComparison::PotentiallyUnavailable:
        break;

      case MinVersionComparison::Obsoleted:
      case MinVersionComparison::Unavailable:
        conditional = AvAttr;
        break;
      }
    }
  return conditional;
}

const AvailableAttr *
DeclAttributes::getDeprecated(const ASTContext &ctx) const {
  const AvailableAttr *conditional = nullptr;
  for (auto Attr : *this) {
    if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
      if (AvAttr->isInvalid())
        continue;

      if (!AvAttr->isActivePlatform(ctx))
        continue;

      // Unconditional deprecated.
      if (AvAttr->isUnconditionallyDeprecated())
        return AvAttr;

      Optional<clang::VersionTuple> DeprecatedVersion = AvAttr->Deprecated;
      if (!DeprecatedVersion.hasValue())
        continue;

      auto MinVersion = ctx.LangOpts.getMinPlatformVersion();

      // We treat the declaration as deprecated if it is deprecated on
      // all deployment targets.
      // Once availability checking is enabled by default, we should
      // query the type refinement context hierarchy to determine
      // whether a declaration is deprecated on all versions
      // allowed by the context containing the reference.
      if (DeprecatedVersion.getValue() <= MinVersion) {
        conditional = AvAttr;
      }
    }
  }
  return conditional;
}

void DeclAttributes::dump() const {
  StreamPrinter P(llvm::errs());
  PrintOptions PO = PrintOptions::printEverything();
  print(P, PO);
}

/// Returns true if the attribute can be presented as a short form available
/// attribute (e.g., as @available(iOS 8.0, *). The presentation requires an
/// introduction version and does not support deprecation, obsoletion, or
/// messages.
static bool isShortAvailable(const DeclAttribute *DA) {
  auto *AvailAttr = dyn_cast<AvailableAttr>(DA);
  if (!AvailAttr)
    return false;

  if (!AvailAttr->Introduced.hasValue())
    return false;

  if (AvailAttr->Deprecated.hasValue())
    return false;

  if (AvailAttr->Obsoleted.hasValue())
    return false;

  if (!AvailAttr->Message.empty())
    return false;

  if (!AvailAttr->Rename.empty())
    return false;

  if (AvailAttr->Unconditional != UnconditionalAvailabilityKind::None)
    return false;

  return true;
}

/// Print the short-form @available() attribute for an array of long-form
/// AvailableAttrs that can be represented in the short form.
/// For example, for:
///   @available(OSX, introduced: 10.10)
///   @available(iOS, introduced: 8.0)
/// this will print:
///   @available(OSX 10.10, iOS 8.0, *)
static void printShortFormAvailable(ArrayRef<const DeclAttribute *> Attrs,
                                    ASTPrinter &Printer,
                                    const PrintOptions &Options) {
  assert(!Attrs.empty());

  Printer << "@available(";
  for (auto *DA : Attrs) {
    auto *AvailAttr = cast<AvailableAttr>(DA);
    assert(AvailAttr->Introduced.hasValue());

    Printer << platformString(AvailAttr->Platform) << " "
            << AvailAttr->Introduced.getValue().getAsString() << ", ";
  }

  Printer << "*)";
  Printer.printNewline();
}

void DeclAttributes::print(ASTPrinter &Printer,
                           const PrintOptions &Options) const {
  if (!DeclAttrs)
    return;

  using AttributeVector = SmallVector<const DeclAttribute *, 8>;
  AttributeVector orderedAttributes(begin(), end());
  std::reverse(orderedAttributes.begin(), orderedAttributes.end());

  // Process attributes in passes.
  AttributeVector shortAvailableAttributes;
  AttributeVector longAttributes;
  AttributeVector attributes;
  AttributeVector modifiers;

  for (auto DA : orderedAttributes) {
    if (!Options.PrintImplicitAttrs && DA->isImplicit())
      continue;
    if (!Options.PrintUserInaccessibleAttrs &&
        DeclAttribute::isUserInaccessible(DA->getKind()))
      continue;
    if (Options.excludeAttrKind(DA->getKind()))
      continue;

    AttributeVector &which = DA->isDeclModifier() ? modifiers :
                             isShortAvailable(DA) ? shortAvailableAttributes :
                             DA->isLongAttribute() ? longAttributes :
                             attributes;
    which.push_back(DA);
  }

  if (!shortAvailableAttributes.empty()) {
    printShortFormAvailable(shortAvailableAttributes, Printer, Options);
  }

  for (auto DA : longAttributes)
    DA->print(Printer, Options);
  for (auto DA : attributes)
    DA->print(Printer, Options);
  for (auto DA : modifiers)
    DA->print(Printer, Options);
}

SourceLoc DeclAttributes::getStartLoc(bool forModifiers) const {
  if (isEmpty())
    return SourceLoc();

  const DeclAttribute *lastAttr = nullptr;
  for (auto attr : *this) {
    if (attr->getRangeWithAt().Start.isValid() &&
        (!forModifiers || attr->isDeclModifier()))
      lastAttr = attr;
  }

  return lastAttr ? lastAttr->getRangeWithAt().Start : SourceLoc();
}

bool DeclAttribute::printImpl(ASTPrinter &Printer, const PrintOptions &Options) const {

  // Handle any attributes that are not printed at all before we make printer
  // callbacks.
  switch (getKind()) {
  case DAK_ObjC:
    if (Options.PrintForSIL && isImplicit())
      return false;
    break;
  case DAK_RawDocComment:
  case DAK_ObjCBridged:
  case DAK_SynthesizedProtocol:
  case DAK_ShowInInterface:
  case DAK_Rethrows:
    return false;
  default:
    break;
  }

  // Handle any decl-modifiers.
  // FIXME: Ideally we would handle decl modifiers as a special kind of
  // attribute, but for now it's simpler to treat them as a keyword in the
  // printer.
  switch (getKind()) {
    // Handle all of the SIMPLE_DECL_ATTRs.
#define SIMPLE_DECL_ATTR(X, CLASS, ...) case DAK_##CLASS:
#include "swift/AST/Attr.def"
  case DAK_Inline:
  case DAK_Accessibility:
  case DAK_Ownership:
  case DAK_Effects:
    if (DeclAttribute::isDeclModifier(getKind())) {
      Printer.printKeyword(getAttrName());
    } else {
      Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
      Printer.printAttrName(getAttrName(), /*needAt=*/true);
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
    }
    return true;

  case DAK_SetterAccessibility:
    Printer.printKeyword(getAttrName());
    Printer << "(set)";
    return true;

  default:
    break;
  }

  Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
  SWIFT_DEFER {
    Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
  };

  switch (getKind()) {
  case DAK_Semantics:
    Printer.printAttrName("@_semantics");
    Printer << "(\"" << cast<SemanticsAttr>(this)->Value << "\")";
    break;

  case DAK_Alignment:
    Printer.printAttrName("@_alignment");
    Printer << "(" << cast<AlignmentAttr>(this)->Value << ")";
    break;

  case DAK_SILGenName:
    Printer.printAttrName("@_silgen_name");
    Printer << "(\"" << cast<SILGenNameAttr>(this)->Name << "\")";
    break;

  case DAK_Available: {
    Printer.printAttrName("@available");
    Printer << "(";
    auto Attr = cast<AvailableAttr>(this);
    Printer << Attr->platformString();

    if (Attr->isUnconditionallyUnavailable())
      Printer << ", unavailable";
    else if (Attr->isUnconditionallyDeprecated())
      Printer << ", deprecated";

    if (Attr->Introduced)
      Printer << ", introduced: " << Attr->Introduced.getValue().getAsString();
    if (Attr->Deprecated)
      Printer << ", deprecated: " << Attr->Deprecated.getValue().getAsString();
    if (Attr->Obsoleted)
      Printer << ", obsoleted: " << Attr->Obsoleted.getValue().getAsString();

    if (!Attr->Rename.empty())
      Printer << ", renamed: \"" << Attr->Rename << "\"";

    // If there's no message, but this is specifically an imported
    // "unavailable in Swift" attribute, synthesize a message to look good in
    // the generated interface.
    if (!Attr->Message.empty())
      Printer << ", message: \"" << Attr->Message << "\"";
    else if (Attr->getUnconditionalAvailability()
               == UnconditionalAvailabilityKind::UnavailableInSwift)
      Printer << ", message: \"Not available in Swift\"";

    Printer << ")";
    break;
  }
  case DAK_AutoClosure:
    Printer.printAttrName("@autoclosure");
    if (cast<AutoClosureAttr>(this)->isEscaping())
      Printer << "(escaping)";
    break;
      
  case DAK_CDecl:
    Printer << "@_cdecl(\"" << cast<CDeclAttr>(this)->Name << "\")";
    break;

  case DAK_ObjC: {
    Printer.printAttrName("@objc");
    llvm::SmallString<32> scratch;
    if (auto Name = cast<ObjCAttr>(this)->getName()) {
      if (!cast<ObjCAttr>(this)->isNameImplicit())
        Printer << "(" << Name->getString(scratch) << ")";
    }
    break;
  }
    
  case DAK_SwiftNativeObjCRuntimeBase: {
    auto *attr = cast<SwiftNativeObjCRuntimeBaseAttr>(this);
    Printer.printAttrName("@_swift_native_objc_runtime_base");
    Printer << "(" << attr->BaseClassName.str() << ")";
    break;
  }

  case DAK_Swift3Migration: {
    auto attr = cast<Swift3MigrationAttr>(this);
    Printer.printAttrName("@swift3_migration");
    Printer << "(";

    bool printedAny = false;
    auto printSeparator = [&] {
      if (printedAny) Printer << ", ";
      else printedAny = true;
    };

    if (attr->getRenamed()) {
      printSeparator();
      Printer << "renamed: \"" << attr->getRenamed() << "\"";
    }

    if (!attr->getMessage().empty()) {
      printSeparator();
      Printer << "message: \"";
      Printer << attr->getMessage();
      Printer << "\"";
    }

    Printer << ")";
    break;
  }

  case DAK_Specialize: {
    Printer << "@" << getAttrName() << "(";
    auto *attr = cast<SpecializeAttr>(this);
    interleave(attr->getTypeLocs(),
               [&](TypeLoc tyLoc){ tyLoc.getType().print(Printer, Options); },
               [&]{ Printer << ", "; });
    Printer << ")";
    break;
  }

  case DAK_Count:
    llvm_unreachable("exceed declaration attribute kinds");

  default:
    llvm_unreachable("handled before this switch");
  }

  return true;
}

void DeclAttribute::print(ASTPrinter &Printer,
                          const PrintOptions &Options) const {

  if (!printImpl(Printer, Options))
    return; // Nothing printed.

  if (isLongAttribute() && Options.PrintLongAttrsOnSeparateLines)
    Printer.printNewline();
  else
    Printer << " ";
}

void DeclAttribute::print(llvm::raw_ostream &OS) const {
  StreamPrinter P(OS);
  print(P, PrintOptions());
}

unsigned DeclAttribute::getOptions(DeclAttrKind DK) {
  switch (DK) {
  case DAK_Count:
    llvm_unreachable("getOptions needs a valid attribute");
#define DECL_ATTR(_, CLASS, OPTIONS, ...)\
  case DAK_##CLASS: return OPTIONS;
#include "swift/AST/Attr.def"
  }
  llvm_unreachable("bad DeclAttrKind");
}

StringRef DeclAttribute::getAttrName() const {
  switch (getKind()) {
  case DAK_Count:
    llvm_unreachable("getAttrName needs a valid attribute");
#define SIMPLE_DECL_ATTR(NAME, CLASS, ...) \
  case DAK_##CLASS: \
    return #NAME;
#include "swift/AST/Attr.def"
  case DAK_SILGenName:
    return "_silgen_name";
  case DAK_Alignment:
    return "_alignment";
  case DAK_CDecl:
    return "_cdecl";
  case DAK_SwiftNativeObjCRuntimeBase:
    return "_swift_native_objc_runtime_base";
  case DAK_Semantics:
    return "_semantics";
  case DAK_Available:
    return "availability";
  case DAK_AutoClosure:
    return "autoclosure";
  case DAK_ObjC:
    return "objc";
  case DAK_Inline: {
    switch (cast<InlineAttr>(this)->getKind()) {
    case InlineKind::Never:
      return "inline(never)";
    case InlineKind::Always:
      return "inline(__always)";
    }
    llvm_unreachable("Invalid inline kind");
  }
  case DAK_Effects:
    switch (cast<EffectsAttr>(this)->getKind()) {
      case EffectsKind::ReadNone:
        return "effects(readnone)";
      case EffectsKind::ReadOnly:
        return "effects(readonly)";
      case EffectsKind::ReadWrite:
        return "effects(readwrite)";
      case EffectsKind::Unspecified:
        return "effects(unspecified)";
    }
  case DAK_Accessibility:
  case DAK_SetterAccessibility:
    switch (cast<AbstractAccessibilityAttr>(this)->getAccess()) {
    case Accessibility::Private:
      return "private";
    case Accessibility::FilePrivate:
      return "fileprivate";
    case Accessibility::Internal:
      return "internal";
    case Accessibility::Public:
      return "public";
    case Accessibility::Open:
      return "open";
    }
    llvm_unreachable("bad accessibility kind");

  case DAK_Ownership:
    switch (cast<OwnershipAttr>(this)->get()) {
    case Ownership::Strong: llvm_unreachable("Never present in the attribute");
    case Ownership::Weak:      return "weak";
    case Ownership::Unowned:   return "unowned";
    case Ownership::Unmanaged: return "unowned(unsafe)";
    }
    llvm_unreachable("bad ownership kind");
  case DAK_RawDocComment:
    return "<<raw doc comment>>";
  case DAK_ObjCBridged:
    return "<<ObjC bridged>>";
  case DAK_SynthesizedProtocol:
    return "<<synthesized protocol>>";
  case DAK_Swift3Migration:
    return "swift3_migration";
  case DAK_Specialize:
    return "_specialize";
  }
  llvm_unreachable("bad DeclAttrKind");
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

  ObjCAttrBits.ImplicitName = false;
}

ObjCAttr *ObjCAttr::create(ASTContext &Ctx, Optional<ObjCSelector> name,
                           bool isNameImplicit) {
  return new (Ctx) ObjCAttr(name, isNameImplicit);
}

ObjCAttr *ObjCAttr::createUnnamed(ASTContext &Ctx, SourceLoc AtLoc,
                                  SourceLoc ObjCLoc) {
  return new (Ctx) ObjCAttr(AtLoc, SourceRange(ObjCLoc), None,
                            SourceRange(), { });
}

ObjCAttr *ObjCAttr::createUnnamedImplicit(ASTContext &Ctx) {
  return new (Ctx) ObjCAttr(None, false);
}

ObjCAttr *ObjCAttr::createNullary(ASTContext &Ctx, SourceLoc AtLoc, 
                                  SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                  SourceLoc NameLoc, Identifier Name,
                                  SourceLoc RParenLoc) {
  void *mem = Ctx.Allocate(totalSizeToAlloc<SourceLoc>(3), alignof(ObjCAttr));
  return new (mem) ObjCAttr(AtLoc, SourceRange(ObjCLoc),
                            ObjCSelector(Ctx, 0, Name),
                            SourceRange(LParenLoc, RParenLoc),
                            NameLoc);
}

ObjCAttr *ObjCAttr::createNullary(ASTContext &Ctx, Identifier Name,
                                  bool isNameImplicit) {
  return new (Ctx) ObjCAttr(ObjCSelector(Ctx, 0, Name), isNameImplicit);
}

ObjCAttr *ObjCAttr::createSelector(ASTContext &Ctx, SourceLoc AtLoc, 
                                   SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                   ArrayRef<SourceLoc> NameLocs,
                                   ArrayRef<Identifier> Names,
                                   SourceLoc RParenLoc) {
  assert(NameLocs.size() == Names.size());
  void *mem = Ctx.Allocate(totalSizeToAlloc<SourceLoc>(NameLocs.size() + 2),
                           alignof(ObjCAttr));
  return new (mem) ObjCAttr(AtLoc, SourceRange(ObjCLoc),
                            ObjCSelector(Ctx, Names.size(), Names),
                            SourceRange(LParenLoc, RParenLoc),
                            NameLocs);
}

ObjCAttr *ObjCAttr::createSelector(ASTContext &Ctx, 
                                   ArrayRef<Identifier> Names,
                                   bool isNameImplicit) {
  return new (Ctx) ObjCAttr(ObjCSelector(Ctx, Names.size(), Names), 
                            isNameImplicit);
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
  return new (context) ObjCAttr(getName(), isNameImplicit());
}

AvailableAttr *
AvailableAttr::createUnconditional(ASTContext &C,
                                   StringRef Message,
                                   StringRef Rename,
                                   UnconditionalAvailabilityKind Reason) {
  assert(Reason != UnconditionalAvailabilityKind::None);
  clang::VersionTuple NoVersion;
  return new (C) AvailableAttr(
    SourceLoc(), SourceRange(), PlatformKind::none, Message, Rename,
    NoVersion, NoVersion, NoVersion, Reason, /* isImplicit */ false);
}

bool AvailableAttr::isActivePlatform(const ASTContext &ctx) const {
  return isPlatformActive(Platform, ctx.LangOpts);
}

bool AvailableAttr::isUnconditionallyUnavailable() const {
  switch (Unconditional) {
  case UnconditionalAvailabilityKind::None:
  case UnconditionalAvailabilityKind::Deprecated:
    return false;

  case UnconditionalAvailabilityKind::Unavailable:
  case UnconditionalAvailabilityKind::UnavailableInSwift:
  case UnconditionalAvailabilityKind::UnavailableInCurrentSwift:
    return true;
  }
}

bool AvailableAttr::isUnconditionallyDeprecated() const {
  switch (Unconditional) {
  case UnconditionalAvailabilityKind::None:
  case UnconditionalAvailabilityKind::Unavailable:
  case UnconditionalAvailabilityKind::UnavailableInSwift:
  case UnconditionalAvailabilityKind::UnavailableInCurrentSwift:
    return false;

  case UnconditionalAvailabilityKind::Deprecated:
    return true;
  }
}

MinVersionComparison AvailableAttr::getMinVersionAvailability(
                       clang::VersionTuple minVersion) const {
  // Unconditional unavailability.
  if (isUnconditionallyUnavailable())
    return MinVersionComparison::Unavailable;

  // If this entity was obsoleted before or at the minimum platform version,
  // consider it obsolete.
  if (Obsoleted && *Obsoleted <= minVersion)
    return MinVersionComparison::Obsoleted;

  // If this entity was introduced after the minimum platform version, it's
  // availability can only be determined dynamically.
  if (Introduced && *Introduced > minVersion)
    return MinVersionComparison::PotentiallyUnavailable;

  // The entity is available.
  return MinVersionComparison::Available;
}

const AvailableAttr *AvailableAttr::isUnavailable(const Decl *D) {
  ASTContext &ctx = D->getASTContext();
  return D->getAttrs().getUnavailable(ctx);
}

SpecializeAttr::SpecializeAttr(SourceLoc atLoc, SourceRange range,
                               ArrayRef<TypeLoc> typeLocs)
    : DeclAttribute(DAK_Specialize, atLoc, range, /*Implicit=*/false),
      numTypes(typeLocs.size())
{
  std::copy(typeLocs.begin(), typeLocs.end(), getTypeLocData());
}

ArrayRef<TypeLoc> SpecializeAttr::getTypeLocs() const {
  return const_cast<SpecializeAttr*>(this)->getTypeLocs();
}

MutableArrayRef<TypeLoc> SpecializeAttr::getTypeLocs() {
  return { this->getTypeLocData(), numTypes };
}

SpecializeAttr *SpecializeAttr::create(ASTContext &Ctx, SourceLoc atLoc,
                                       SourceRange range,
                                       ArrayRef<TypeLoc> typeLocs) {
  unsigned size = sizeof(SpecializeAttr) + (typeLocs.size() * sizeof(TypeLoc));
  void *mem = Ctx.Allocate(size, alignof(SpecializeAttr));
  return new (mem) SpecializeAttr(atLoc, range, typeLocs);
}
