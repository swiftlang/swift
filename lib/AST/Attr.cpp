//===--- Attr.cpp - Swift Language Attr ASTs ------------------------------===//
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
//  This file implements routines relating to declaration attributes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Attr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/StringSwitch.h"
using namespace swift;

#define DECL_ATTR(_, Id, ...) \
  static_assert(IsTriviallyDestructible<Id##Attr>::value, \
                "Attrs are BumpPtrAllocated; the destructor is never called");
#include "swift/AST/Attr.def"
static_assert(IsTriviallyDestructible<DeclAttributes>::value,
              "DeclAttributes are BumpPtrAllocated; the d'tor is never called");
static_assert(IsTriviallyDestructible<TypeAttributes>::value,
              "TypeAttributes are BumpPtrAllocated; the d'tor is never called");


// Only allow allocation of attributes using the allocator in ASTContext.
void *AttributeBase::operator new(size_t Bytes, ASTContext &C,
                                  unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

StringRef swift::getAccessLevelSpelling(AccessLevel value) {
  switch (value) {
  case AccessLevel::Private: return "private";
  case AccessLevel::FilePrivate: return "fileprivate";
  case AccessLevel::Internal: return "internal";
  case AccessLevel::Public: return "public";
  case AccessLevel::Open: return "open";
  }

  llvm_unreachable("Unhandled AccessLevel in switch.");
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
  auto Options = getOptions(DAK);
  switch (DK) {
#define DECL(Id, Parent) case DeclKind::Id: return (Options & On##Id) != 0;
#include "swift/AST/DeclNodes.def"
  }
  llvm_unreachable("bad DeclKind");
}

bool
DeclAttributes::isUnavailableInSwiftVersion(
  const version::Version &effectiveVersion) const {
  llvm::VersionTuple vers = effectiveVersion;
  for (auto attr : *this) {
    if (auto available = dyn_cast<AvailableAttr>(attr)) {
      if (available->isInvalid())
        continue;

      if (available->getPlatformAgnosticAvailability() ==
          PlatformAgnosticAvailabilityKind::SwiftVersionSpecific) {
        if (available->Introduced.hasValue() &&
            available->Introduced.getValue() > vers)
          return true;
        if (available->Obsoleted.hasValue() &&
            available->Obsoleted.getValue() <= vers)
          return true;
      }
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
      if (!AvAttr->isActivePlatform(ctx) &&
          !AvAttr->isLanguageVersionSpecific())
        continue;

      // Unconditional unavailable.
      if (AvAttr->isUnconditionallyUnavailable())
        return AvAttr;

      switch (AvAttr->getVersionAvailability(ctx)) {
      case AvailableVersionComparison::Available:
      case AvailableVersionComparison::PotentiallyUnavailable:
        break;

      case AvailableVersionComparison::Obsoleted:
      case AvailableVersionComparison::Unavailable:
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

      if (!AvAttr->isActivePlatform(ctx) &&
          !AvAttr->isLanguageVersionSpecific())
        continue;

      // Unconditional deprecated.
      if (AvAttr->isUnconditionallyDeprecated())
        return AvAttr;

      Optional<llvm::VersionTuple> DeprecatedVersion = AvAttr->Deprecated;
      if (!DeprecatedVersion.hasValue())
        continue;

      llvm::VersionTuple MinVersion =
        AvAttr->isLanguageVersionSpecific() ?
        ctx.LangOpts.EffectiveLanguageVersion :
        ctx.LangOpts.getMinPlatformVersion();

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

void DeclAttributes::dump(const Decl *D) const {
  StreamPrinter P(llvm::errs());
  PrintOptions PO = PrintOptions::printEverything();
  print(P, PO, D);
}

/// Returns true if the attribute can be presented as a short form available
/// attribute (e.g., as @available(iOS 8.0, *). The presentation requires an
/// introduction version and does not support deprecation, obsoletion, or
/// messages.
LLVM_READONLY
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

  switch (AvailAttr->PlatformAgnostic) {
  case PlatformAgnosticAvailabilityKind::Deprecated:
  case PlatformAgnosticAvailabilityKind::Unavailable:
  case PlatformAgnosticAvailabilityKind::UnavailableInSwift:
    return false;
  case PlatformAgnosticAvailabilityKind::None:
  case PlatformAgnosticAvailabilityKind::SwiftVersionSpecific:
    return true;
  }

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
  auto FirstAvail = cast<AvailableAttr>(Attrs.front());
  if (Attrs.size() == 1 &&
      FirstAvail->isLanguageVersionSpecific()) {
    assert(FirstAvail->Introduced.hasValue());
    Printer << "swift "
            << FirstAvail->Introduced.getValue().getAsString()
            << ")";
  } else {
    for (auto *DA : Attrs) {
      auto *AvailAttr = cast<AvailableAttr>(DA);
      assert(AvailAttr->Introduced.hasValue());
      Printer << platformString(AvailAttr->Platform) << " "
              << AvailAttr->Introduced.getValue().getAsString() << ", ";
    }
    Printer << "*)";
  }
  Printer.printNewline();
}

void DeclAttributes::print(ASTPrinter &Printer, const PrintOptions &Options,
                           const Decl *D) const {
  if (!DeclAttrs)
    return;

  using AttributeVector = SmallVector<const DeclAttribute *, 8>;
  AttributeVector orderedAttributes(begin(), end());
  std::reverse(orderedAttributes.begin(), orderedAttributes.end());

  // Process attributes in passes.
  AttributeVector shortAvailableAttributes;
  const DeclAttribute *swiftVersionAvailableAttribute = nullptr;
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

    // Be careful not to coalesce `@available(swift 5)` with other short
    // `available' attributes.
    if (auto *availableAttr = dyn_cast<AvailableAttr>(DA)) {
      if (availableAttr->isLanguageVersionSpecific() &&
          isShortAvailable(availableAttr)) {
        swiftVersionAvailableAttribute = availableAttr;
        continue;
      }
    }

    AttributeVector &which = DA->isDeclModifier() ? modifiers :
                             isShortAvailable(DA) ? shortAvailableAttributes :
                             DA->isLongAttribute() ? longAttributes :
                             attributes;
    which.push_back(DA);
  }

  if (swiftVersionAvailableAttribute)
    printShortFormAvailable(swiftVersionAvailableAttribute, Printer, Options);
  if (!shortAvailableAttributes.empty())
    printShortFormAvailable(shortAvailableAttributes, Printer, Options);

  for (auto DA : longAttributes)
    DA->print(Printer, Options, D);
  for (auto DA : attributes)
    DA->print(Printer, Options, D);
  for (auto DA : modifiers)
    DA->print(Printer, Options, D);
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

bool DeclAttribute::printImpl(ASTPrinter &Printer, const PrintOptions &Options,
                              const Decl *D) const {

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
  case DAK_Infix:
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
  case DAK_AccessControl:
  case DAK_ReferenceOwnership:
  case DAK_Effects:
  case DAK_Optimize:
    if (DeclAttribute::isDeclModifier(getKind())) {
      Printer.printKeyword(getAttrName());
    } else {
      Printer.printSimpleAttr(getAttrName(), /*needAt=*/true);
    }
    return true;

  case DAK_SetterAccess:
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
    Printer << "(" << cast<AlignmentAttr>(this)->getValue() << ")";
    break;

  case DAK_SILGenName:
    Printer.printAttrName("@_silgen_name");
    Printer << "(\"" << cast<SILGenNameAttr>(this)->Name << "\")";
    break;

  case DAK_Available: {
    Printer.printAttrName("@available");
    Printer << "(";
    auto Attr = cast<AvailableAttr>(this);
    if (Attr->isLanguageVersionSpecific())
      Printer << "swift";
    else
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
    else if (Attr->getPlatformAgnosticAvailability()
               == PlatformAgnosticAvailabilityKind::UnavailableInSwift)
      Printer << ", message: \"Not available in Swift\"";

    Printer << ")";
    break;
  }

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

  case DAK_PrivateImport: {
    Printer.printAttrName("@_private(sourceFile: \"");
    Printer << cast<PrivateImportAttr>(this)->getSourceFile() << "\")";
    break;
  }
    
  case DAK_SwiftNativeObjCRuntimeBase: {
    auto *attr = cast<SwiftNativeObjCRuntimeBaseAttr>(this);
    Printer.printAttrName("@_swift_native_objc_runtime_base");
    Printer << "(" << attr->BaseClassName.str() << ")";
    break;
  }

  case DAK_Specialize: {
    Printer << "@" << getAttrName() << "(";
    auto *attr = cast<SpecializeAttr>(this);
    auto exported = attr->isExported() ? "true" : "false";
    auto kind = attr->isPartialSpecialization() ? "partial" : "full";

    Printer << "exported: "<<  exported << ", ";
    Printer << "kind: " << kind << ", ";

    if (!attr->getRequirements().empty()) {
      Printer << "where ";
    }
    std::function<Type(Type)> GetInterfaceType;
    auto *FnDecl = dyn_cast_or_null<AbstractFunctionDecl>(D);
    if (!FnDecl || !FnDecl->getGenericEnvironment())
      GetInterfaceType = [](Type Ty) -> Type { return Ty; };
    else {
      // Use GenericEnvironment to produce user-friendly
      // names instead of something like t_0_0.
      auto *GenericEnv = FnDecl->getGenericEnvironment();
      assert(GenericEnv);
      GetInterfaceType = [=](Type Ty) -> Type {
        return GenericEnv->getSugaredType(Ty);
      };
    }
    interleave(attr->getRequirements(),
               [&](Requirement req) {
                 auto FirstTy = GetInterfaceType(req.getFirstType());
                 if (req.getKind() != RequirementKind::Layout) {
                   auto SecondTy = GetInterfaceType(req.getSecondType());
                   Requirement ReqWithDecls(req.getKind(), FirstTy, SecondTy);
                   ReqWithDecls.print(Printer, Options);
                 } else {
                   Requirement ReqWithDecls(req.getKind(), FirstTy,
                                            req.getLayoutConstraint());
                   ReqWithDecls.print(Printer, Options);
                 }
               },
               [&] { Printer << ", "; });

    Printer << ")";
    break;
  }

  case DAK_Implements: {
    Printer.printAttrName("@_implements");
    Printer << "(";
    auto *attr = cast<ImplementsAttr>(this);
    attr->getProtocolType().getType().print(Printer, Options);
    Printer << ", " << attr->getMemberName() << ")";
    break;
  }

  case DAK_ObjCRuntimeName: {
    Printer.printAttrName("@objc");
    Printer << "(";
    auto *attr = cast<ObjCRuntimeNameAttr>(this);
    Printer << "\"" << attr->Name << "\"";
    Printer << ")";
    break;
  }

  case DAK_ClangImporterSynthesizedType: {
    Printer.printAttrName("@_clangImporterSynthesizedType");
    auto *attr = cast<ClangImporterSynthesizedTypeAttr>(this);
    Printer << "(originalTypeName: \"" << attr->originalTypeName
            << "\", manglingForKind: \"" << attr->getManglingName() << "\")";
    break;
  }

  // SWIFT_ENABLE_TENSORFLOW
  case DAK_Differentiable: {
    Printer.printAttrName("@differentiable");
    Printer << '(';
    auto *attr = cast<DifferentiableAttr>(this);
    switch (attr->getMode()) {
    case AutoDiffMode::Forward:
      Printer << "forward";
      break;
    case AutoDiffMode::Reverse:
      Printer << "reverse";
      break;
    }
    auto params = attr->getParameters();
    // Print differentiation parameters, if any.
    if (!params.empty()) {
      Printer << ", wrt: (";
      interleave(params, [&](const AutoDiffParameter &param) {
        switch (param.getKind()) {
        case AutoDiffParameter::Kind::Index:
          Printer << '.' << param.getIndex();
          break;
        case AutoDiffParameter::Kind::Self:
          Printer << "self";
          break;
        }
      }, [&] {
        Printer << ", ";
      });
      Printer << ")";
    }
    // Print primal function name if any.
    if (auto primal = attr->getPrimal())
      Printer << ", primal: " << primal->Name;
    // Print adjoint function name.
    if (auto adjoint = attr->getAdjoint())
      Printer << ", adjoint: " << adjoint->Name;
    // Print jvp function name.
    if (auto jvp = attr->getJVP())
      Printer << ", jvp: " << jvp->Name;
    // Print vjp function name.
    if (auto vjp = attr->getVJP())
      Printer << ", vjp: " << vjp->Name;
    // FIXME: Print 'where' clause, if any.
    Printer << ")";
    break;
  }

  case DAK_DynamicReplacement: {
    Printer.printAttrName("@_dynamicReplacement");
    Printer << "(for: \"";
    auto *attr = cast<DynamicReplacementAttr>(this);
    Printer << attr->getReplacedFunctionName() << "\")";
    break;
  }

  case DAK_Count:
    llvm_unreachable("exceed declaration attribute kinds");

#define SIMPLE_DECL_ATTR(X, CLASS, ...) case DAK_##CLASS:
#include "swift/AST/Attr.def"
    llvm_unreachable("handled above");

  default:
    assert(DeclAttribute::isDeclModifier(getKind()) &&
           "handled above");
  }

  return true;
}

void DeclAttribute::print(ASTPrinter &Printer, const PrintOptions &Options,
                          const Decl *D) const {

  if (!printImpl(Printer, Options, D))
    return; // Nothing printed.

  if (isLongAttribute() && Options.PrintLongAttrsOnSeparateLines)
    Printer.printNewline();
  else
    Printer << " ";
}

void DeclAttribute::print(llvm::raw_ostream &OS, const Decl *D) const {
  StreamPrinter P(OS);
  print(P, PrintOptions(), D);
}

uint64_t DeclAttribute::getOptions(DeclAttrKind DK) {
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
  case DAK_ObjC:
  case DAK_ObjCRuntimeName:
    return "objc";
  case DAK_DynamicReplacement:
    return "_dynamicReplacement";
  case DAK_PrivateImport:
    return "_private";
  case DAK_RestatedObjCConformance:
    return "_restatedObjCConformance";
  case DAK_Inline: {
    switch (cast<InlineAttr>(this)->getKind()) {
    case InlineKind::Never:
      return "inline(never)";
    case InlineKind::Always:
      return "inline(__always)";
    }
    llvm_unreachable("Invalid inline kind");
  }
  case DAK_Optimize: {
    switch (cast<OptimizeAttr>(this)->getMode()) {
    case OptimizationMode::NoOptimization:
      return "_optimize(none)";
    case OptimizationMode::ForSpeed:
      return "_optimize(speed)";
    case OptimizationMode::ForSize:
      return "_optimize(size)";
    default:
      llvm_unreachable("Invalid optimization kind");
    }
  }
  case DAK_Effects:
    switch (cast<EffectsAttr>(this)->getKind()) {
      case EffectsKind::ReadNone:
        return "_effects(readnone)";
      case EffectsKind::ReadOnly:
        return "_effects(readonly)";
      case EffectsKind::ReleaseNone:
        return "_effects(releasenone)";
      case EffectsKind::ReadWrite:
        return "_effects(readwrite)";
      case EffectsKind::Unspecified:
        return "_effects(unspecified)";
    }
  case DAK_AccessControl:
  case DAK_SetterAccess: {
    AccessLevel access = cast<AbstractAccessControlAttr>(this)->getAccess();
    return getAccessLevelSpelling(access);
  }

  case DAK_ReferenceOwnership:
    return keywordOf(cast<ReferenceOwnershipAttr>(this)->get());
  case DAK_RawDocComment:
    return "<<raw doc comment>>";
  case DAK_ObjCBridged:
    return "<<ObjC bridged>>";
  case DAK_SynthesizedProtocol:
    return "<<synthesized protocol>>";
  case DAK_Specialize:
    return "_specialize";
  case DAK_Implements:
    return "_implements";
  case DAK_ClangImporterSynthesizedType:
    return "_clangImporterSynthesizedType";
  // SWIFT_ENABLE_TENSORFLOW
  case DAK_Differentiable:
    return "differentiable";
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
    Bits.ObjCAttr.HasTrailingLocationInfo = true;
    getTrailingLocations()[0] = parenRange.Start;
    getTrailingLocations()[1] = parenRange.End;
    std::memcpy(getTrailingLocations().slice(2).data(), nameLocs.data(),
                nameLocs.size() * sizeof(SourceLoc));
  } else {
    Bits.ObjCAttr.HasTrailingLocationInfo = false;
  }

  Bits.ObjCAttr.ImplicitName = false;
  Bits.ObjCAttr.Swift3Inferred = false;
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
  return new (mem) ObjCAttr(AtLoc, SourceRange(ObjCLoc, RParenLoc),
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
  return new (mem) ObjCAttr(AtLoc, SourceRange(ObjCLoc, RParenLoc),
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
  auto attr = new (context) ObjCAttr(getName(), isNameImplicit());
  attr->setSwift3Inferred(isSwift3Inferred());
  return attr;
}

PrivateImportAttr::PrivateImportAttr(SourceLoc atLoc, SourceRange baseRange,
                                     StringRef sourceFile,
                                     SourceRange parenRange)
    : DeclAttribute(DAK_PrivateImport, atLoc, baseRange, /*Implicit=*/false),
      SourceFile(sourceFile) {}

PrivateImportAttr *PrivateImportAttr::create(ASTContext &Ctxt, SourceLoc AtLoc,
                                             SourceLoc PrivateLoc,
                                             SourceLoc LParenLoc,
                                             StringRef sourceFile,
                                             SourceLoc RParenLoc) {
  return new (Ctxt)
      PrivateImportAttr(AtLoc, SourceRange(PrivateLoc, RParenLoc), sourceFile,
                        SourceRange(LParenLoc, RParenLoc));
}

DynamicReplacementAttr::DynamicReplacementAttr(SourceLoc atLoc,
                                               SourceRange baseRange,
                                               DeclName name,
                                               SourceRange parenRange)
    : DeclAttribute(DAK_DynamicReplacement, atLoc, baseRange,
                    /*Implicit=*/false),
      ReplacedFunctionName(name), ReplacedFunction(nullptr) {
  Bits.DynamicReplacementAttr.HasTrailingLocationInfo = true;
  getTrailingLocations()[0] = parenRange.Start;
  getTrailingLocations()[1] = parenRange.End;
}

DynamicReplacementAttr *
DynamicReplacementAttr::create(ASTContext &Ctx, SourceLoc AtLoc,
                               SourceLoc DynReplLoc, SourceLoc LParenLoc,
                               DeclName ReplacedFunction, SourceLoc RParenLoc) {
  void *mem = Ctx.Allocate(totalSizeToAlloc<SourceLoc>(2),
                           alignof(DynamicReplacementAttr));
  return new (mem) DynamicReplacementAttr(
      AtLoc, SourceRange(DynReplLoc, RParenLoc), ReplacedFunction,
      SourceRange(LParenLoc, RParenLoc));
}

DynamicReplacementAttr *DynamicReplacementAttr::create(ASTContext &Ctx,
                                                       DeclName name) {
  return new (Ctx) DynamicReplacementAttr(name);
}

DynamicReplacementAttr *
DynamicReplacementAttr::create(ASTContext &Ctx, DeclName name,
                               AbstractFunctionDecl *f) {
  auto res = new (Ctx) DynamicReplacementAttr(name);
  res->setReplacedFunction(f);
  return res;
}

SourceLoc DynamicReplacementAttr::getLParenLoc() const {
  return getTrailingLocations()[0];
}

SourceLoc DynamicReplacementAttr::getRParenLoc() const {
  return getTrailingLocations()[1];
}

AvailableAttr *
AvailableAttr::createPlatformAgnostic(ASTContext &C,
                                   StringRef Message,
                                   StringRef Rename,
                                   PlatformAgnosticAvailabilityKind Kind,
                                   llvm::VersionTuple Obsoleted) {
  assert(Kind != PlatformAgnosticAvailabilityKind::None);
  llvm::VersionTuple NoVersion;
  if (Kind == PlatformAgnosticAvailabilityKind::SwiftVersionSpecific) {
    assert(!Obsoleted.empty());
  }
  return new (C) AvailableAttr(
    SourceLoc(), SourceRange(), PlatformKind::none, Message, Rename,
    NoVersion, SourceRange(),
    NoVersion, SourceRange(),
    Obsoleted, SourceRange(),
    Kind, /* isImplicit */ false);
}

bool AvailableAttr::isActivePlatform(const ASTContext &ctx) const {
  return isPlatformActive(Platform, ctx.LangOpts);
}

bool AvailableAttr::isLanguageVersionSpecific() const {
  if (PlatformAgnostic ==
      PlatformAgnosticAvailabilityKind::SwiftVersionSpecific)
    {
      assert(Platform == PlatformKind::none &&
             (Introduced.hasValue() ||
              Deprecated.hasValue() ||
              Obsoleted.hasValue()));
      return true;
    }
  return false;
}

bool AvailableAttr::isUnconditionallyUnavailable() const {
  switch (PlatformAgnostic) {
  case PlatformAgnosticAvailabilityKind::None:
  case PlatformAgnosticAvailabilityKind::Deprecated:
  case PlatformAgnosticAvailabilityKind::SwiftVersionSpecific:
    return false;

  case PlatformAgnosticAvailabilityKind::Unavailable:
  case PlatformAgnosticAvailabilityKind::UnavailableInSwift:
    return true;
  }

  llvm_unreachable("Unhandled PlatformAgnosticAvailabilityKind in switch.");
}

bool AvailableAttr::isUnconditionallyDeprecated() const {
  switch (PlatformAgnostic) {
  case PlatformAgnosticAvailabilityKind::None:
  case PlatformAgnosticAvailabilityKind::Unavailable:
  case PlatformAgnosticAvailabilityKind::UnavailableInSwift:
  case PlatformAgnosticAvailabilityKind::SwiftVersionSpecific:
    return false;

  case PlatformAgnosticAvailabilityKind::Deprecated:
    return true;
  }

  llvm_unreachable("Unhandled PlatformAgnosticAvailabilityKind in switch.");
}

AvailableVersionComparison AvailableAttr::getVersionAvailability(
  const ASTContext &ctx) const {

  // Unconditional unavailability.
  if (isUnconditionallyUnavailable())
    return AvailableVersionComparison::Unavailable;

  llvm::VersionTuple queryVersion =
    isLanguageVersionSpecific() ?
    ctx.LangOpts.EffectiveLanguageVersion :
    ctx.LangOpts.getMinPlatformVersion();

  // If this entity was obsoleted before or at the query platform version,
  // consider it obsolete.
  if (Obsoleted && *Obsoleted <= queryVersion)
    return AvailableVersionComparison::Obsoleted;

  // If this entity was introduced after the query version and we're doing a
  // platform comparison, true availability can only be determined dynamically;
  // if we're doing a _language_ version check, the query version is a
  // static requirement, so we treat "introduced later" as just plain
  // unavailable.
  if (Introduced && *Introduced > queryVersion) {
    if (isLanguageVersionSpecific())
      return AvailableVersionComparison::Unavailable;
    else
      return AvailableVersionComparison::PotentiallyUnavailable;
  }

  // The entity is available.
  return AvailableVersionComparison::Available;
}

const AvailableAttr *AvailableAttr::isUnavailable(const Decl *D) {
  ASTContext &ctx = D->getASTContext();
  return D->getAttrs().getUnavailable(ctx);
}

SpecializeAttr::SpecializeAttr(SourceLoc atLoc, SourceRange range,
                               TrailingWhereClause *clause,
                               bool exported,
                               SpecializationKind kind)
    : DeclAttribute(DAK_Specialize, atLoc, range, /*Implicit=*/false),
      trailingWhereClause(clause) {
  Bits.SpecializeAttr.exported = exported;
  Bits.SpecializeAttr.kind = unsigned(kind);
  Bits.SpecializeAttr.numRequirements = 0;
}

SpecializeAttr::SpecializeAttr(SourceLoc atLoc, SourceRange range,
                               ArrayRef<Requirement> requirements,
                               bool exported,
                               SpecializationKind kind)
    : DeclAttribute(DAK_Specialize, atLoc, range, /*Implicit=*/false) {
  Bits.SpecializeAttr.exported = exported;
  Bits.SpecializeAttr.kind = unsigned(kind);
  Bits.SpecializeAttr.numRequirements = requirements.size();
  std::copy(requirements.begin(), requirements.end(), getRequirementsData());
}

void SpecializeAttr::setRequirements(ASTContext &Ctx,
                                     ArrayRef<Requirement> requirements) {
  unsigned numClauseRequirements =
      (trailingWhereClause) ? trailingWhereClause->getRequirements().size() : 0;
  assert(requirements.size() <= numClauseRequirements);
  if (!numClauseRequirements)
    return;
  Bits.SpecializeAttr.numRequirements = requirements.size();
  std::copy(requirements.begin(), requirements.end(), getRequirementsData());
}

ArrayRef<Requirement> SpecializeAttr::getRequirements() const {
  return const_cast<SpecializeAttr*>(this)->getRequirements();
}

TrailingWhereClause *SpecializeAttr::getTrailingWhereClause() const {
  return trailingWhereClause;
}

SpecializeAttr *SpecializeAttr::create(ASTContext &Ctx, SourceLoc atLoc,
                                       SourceRange range,
                                       TrailingWhereClause *clause,
                                       bool exported,
                                       SpecializationKind kind) {
  unsigned numRequirements = (clause) ? clause->getRequirements().size() : 0;
  unsigned size =
      sizeof(SpecializeAttr) + (numRequirements * sizeof(Requirement));
  void *mem = Ctx.Allocate(size, alignof(SpecializeAttr));
  return new (mem)
      SpecializeAttr(atLoc, range, clause, exported, kind);
}

SpecializeAttr *SpecializeAttr::create(ASTContext &Ctx, SourceLoc atLoc,
                                       SourceRange range,
                                       ArrayRef<Requirement> requirements,
                                       bool exported,
                                       SpecializationKind kind) {
  unsigned numRequirements = requirements.size();
  unsigned size =
      sizeof(SpecializeAttr) + (numRequirements * sizeof(Requirement));
  void *mem = Ctx.Allocate(size, alignof(SpecializeAttr));
  return new (mem)
      SpecializeAttr(atLoc, range, requirements, exported, kind);
}

// SWIFT_ENABLE_TENSORFLOW
DifferentiableAttr::DifferentiableAttr(SourceLoc atLoc, SourceRange baseRange,
                                       AutoDiffMode mode, SourceLoc modeLoc,
                                       ArrayRef<AutoDiffParameter> parameters,
                                       Optional<DeclNameWithLoc> primal,
                                       Optional<DeclNameWithLoc> adjoint,
                                       Optional<DeclNameWithLoc> jvp,
                                       Optional<DeclNameWithLoc> vjp,
                                       TrailingWhereClause *clause)
  : DeclAttribute(DAK_Differentiable, atLoc, baseRange, /*Implicit*/false),
    Mode(mode), ModeLoc(modeLoc), NumParameters(parameters.size()),
    Primal(std::move(primal)), Adjoint(std::move(adjoint)),
    JVP(std::move(jvp)), VJP(std::move(vjp)), WhereClause(clause) {
  std::copy(parameters.begin(), parameters.end(), getParametersData());
}

DifferentiableAttr *
DifferentiableAttr::create(ASTContext &context, SourceLoc atLoc,
                           SourceRange baseRange, AutoDiffMode mode,
                           SourceLoc modeLoc,
                           ArrayRef<AutoDiffParameter> parameters,
                           Optional<DeclNameWithLoc> primal,
                           Optional<DeclNameWithLoc> adjoint,
                           Optional<DeclNameWithLoc> jvp,
                           Optional<DeclNameWithLoc> vjp,
                           TrailingWhereClause *clause) {
  unsigned numParams = parameters.size();
  unsigned size = sizeof(DifferentiableAttr) +
    numParams * sizeof(AutoDiffParameter);
  void *mem = context.Allocate(size, alignof(DifferentiableAttr));
  return new (mem) DifferentiableAttr(atLoc, baseRange, mode, modeLoc,
                                      parameters, std::move(primal),
                                      std::move(adjoint), std::move(jvp),
                                      std::move(vjp), clause);
}

ArrayRef<AutoDiffParameter>
DifferentiableAttr::getParameters() const {
  return const_cast<DifferentiableAttr *>(this)->getParameters();
}

ImplementsAttr::ImplementsAttr(SourceLoc atLoc, SourceRange range,
                               TypeLoc ProtocolType,
                               DeclName MemberName,
                               DeclNameLoc MemberNameLoc)
    : DeclAttribute(DAK_Implements, atLoc, range, /*Implicit=*/false),
      ProtocolType(ProtocolType),
      MemberName(MemberName),
      MemberNameLoc(MemberNameLoc) {
}


ImplementsAttr *ImplementsAttr::create(ASTContext &Ctx, SourceLoc atLoc,
                                       SourceRange range,
                                       TypeLoc ProtocolType,
                                       DeclName MemberName,
                                       DeclNameLoc MemberNameLoc) {
  void *mem = Ctx.Allocate(sizeof(ImplementsAttr), alignof(ImplementsAttr));
  return new (mem) ImplementsAttr(atLoc, range, ProtocolType,
                                  MemberName, MemberNameLoc);
}

TypeLoc ImplementsAttr::getProtocolType() const {
  return ProtocolType;
}

TypeLoc &ImplementsAttr::getProtocolType() {
  return ProtocolType;
}
