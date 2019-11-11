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
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/IndexSubset.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"
#include "swift/AST/ParameterList.h"
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

#define DECL_ATTR(Name, Id, ...)                                                                     \
static_assert(DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::ABIBreakingToAdd) != \
              DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::ABIStableToAdd),     \
              #Name " needs to specify either ABIBreakingToAdd or ABIStableToAdd");
#include "swift/AST/Attr.def"

#define DECL_ATTR(Name, Id, ...)                                                                        \
static_assert(DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::ABIBreakingToRemove) != \
              DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::ABIStableToRemove),     \
              #Name " needs to specify either ABIBreakingToRemove or ABIStableToRemove");
#include "swift/AST/Attr.def"

#define DECL_ATTR(Name, Id, ...)                                                                     \
static_assert(DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::APIBreakingToAdd) != \
              DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::APIStableToAdd),     \
              #Name " needs to specify either APIBreakingToAdd or APIStableToAdd");
#include "swift/AST/Attr.def"

#define DECL_ATTR(Name, Id, ...)                                                                        \
static_assert(DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::APIBreakingToRemove) != \
              DeclAttribute::isOptionSetFor##Id(DeclAttribute::DeclAttrOptions::APIStableToRemove),     \
              #Name " needs to specify either APIBreakingToRemove or APIStableToRemove");
#include "swift/AST/Attr.def"

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

const AvailableAttr *
DeclAttributes::getPotentiallyUnavailable(const ASTContext &ctx) const {
  const AvailableAttr *potential = nullptr;
  const AvailableAttr *conditional = nullptr;

  for (auto Attr : *this)
    if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
      if (AvAttr->isInvalid())
        continue;

      if (!AvAttr->isActivePlatform(ctx) &&
          !AvAttr->isLanguageVersionSpecific() &&
          !AvAttr->isPackageDescriptionVersionSpecific())
        continue;

      // Definitely not available.
      if (AvAttr->isUnconditionallyUnavailable())
        return AvAttr;

      switch (AvAttr->getVersionAvailability(ctx)) {
      case AvailableVersionComparison::Available:
        // Doesn't limit the introduced version.
        break;

      case AvailableVersionComparison::PotentiallyUnavailable:
        // We'll return this if we don't see something that proves it's
        // not available in this version.
        potential = AvAttr;
        break;

      case AvailableVersionComparison::Unavailable:
      case AvailableVersionComparison::Obsoleted:
        conditional = AvAttr;
        break;
      }
    }

  if (conditional)
    return conditional;
  return potential;
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
          !AvAttr->isLanguageVersionSpecific() &&
          !AvAttr->isPackageDescriptionVersionSpecific())
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
          !AvAttr->isLanguageVersionSpecific() &&
          !AvAttr->isPackageDescriptionVersionSpecific())
        continue;

      // Unconditional deprecated.
      if (AvAttr->isUnconditionallyDeprecated())
        return AvAttr;

      Optional<llvm::VersionTuple> DeprecatedVersion = AvAttr->Deprecated;
      if (!DeprecatedVersion.hasValue())
        continue;

      llvm::VersionTuple MinVersion = AvAttr->getActiveVersion(ctx);

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
  case PlatformAgnosticAvailabilityKind::PackageDescriptionVersionSpecific:
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
      FirstAvail->getPlatformAgnosticAvailability() !=
      PlatformAgnosticAvailabilityKind::None) {
    assert(FirstAvail->Introduced.hasValue());
    if (FirstAvail->isLanguageVersionSpecific()) {
      Printer << "swift ";
    } else {
      assert(FirstAvail->isPackageDescriptionVersionSpecific());
      Printer << "_PackageDescription ";
    }
    Printer << FirstAvail->Introduced.getValue().getAsString()
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

static std::string getDifferentiationParametersClauseString(
    const AbstractFunctionDecl *function, IndexSubset *indices,
    ArrayRef<ParsedAutoDiffParameter> parsedParams) {
  bool isInstanceMethod = function && function->isInstanceMember();
  std::string result;
  llvm::raw_string_ostream printer(result);

  // Use parameter indices from `IndexSubset`, if specified.
  if (indices) {
    auto parameters = indices->getBitVector();
    auto parameterCount = parameters.count();
    printer << "wrt: ";
    if (parameterCount > 1)
      printer << '(';
    // Check if differentiating wrt `self`. If so, manually print it first.
    if (isInstanceMethod && parameters.test(parameters.size() - 1)) {
      parameters.reset(parameters.size() - 1);
      printer << "self";
      if (parameters.any())
        printer << ", ";
    }
    // Print remaining differentiation parameters.
    interleave(parameters.set_bits(), [&](unsigned index) {
      printer << function->getParameters()->get(index)->getName().str();
    }, [&] { printer << ", "; });
    if (parameterCount > 1)
      printer << ')';
  }
  // Otherwise, use the parsed parameters.
  else if (!parsedParams.empty()) {
    printer << "wrt: ";
    if (parsedParams.size() > 1)
      printer << '(';
    interleave(parsedParams, [&](const ParsedAutoDiffParameter &param) {
      switch (param.getKind()) {
      case ParsedAutoDiffParameter::Kind::Named:
        printer << param.getName();
        break;
      case ParsedAutoDiffParameter::Kind::Self:
        printer << "self";
        break;
      case ParsedAutoDiffParameter::Kind::Ordered:
        auto *paramList = function->getParameters();
        assert(param.getIndex() <= paramList->size() &&
               "wrt parameter is out of range");
        auto *funcParam = paramList->get(param.getIndex());
        printer << funcParam->getNameStr();
        break;
      }
    }, [&] { printer << ", "; });
    if (parsedParams.size() > 1)
      printer << ')';
  }
  return printer.str();
}

// Print the arguments of the given `@differentiable` attribute.
static void printDifferentiableAttrArguments(
    const DifferentiableAttr *attr, ASTPrinter &printer, PrintOptions Options,
    const Decl *D, bool omitWrtClause = false,
    bool omitAssociatedFunctions = false) {
  // Create a temporary string for the attribute argument text.
  std::string attrArgText;
  llvm::raw_string_ostream stream(attrArgText);

  // Get original function.
  auto *original = dyn_cast_or_null<AbstractFunctionDecl>(D);
  // Handle stored/computed properties and subscript methods.
  if (auto *asd = dyn_cast_or_null<AbstractStorageDecl>(D))
    original = asd->getAccessor(AccessorKind::Get);

  // Print comma if not leading clause.
  bool isLeadingClause = true;
  auto printCommaIfNecessary = [&] {
    if (isLeadingClause) {
      isLeadingClause = false;
      return;
    }
    stream << ", ";
  };

  // Print if the function is marked as linear.
  if (attr->isLinear()) {
    isLeadingClause = false;
    stream << "linear";
  }

  // Print differentiation parameters, unless they are to be omitted.
  if (!omitWrtClause) {
    auto diffParamsString = getDifferentiationParametersClauseString(
        original, attr->getParameterIndices(), attr->getParsedParameters());
    // Check whether differentiation parameter clause is empty.
    // Handles edge case where resolved parameter indices are unset and
    // parsed parameters are empty. This case should never trigger for
    // user-visible printing.
    if (!diffParamsString.empty()) {
      printCommaIfNecessary();
      stream << diffParamsString;
    }
  }
  // Print associated function names, unless they are to be omitted.
  if (!omitAssociatedFunctions) {
    // Print jvp function name, if specified.
    if (auto jvp = attr->getJVP()) {
      printCommaIfNecessary();
      stream << "jvp: " << jvp->Name;
    }
    // Print vjp function name, if specified.
    if (auto vjp = attr->getVJP()) {
      printCommaIfNecessary();
      stream << "vjp: " << vjp->Name;
    }
  }
  // Print 'where' clause, if any.
  // First, filter out requirements satisfied by the original function's
  // generic signature. They should not be printed.
  ArrayRef<Requirement> derivativeRequirements;
  if (auto derivativeGenSig = attr->getDerivativeGenericSignature())
    derivativeRequirements = derivativeGenSig->getRequirements();
  auto requirementsToPrint =
    llvm::make_filter_range(derivativeRequirements, [&](Requirement req) {
        if (const auto &originalGenSig = original->getGenericSignature())
          if (originalGenSig->isRequirementSatisfied(req))
            return false;
        return true;
      });
  if (!llvm::empty(requirementsToPrint)) {
    if (!isLeadingClause)
      stream << ' ';
    stream << "where ";
    std::function<Type(Type)> getInterfaceType;
    if (!original || !original->getGenericEnvironment()) {
      getInterfaceType = [](Type Ty) -> Type { return Ty; };
    } else {
      // Use GenericEnvironment to produce user-friendly
      // names instead of something like 't_0_0'.
      auto *genericEnv = original->getGenericEnvironment();
      assert(genericEnv);
      getInterfaceType = [=](Type Ty) -> Type {
        return genericEnv->getSugaredType(Ty);
      };
    }
    interleave(requirementsToPrint, [&](Requirement req) {
      if (const auto &originalGenSig = original->getGenericSignature())
        if (originalGenSig->isRequirementSatisfied(req))
          return;
      auto FirstTy = getInterfaceType(req.getFirstType());
      if (req.getKind() != RequirementKind::Layout) {
        auto SecondTy = getInterfaceType(req.getSecondType());
        Requirement ReqWithDecls(req.getKind(), FirstTy, SecondTy);
        ReqWithDecls.print(stream, Options);
      } else {
        Requirement ReqWithDecls(req.getKind(), FirstTy,
        req.getLayoutConstraint());
        ReqWithDecls.print(stream, Options);
      }
    }, [&] {
      stream << ", ";
    });
  }

  // If the attribute argument text is empty, return. Do not print parentheses.
  if (stream.str().empty())
    return;

  // Otherwise, print the attribute argument text enclosed in parentheses.
  printer << '(';
  printer << stream.str();
  printer << ')';
}

void DeclAttributes::print(ASTPrinter &Printer, const PrintOptions &Options,
                           const Decl *D) const {
  if (!DeclAttrs)
    return;

  SmallVector<const DeclAttribute *, 8> orderedAttributes(begin(), end());
  print(Printer, Options, orderedAttributes, D);
}

void DeclAttributes::print(ASTPrinter &Printer, const PrintOptions &Options,
                           ArrayRef<const DeclAttribute *> FlattenedAttrs,
                           const Decl *D) {
  using AttributeVector = SmallVector<const DeclAttribute *, 8>;

  // Process attributes in passes.
  AttributeVector shortAvailableAttributes;
  const DeclAttribute *swiftVersionAvailableAttribute = nullptr;
  const DeclAttribute *packageDescriptionVersionAvailableAttribute = nullptr;
  AttributeVector longAttributes;
  AttributeVector attributes;
  AttributeVector modifiers;

  CustomAttr *FuncBuilderAttr = nullptr;
  if (auto *VD = dyn_cast_or_null<ValueDecl>(D)) {
    FuncBuilderAttr = VD->getAttachedFunctionBuilder();
  }
  for (auto DA : llvm::reverse(FlattenedAttrs)) {
    // Always print function builder attribute.
    bool isFunctionBuilderAttr = DA == FuncBuilderAttr;
    if (!Options.PrintImplicitAttrs && DA->isImplicit())
      continue;
    if (!Options.PrintUserInaccessibleAttrs &&
        !isFunctionBuilderAttr &&
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
      if (availableAttr->isPackageDescriptionVersionSpecific() &&
          isShortAvailable(availableAttr)) {
        packageDescriptionVersionAvailableAttribute = availableAttr;
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
  if (packageDescriptionVersionAvailableAttribute)
    printShortFormAvailable(packageDescriptionVersionAvailableAttribute, Printer, Options);
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
      Printer.printKeyword(getAttrName(), Options);
    } else {
      Printer.printSimpleAttr(getAttrName(), /*needAt=*/true);
    }
    return true;

  case DAK_SetterAccess:
    Printer.printKeyword(getAttrName(), Options, "(set)");
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
    else if (Attr->isPackageDescriptionVersionSpecific())
      Printer << "_PackageDescription";
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
    if (!Attr->Message.empty()) {
      Printer << ", message: ";
      Printer.printEscapedStringLiteral(Attr->Message);
    }
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
    SmallVector<Requirement, 4> requirementsScratch;
    ArrayRef<Requirement> requirements;
    if (auto sig = attr->getSpecializedSgnature())
      requirements = sig->getRequirements();

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

      if (auto sig = attr->getSpecializedSgnature()) {
        requirementsScratch = sig->requirementsNotSatisfiedBy(
            GenericEnv->getGenericSignature());
        requirements = requirementsScratch;
      }
    }

    if (!requirements.empty()) {
      Printer << "where ";
    }

    interleave(requirements,
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
    Printer.printAttrName("@_objcRuntimeName");
    Printer << "(";
    auto *attr = cast<ObjCRuntimeNameAttr>(this);
    Printer << attr->Name;
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

  case DAK_DynamicReplacement: {
    Printer.printAttrName("@_dynamicReplacement");
    Printer << "(for: \"";
    auto *attr = cast<DynamicReplacementAttr>(this);
    Printer << attr->getReplacedFunctionName() << "\")";
    break;
  }

  case DAK_Custom: {
    Printer.printAttrName("@");
    const TypeLoc &typeLoc = cast<CustomAttr>(this)->getTypeLoc();
    if (auto type = typeLoc.getType())
      type->print(Printer, Options);
    else
      typeLoc.getTypeRepr()->print(Printer, Options);
    break;
  }

  case DAK_ProjectedValueProperty:
    Printer.printAttrName("@_projectedValueProperty");
    Printer << "(";
    Printer << cast<ProjectedValuePropertyAttr>(this)->ProjectionPropertyName;
    Printer << ")";
    break;

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
  case DAK_Custom:
    return "<<custom>>";
  case DAK_ProjectedValueProperty:
    return "_projectedValueProperty";
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

bool AvailableAttr::isPackageDescriptionVersionSpecific() const {
  if (PlatformAgnostic ==
      PlatformAgnosticAvailabilityKind::PackageDescriptionVersionSpecific)
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
  case PlatformAgnosticAvailabilityKind::PackageDescriptionVersionSpecific:
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
  case PlatformAgnosticAvailabilityKind::PackageDescriptionVersionSpecific:
    return false;

  case PlatformAgnosticAvailabilityKind::Deprecated:
    return true;
  }

  llvm_unreachable("Unhandled PlatformAgnosticAvailabilityKind in switch.");
}

llvm::VersionTuple AvailableAttr::getActiveVersion(const ASTContext &ctx) const {
  if (isLanguageVersionSpecific()) {
    return ctx.LangOpts.EffectiveLanguageVersion;
  } else if (isPackageDescriptionVersionSpecific()) {
    return ctx.LangOpts.PackageDescriptionVersion;
  } else {
    return ctx.LangOpts.getMinPlatformVersion();
  }
}

AvailableVersionComparison AvailableAttr::getVersionAvailability(
  const ASTContext &ctx) const {

  // Unconditional unavailability.
  if (isUnconditionallyUnavailable())
    return AvailableVersionComparison::Unavailable;

  llvm::VersionTuple queryVersion = getActiveVersion(ctx);

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
    if (isLanguageVersionSpecific() || isPackageDescriptionVersionSpecific())
      return AvailableVersionComparison::Unavailable;
    else
      return AvailableVersionComparison::PotentiallyUnavailable;
  }

  // The entity is available.
  return AvailableVersionComparison::Available;
}

const AvailableAttr *AvailableAttr::isUnavailable(const Decl *D) {
  ASTContext &ctx = D->getASTContext();
  if (auto attr = D->getAttrs().getUnavailable(ctx))
    return attr;

  // If D is an extension member, check if the extension is unavailable.
  //
  // Skip decls imported from Clang, they could be associated to the wrong
  // extension and inherit undesired unavailability. The ClangImporter
  // associates Objective-C protocol members to the first category where the
  // protocol is directly or indirectly adopted, no matter its availability
  // and the availability of other categories. rdar://problem/53956555
  if (!D->getClangNode())
    if (auto ext = dyn_cast<ExtensionDecl>(D->getDeclContext()))
        return AvailableAttr::isUnavailable(ext);

  return nullptr;
}

SpecializeAttr::SpecializeAttr(SourceLoc atLoc, SourceRange range,
                               TrailingWhereClause *clause,
                               bool exported,
                               SpecializationKind kind,
                               GenericSignature specializedSignature)
    : DeclAttribute(DAK_Specialize, atLoc, range,
                    /*Implicit=*/clause == nullptr),
      trailingWhereClause(clause),
      specializedSignature(specializedSignature) {
  Bits.SpecializeAttr.exported = exported;
  Bits.SpecializeAttr.kind = unsigned(kind);
}

TrailingWhereClause *SpecializeAttr::getTrailingWhereClause() const {
  return trailingWhereClause;
}

SpecializeAttr *SpecializeAttr::create(ASTContext &Ctx, SourceLoc atLoc,
                                       SourceRange range,
                                       TrailingWhereClause *clause,
                                       bool exported,
                                       SpecializationKind kind,
                                       GenericSignature specializedSignature) {
  return new (Ctx) SpecializeAttr(atLoc, range, clause, exported, kind,
                                  specializedSignature);
}

DifferentiableAttr::DifferentiableAttr(ASTContext &context, bool implicit,
                                       SourceLoc atLoc, SourceRange baseRange,
                                       bool linear,
                                       ArrayRef<ParsedAutoDiffParameter> params,
                                       Optional<DeclNameWithLoc> jvp,
                                       Optional<DeclNameWithLoc> vjp,
                                       TrailingWhereClause *clause)
  : DeclAttribute(DAK_Differentiable, atLoc, baseRange, implicit),
    linear(linear), NumParsedParameters(params.size()), JVP(std::move(jvp)),
    VJP(std::move(vjp)), WhereClause(clause) {
  std::copy(params.begin(), params.end(),
            getTrailingObjects<ParsedAutoDiffParameter>());
}

DifferentiableAttr::DifferentiableAttr(ASTContext &context, bool implicit,
                                       SourceLoc atLoc, SourceRange baseRange,
                                       bool linear,
                                       IndexSubset *indices,
                                       Optional<DeclNameWithLoc> jvp,
                                       Optional<DeclNameWithLoc> vjp,
                                       GenericSignature derivativeGenSig)
    : DeclAttribute(DAK_Differentiable, atLoc, baseRange, implicit),
      linear(linear), JVP(std::move(jvp)), VJP(std::move(vjp)),
      ParameterIndices(indices) {
  setDerivativeGenericSignature(context, derivativeGenSig);
}

DifferentiableAttr *
DifferentiableAttr::create(ASTContext &context, bool implicit,
                           SourceLoc atLoc, SourceRange baseRange,
                           bool linear,
                           ArrayRef<ParsedAutoDiffParameter> parameters,
                           Optional<DeclNameWithLoc> jvp,
                           Optional<DeclNameWithLoc> vjp,
                           TrailingWhereClause *clause) {
  unsigned size = totalSizeToAlloc<ParsedAutoDiffParameter>(parameters.size());
  void *mem = context.Allocate(size, alignof(DifferentiableAttr));
  return new (mem) DifferentiableAttr(context, implicit, atLoc, baseRange,
                                      linear, parameters, std::move(jvp),
                                      std::move(vjp), clause);
}

DifferentiableAttr *
DifferentiableAttr::create(ASTContext &context, bool implicit,
                           SourceLoc atLoc, SourceRange baseRange,
                           bool linear, IndexSubset *indices,
                           Optional<DeclNameWithLoc> jvp,
                           Optional<DeclNameWithLoc> vjp,
                           GenericSignature derivativeGenSig) {
  void *mem = context.Allocate(sizeof(DifferentiableAttr),
                               alignof(DifferentiableAttr));
  return new (mem) DifferentiableAttr(context, implicit, atLoc, baseRange,
                                      linear, indices, std::move(jvp),
                                      std::move(vjp), derivativeGenSig);
}

void DifferentiableAttr::setJVPFunction(FuncDecl *decl) {
  JVPFunction = decl;
  if (decl && !JVP)
    JVP = {decl->getFullName(), DeclNameLoc(decl->getNameLoc())};
}

void DifferentiableAttr::setVJPFunction(FuncDecl *decl) {
  VJPFunction = decl;
  if (decl && !VJP)
    VJP = {decl->getFullName(), DeclNameLoc(decl->getNameLoc())};
}

GenericEnvironment *DifferentiableAttr::getDerivativeGenericEnvironment(
    AbstractFunctionDecl *original) const {
  GenericEnvironment *derivativeGenEnv = original->getGenericEnvironment();
  if (auto derivativeGenSig = getDerivativeGenericSignature())
    return derivativeGenEnv = derivativeGenSig->getGenericEnvironment();
  return original->getGenericEnvironment();
}

void DifferentiableAttr::print(llvm::raw_ostream &OS, const Decl *D,
                               bool omitWrtClause,
                               bool omitAssociatedFunctions) const {
  StreamPrinter P(OS);
  P << "@" << getAttrName();
  printDifferentiableAttrArguments(this, P, PrintOptions(), D, omitWrtClause,
                                   omitAssociatedFunctions);
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

CustomAttr::CustomAttr(SourceLoc atLoc, SourceRange range, TypeLoc type,
                       PatternBindingInitializer *initContext, Expr *arg,
                       ArrayRef<Identifier> argLabels,
                       ArrayRef<SourceLoc> argLabelLocs, bool implicit)
    : DeclAttribute(DAK_Custom, atLoc, range, implicit),
      type(type),
      arg(arg),
      initContext(initContext) {
  hasArgLabelLocs = !argLabelLocs.empty();
  numArgLabels = argLabels.size();
  initializeCallArguments(argLabels, argLabelLocs,
                          /*hasTrailingClosure=*/false);
}

CustomAttr *CustomAttr::create(ASTContext &ctx, SourceLoc atLoc, TypeLoc type,
                               bool hasInitializer,
                               PatternBindingInitializer *initContext,
                               SourceLoc lParenLoc,
                               ArrayRef<Expr *> args,
                               ArrayRef<Identifier> argLabels,
                               ArrayRef<SourceLoc> argLabelLocs,
                               SourceLoc rParenLoc,
                               bool implicit) {
  SmallVector<Identifier, 2> argLabelsScratch;
  SmallVector<SourceLoc, 2> argLabelLocsScratch;
  Expr *arg = nullptr;
  if (hasInitializer) {
    arg = packSingleArgument(ctx, lParenLoc, args, argLabels, argLabelLocs,
                             rParenLoc, nullptr, implicit, argLabelsScratch,
                             argLabelLocsScratch);
  }

  SourceRange range(atLoc, type.getSourceRange().End);
  if (arg)
    range.End = arg->getEndLoc();

  size_t size = totalSizeToAlloc(argLabels, argLabelLocs,
                                 /*hasTrailingClosure=*/false);
  void *mem = ctx.Allocate(size, alignof(CustomAttr));
  return new (mem) CustomAttr(atLoc, range, type, initContext, arg, argLabels,
                              argLabelLocs, implicit);
}

void swift::simple_display(llvm::raw_ostream &out, const DeclAttribute *attr) {
  if (attr)
    attr->print(out);
}
