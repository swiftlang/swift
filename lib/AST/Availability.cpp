//===--- Availability.cpp - Swift Availability Structures -----------------===//
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
// This file defines data structures for API availability.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/Availability.h"
#include "swift/AST/PlatformKind.h"
#include "swift/AST/TypeWalker.h"
#include <map>

using namespace swift;

AvailabilityContext AvailabilityContext::forDeploymentTarget(ASTContext &Ctx) {
  return AvailabilityContext(
      VersionRange::allGTE(Ctx.LangOpts.getMinPlatformVersion()));
}

namespace {

/// The inferred availability required to access a group of declarations
/// on a single platform.
struct InferredAvailability {
  PlatformAgnosticAvailabilityKind PlatformAgnostic
    = PlatformAgnosticAvailabilityKind::None;
  
  Optional<llvm::VersionTuple> Introduced;
  Optional<llvm::VersionTuple> Deprecated;
  Optional<llvm::VersionTuple> Obsoleted;
};

/// The type of a function that merges two version tuples.
typedef const llvm::VersionTuple &(*MergeFunction)(
    const llvm::VersionTuple &, const llvm::VersionTuple &);

} // end anonymous namespace

/// Apply a merge function to two optional versions, returning the result
/// in Inferred.
static void
mergeIntoInferredVersion(const Optional<llvm::VersionTuple> &Version,
                         Optional<llvm::VersionTuple> &Inferred,
                         MergeFunction Merge) {
  if (Version.hasValue()) {
    if (Inferred.hasValue()) {
      Inferred = Merge(Inferred.getValue(), Version.getValue());
    } else {
      Inferred = Version;
    }
  }
}

/// Merge an attribute's availability with an existing inferred availability
/// so that the new inferred availability is at least as available as
/// the attribute requires.
static void mergeWithInferredAvailability(const AvailableAttr *Attr,
                                          InferredAvailability &Inferred) {
  Inferred.PlatformAgnostic
    = static_cast<PlatformAgnosticAvailabilityKind>(
      std::max(static_cast<unsigned>(Inferred.PlatformAgnostic),
               static_cast<unsigned>(Attr->getPlatformAgnosticAvailability())));

  // The merge of two introduction versions is the maximum of the two versions.
  mergeIntoInferredVersion(Attr->Introduced, Inferred.Introduced, std::max);

  // The merge of deprecated and obsoleted versions takes the minimum.
  mergeIntoInferredVersion(Attr->Deprecated, Inferred.Deprecated, std::min);
  mergeIntoInferredVersion(Attr->Obsoleted, Inferred.Obsoleted, std::min);
}

/// Create an implicit availability attribute for the given platform
/// and with the inferred availability.
static AvailableAttr *
createAvailableAttr(PlatformKind Platform,
                       const InferredAvailability &Inferred,
                       ASTContext &Context) {

  llvm::VersionTuple Introduced =
      Inferred.Introduced.getValueOr(llvm::VersionTuple());
  llvm::VersionTuple Deprecated =
      Inferred.Deprecated.getValueOr(llvm::VersionTuple());
  llvm::VersionTuple Obsoleted =
      Inferred.Obsoleted.getValueOr(llvm::VersionTuple());

  return new (Context) AvailableAttr(
      SourceLoc(), SourceRange(), Platform,
      /*Message=*/StringRef(),
      /*Rename=*/StringRef(),
        Introduced, /*IntroducedRange=*/SourceRange(),
        Deprecated, /*DeprecatedRange=*/SourceRange(),
        Obsoleted, /*ObsoletedRange=*/SourceRange(),
      Inferred.PlatformAgnostic, /*Implicit=*/true);
}

void AvailabilityInference::applyInferredAvailableAttrs(
    Decl *ToDecl, ArrayRef<const Decl *> InferredFromDecls,
    ASTContext &Context) {

  // Iterate over the declarations and infer required availability on
  // a per-platform basis.
  std::map<PlatformKind, InferredAvailability> Inferred;
  for (const Decl *D : InferredFromDecls) {
    for (const DeclAttribute *Attr : D->getAttrs()) {
      auto *AvAttr = dyn_cast<AvailableAttr>(Attr);
      if (!AvAttr || AvAttr->isInvalid())
        continue;

      mergeWithInferredAvailability(AvAttr, Inferred[AvAttr->Platform]);
    }
  }

  // Create an availability attribute for each observed platform and add
  // to ToDecl.
  DeclAttributes &Attrs = ToDecl->getAttrs();
  for (auto &Pair : Inferred) {
    auto *Attr = createAvailableAttr(Pair.first, Pair.second, Context);
    Attrs.add(Attr);
  }
}

/// Returns true if the introduced version in \p newAttr should be used instead
/// of the introduced version in \p prevAttr when both are attached to the same
/// declaration and refer to the active platform.
static bool isBetterThan(const AvailableAttr *newAttr,
                         const AvailableAttr *prevAttr) {
  assert(newAttr);

  // If there is no prevAttr, newAttr of course wins.
  if (!prevAttr)
    return true;

  // If they belong to the same platform, the one that introduces later wins.
  if (prevAttr->Platform == newAttr->Platform)
    return prevAttr->Introduced.getValue() < newAttr->Introduced.getValue();

  // If the new attribute's platform inherits from the old one, it wins.
  return inheritsAvailabilityFromPlatform(newAttr->Platform,
                                          prevAttr->Platform);
}

Optional<AvailabilityContext>
AvailabilityInference::annotatedAvailableRange(const Decl *D, ASTContext &Ctx) {
  const AvailableAttr *bestAvailAttr = nullptr;

  for (auto Attr : D->getAttrs()) {
    auto *AvailAttr = dyn_cast<AvailableAttr>(Attr);
    if (AvailAttr == nullptr || !AvailAttr->Introduced.hasValue() ||
        !AvailAttr->isActivePlatform(Ctx) ||
        AvailAttr->isLanguageVersionSpecific() ||
        AvailAttr->isPackageDescriptionVersionSpecific()) {
      continue;
    }

    if (isBetterThan(AvailAttr, bestAvailAttr))
      bestAvailAttr = AvailAttr;
  }

  if (!bestAvailAttr)
    return None;

  return AvailabilityContext{
    VersionRange::allGTE(bestAvailAttr->Introduced.getValue())};
}

AvailabilityContext AvailabilityInference::availableRange(const Decl *D,
                                                          ASTContext &Ctx) {
  Optional<AvailabilityContext> AnnotatedRange =
      annotatedAvailableRange(D, Ctx);
  if (AnnotatedRange.hasValue()) {
    return AnnotatedRange.getValue();
  }

  // Unlike other declarations, extensions can be used without referring to them
  // by name (they don't have one) in the source. For this reason, when checking
  // the available range of a declaration we also need to check to see if it is
  // immediately contained in an extension and use the extension's availability
  // if the declaration does not have an explicit @available attribute
  // itself. This check relies on the fact that we cannot have nested
  // extensions.

  DeclContext *DC = D->getDeclContext();
  if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
    AnnotatedRange = annotatedAvailableRange(ED, Ctx);
    if (AnnotatedRange.hasValue()) {
      return AnnotatedRange.getValue();
    }
  }

  // Treat unannotated declarations as always available.
  return AvailabilityContext::alwaysAvailable();
}

namespace {
/// Infers the availability required to access a type.
class AvailabilityInferenceTypeWalker : public TypeWalker {
public:
  ASTContext &AC;
  AvailabilityContext AvailabilityInfo = AvailabilityContext::alwaysAvailable();

  AvailabilityInferenceTypeWalker(ASTContext &AC) : AC(AC) {}

  Action walkToTypePre(Type ty) override {
    if (auto *nominalDecl = ty->getAnyNominal()) {
      AvailabilityInfo.intersectWith(
          AvailabilityInference::availableRange(nominalDecl, AC));
    }

    return Action::Continue;
  }
};
} // end anonymous namespace


AvailabilityContext AvailabilityInference::inferForType(Type t) {
  AvailabilityInferenceTypeWalker walker(t->getASTContext());
  t.walk(walker);
  return walker.AvailabilityInfo;
}

AvailabilityContext ASTContext::getObjCMetadataUpdateCallbackAvailability() {
  return getSwift50Availability();
}

AvailabilityContext ASTContext::getObjCGetClassHookAvailability() {
  return getSwift50Availability();
}

AvailabilityContext ASTContext::getSwift50Availability() {
  auto target = LangOpts.Target;

  if (target.getArchName() == "arm64e")
    return AvailabilityContext::alwaysAvailable();

  if (target.isMacOSX()) {
    if (target.isAArch64())
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
                            VersionRange::allGTE(llvm::VersionTuple(10,14,4)));
  } else if (target.isiOS()) {
    if (target.isAArch64() &&
        (target.isSimulatorEnvironment() || target.isMacCatalystEnvironment()))
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
                            VersionRange::allGTE(llvm::VersionTuple(12,2)));
  } else if (target.isWatchOS()) {
    return AvailabilityContext(
                            VersionRange::allGTE(llvm::VersionTuple(5,2)));
  } else {
    return AvailabilityContext::alwaysAvailable();
  }
}

AvailabilityContext ASTContext::getOpaqueTypeAvailability() {
  return getSwift51Availability();
}

AvailabilityContext ASTContext::getObjCClassStubsAvailability() {
  return getSwift51Availability();
}

AvailabilityContext ASTContext::getSwift51Availability() {
  auto target = LangOpts.Target;
  
  if (target.getArchName() == "arm64e")
    return AvailabilityContext::alwaysAvailable();

  if (target.isMacOSX()) {
    if (target.isAArch64())
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
                            VersionRange::allGTE(llvm::VersionTuple(10,15,0)));
  } else if (target.isiOS()) {
    if (target.isAArch64() &&
        (target.isSimulatorEnvironment() || target.isMacCatalystEnvironment()))
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
                            VersionRange::allGTE(llvm::VersionTuple(13,0,0)));
  } else if (target.isWatchOS()) {
    return AvailabilityContext(
                            VersionRange::allGTE(llvm::VersionTuple(6,0,0)));
  } else {
    return AvailabilityContext::alwaysAvailable();
  }
}

AvailabilityContext ASTContext::getTypesInAbstractMetadataStateAvailability() {
  return getSwift52Availability();
}

AvailabilityContext ASTContext::getPrespecializedGenericMetadataAvailability() {
  return getSwift53Availability();
}

AvailabilityContext ASTContext::getSwift52Availability() {
  auto target = LangOpts.Target;

  if (target.getArchName() == "arm64e")
    return AvailabilityContext::alwaysAvailable();

  if (target.isMacOSX()) {
    if (target.isAArch64())
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(10, 15, 4)));
  } else if (target.isiOS()) {
    if (target.isAArch64() &&
        (target.isSimulatorEnvironment() || target.isMacCatalystEnvironment()))
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(13, 4, 0)));
  } else if (target.isWatchOS()) {
    if (target.isArch64Bit())
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(6, 2, 0)));
  }
  return AvailabilityContext::alwaysAvailable();
}

AvailabilityContext ASTContext::getSwift53Availability() {
  auto target = LangOpts.Target;

  if (target.getArchName() == "arm64e")
    return AvailabilityContext::alwaysAvailable();

  if (target.isMacOSX() ) {
    if (target.isAArch64())
      return AvailabilityContext::alwaysAvailable();

    llvm::VersionTuple macOVersion53(10, 16, 0);
    macOVersion53 = canonicalizePlatformVersion(PlatformKind::OSX, macOVersion53);
    return AvailabilityContext(
        VersionRange::allGTE(macOVersion53));
  } else if (target.isiOS()) {
    if (target.isAArch64() &&
        (target.isSimulatorEnvironment() || target.isMacCatalystEnvironment()))
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(14, 0, 0)));
  } else if (target.isWatchOS()) {
    if (target.isArch64Bit())
      return AvailabilityContext::alwaysAvailable();

    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(7, 0, 0)));
  } else {
    return AvailabilityContext::alwaysAvailable();
  }
}

AvailabilityContext ASTContext::getSwiftFutureAvailability() {
  auto target = LangOpts.Target;

  if (target.isMacOSX() ) {
    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(99, 99, 0)));
  } else if (target.isiOS()) {
    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(99, 0, 0)));
  } else if (target.isWatchOS()) {
    return AvailabilityContext(
        VersionRange::allGTE(llvm::VersionTuple(9, 99, 0)));
  } else {
    return AvailabilityContext::alwaysAvailable();
  }
}
