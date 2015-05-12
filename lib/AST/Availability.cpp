//===--- Availability.cpp - Swift Availability Structures -------*- C++ -*-===//
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
// This file defines data structures for API availability.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Availability.h"
#include "swift/AST/PlatformKind.h"

using namespace swift;

namespace {

/// The inferred availability required to access a group of declarations
/// on a single platform.
struct InferredAvailability {
  UnconditionalAvailabilityKind Unconditional
    = UnconditionalAvailabilityKind::None;
  
  Optional<clang::VersionTuple> Introduced;
  Optional<clang::VersionTuple> Deprecated;
  Optional<clang::VersionTuple> Obsoleted;
};

/// The type of a function that merges two version tuples.
typedef const clang::VersionTuple &(*MergeFunction)(
    const clang::VersionTuple &, const clang::VersionTuple &);

} // end anonymous namespace

/// Apply a merge function to two optional versions, returning the result
/// in Inferred.
static void
mergeIntoInferredVersion(const Optional<clang::VersionTuple> &Version,
                         Optional<clang::VersionTuple> &Inferred,
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
  Inferred.Unconditional
    = static_cast<UnconditionalAvailabilityKind>(
      std::max(static_cast<unsigned>(Inferred.Unconditional),
               static_cast<unsigned>(Attr->getUnconditionalAvailability())));

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

  clang::VersionTuple Introduced =
      Inferred.Introduced.getValueOr(clang::VersionTuple());
  clang::VersionTuple Deprecated =
      Inferred.Deprecated.getValueOr(clang::VersionTuple());
  clang::VersionTuple Obsoleted =
      Inferred.Obsoleted.getValueOr(clang::VersionTuple());

  return new (Context) AvailableAttr(
      SourceLoc(), SourceRange(), Platform,
      /*Message=*/StringRef(),
      /*Rename=*/StringRef(), Introduced, Deprecated, Obsoleted,
      Inferred.Unconditional, /*Implicit=*/true);
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
