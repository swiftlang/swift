//===--- CxxUnsafetyReason.h - Why a C++ decl is unsafe ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CLANGIMPORTER_CXXUNSAFETYREASON_H
#define SWIFT_CLANGIMPORTER_CXXUNSAFETYREASON_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace clang {
class NamedDecl;
class RecordDecl;
} // end namespace clang

namespace swift::importer {

/// Why the importer judged a C++ entity unsafe. Each case corresponds to one
/// branch of one heuristic; the branch that decides the verdict is the one that
/// records the reason, so the two cannot disagree.
enum class CxxUnsafetyReason {
  /// 'begin'/'end' are assumed to return iterators.
  IteratorFromBeginEnd,
  /// Returns a pointer or reference out of a type that owns its storage.
  PointerProjection,
  /// A standard library method known to be hard to use correctly.
  KnownUnsafeStdMethod,
  /// The return type is an iterator.
  ReturnsIterator,
  /// Returns a view into a type that owns its storage.
  ViewProjection,

  /// A field of the record is unsafe.
  UnsafeField,
  /// A base class of the record is unsafe.
  UnsafeBase,
  /// A template argument of the record is unsafe.
  UnsafeTemplateArgument,
  /// The record carries an explicit unsafe annotation.
  ExplicitAnnotation,
  /// A non-escapable view with a lifetime dependency Swift cannot track.
  IndirectView,

  /// The result is non-escapable and its lifetime was inferred, not written.
  InferredResultDependence,
  /// A non-escapable parameter has no lifetime annotation.
  UnannotatedNonEscapableParam,

  // A lifetime annotation was written but Swift could not represent it. Each
  // case names the situation that defeated it.
  //
  /// The annotated result is Escapable, so Swift drops the dependency.
  SkippedLifetimeEscapableResult,
  /// The annotated parameter is imported as a class.
  SkippedLifetimeImportedAsClass,
  /// The annotated parameter is an rvalue reference, so it is not guaranteed to
  /// outlive the call.
  SkippedLifetimeRValueReference,
  /// The annotated parameter has no borrowable storage.
  SkippedLifetimeNoBorrowableStorage,
};

/// Why a C++ type's escapability could not be determined. Each case is one
/// place ClangTypeEscapability gives up; that computation records the reason, so
/// an explanation cannot contradict the verdict it explains.
enum class CxxUnknownEscapabilityReason {
  /// A conditionally-escapable type (std::shared_ptr, SWIFT_ESCAPABLE_IF) whose
  /// argument has unknown escapability.
  ConditionalArgument,
  /// A record too complex to infer from, so only an annotation would settle it.
  CannotDeriveFromMembers,
  /// A pointer or reference, which is deliberately treated as unknown.
  Pointer,
};

/// Why an entity is unsafe, and what to blame for it.
struct CxxUnsafetyExplanation {
  CxxUnsafetyReason reason;
  /// The declaration to name, or null for reasons that have nothing to name.
  const clang::NamedDecl *culprit = nullptr;
};

/// Why a type's escapability is unknown, and what to blame for it.
struct CxxUnknownEscapability {
  CxxUnknownEscapabilityReason reason;
  /// The declaration to name, or null for reasons that have nothing to name.
  const clang::NamedDecl *culprit = nullptr;
  /// The record \c culprit belongs to. The escapability traversal is flattened,
  /// so a reason can be found below the type being explained; when this is a
  /// different record, the explanation belongs to that one.
  const clang::RecordDecl *owner = nullptr;
};

/// A phrase completing "'x' is unsafe because ...". No leading capital, no
/// trailing period.
///
/// Reasons that name a declaration fall back to wording without one when
/// \p culprit is null, so callers need not check.
std::string describe(CxxUnsafetyReason reason,
                     const clang::NamedDecl *culprit = nullptr);

/// A phrase completing "'x' has unknown escapability because ...".
std::string describe(CxxUnknownEscapabilityReason reason,
                     const clang::NamedDecl *culprit = nullptr);

} // end namespace swift::importer

#endif
