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
};

/// A phrase completing "'x' is unsafe because ...". No leading capital, no
/// trailing period.
StringRef describe(CxxUnsafetyReason reason);

} // end namespace swift::importer

#endif
