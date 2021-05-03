//===--- DiagnosticCollector.h --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This defines a diagnostic collector, collecting information required for
// emitting, de-duplicating, and coalescing individual diagnostics into
// something more meaningful.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIAGNOSTICCOLLECTOR_H
#define SWIFT_BASIC_DIAGNOSTICCOLLECTOR_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

#include <type_traits>
#include <vector>

namespace swift {

/// A container for collecting multiple diagnostics that have a common anchor.
template <typename AnchorType, typename DiagnosticInformationType>
class DiagnosticCollector {
  llvm::DenseMap<AnchorType, std::vector<DiagnosticInformationType>>
      diagnostics;
  llvm::SmallVector<AnchorType> anchors;

  decltype(diagnostics.begin()) initializeOrGet(AnchorType anchor) {
    auto key = diagnostics.find(anchor);
    if (key == diagnostics.end()) {
      anchors.push_back(anchor);
      key = diagnostics.insert({anchor, {}}).first;
    }
    return key;
  }

public:
  DiagnosticCollector() {}

  void addDiagnostic(AnchorType anchor,
                     DiagnosticInformationType &&diagnostic) {
    auto key = initializeOrGet(anchor);
    key->second.push_back(std::forward<DiagnosticInformationType>(diagnostic));
  }

  template <typename... Args>
  void emplaceDiagnostic(AnchorType anchor, Args &&... args) {
    auto key = initializeOrGet(anchor);
    key->second.emplace_back(std::forward<Args>(args)...);
  }

  const llvm::ArrayRef<DiagnosticInformationType>
  getDiagnostics(AnchorType anchor) const {
    auto key = diagnostics.find(anchor);
    if (key == diagnostics.end())
      return {};
    return key->second;
  }

  /// Iterate the keys in the order they were emitted
  decltype(anchors.begin()) begin() { return anchors.begin(); }
  decltype(anchors.end()) end() { return anchors.end(); }

  bool empty() const { return anchors.empty(); }

  bool empty(AnchorType anchor) const { return getDiagnostics(anchor).empty(); }
};
} // namespace swift

#endif // SWIFT_BASIC_DIAGNOSTICCOLLECTOR_H
