//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/IDEBridging.h"
#include "llvm/Support/raw_ostream.h"
#include <climits>

ResolvedLoc::ResolvedLoc(BridgedCharSourceRange range,
                         CharSourceRangeVector labelRanges,
                         unsigned firstTrailingLabel, LabelRangeType labelType,
                         bool isActive, ResolvedLocContext context)
    : range(range.unbridged()), labelRanges(labelRanges),
      firstTrailingLabel(firstTrailingLabel == UINT_MAX
                             ? llvm::None
                             : llvm::Optional<unsigned>(firstTrailingLabel)),
      labelType(labelType), isActive(isActive), context(context) {}

ResolvedLoc::ResolvedLoc(swift::CharSourceRange range,
                         std::vector<swift::CharSourceRange> labelRanges,
                         llvm::Optional<unsigned> firstTrailingLabel,
                         LabelRangeType labelType, bool isActive,
                         ResolvedLocContext context)
    : range(range), labelRanges(labelRanges),
      firstTrailingLabel(firstTrailingLabel), labelType(labelType),
      isActive(isActive), context(context) {}

ResolvedLoc::ResolvedLoc() {}

ResolvedLocVector ResolvedLocVector_createEmpty() {
  return ResolvedLocVector();
}

BridgedResolvedLocVector::BridgedResolvedLocVector(
    const std::vector<ResolvedLoc> &vector)
    : vector(new std::vector<ResolvedLoc>(vector)) {}

BridgedResolvedLocVector::BridgedResolvedLocVector(const void *opaqueValue)
    : vector(static_cast<const std::vector<ResolvedLoc> *>(opaqueValue)) {}

const std::vector<ResolvedLoc> &BridgedResolvedLocVector::unbridged() {
  return *vector;
}

void BridgedResolvedLocVector::destroy() { delete vector; }

const void *BridgedResolvedLocVector::getOpaqueValue() const {
  return static_cast<const void *>(vector);
}
