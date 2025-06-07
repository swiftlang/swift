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

ResolvedLoc::ResolvedLoc(swift::CharSourceRange range,
                         std::vector<swift::CharSourceRange> labelRanges,
                         std::optional<unsigned> firstTrailingLabel,
                         LabelRangeType labelType, bool isActive,
                         ResolvedLocContext context)
    : range(range), labelRanges(labelRanges),
      firstTrailingLabel(firstTrailingLabel), labelType(labelType),
      isActive(isActive), context(context) {}

ResolvedLoc::ResolvedLoc() {}

BridgedResolvedLoc::BridgedResolvedLoc(BridgedCharSourceRange range,
                                       BridgedCharSourceRangeVector labelRanges,
                                       unsigned firstTrailingLabel,
                                       LabelRangeType labelType, bool isActive,
                                       ResolvedLocContext context)
    : resolvedLoc(
          new ResolvedLoc(range.unbridged(), labelRanges.takeUnbridged(),
                          firstTrailingLabel == UINT_MAX
                              ? std::nullopt
                              : std::optional<unsigned>(firstTrailingLabel),
                          labelType, isActive, context)) {}

BridgedResolvedLocVector::BridgedResolvedLocVector()
    : vector(new std::vector<BridgedResolvedLoc>()) {}

void BridgedResolvedLocVector::push_back(BridgedResolvedLoc Loc) {
  static_cast<std::vector<ResolvedLoc> *>(vector)->push_back(
      Loc.takeUnbridged());
}

BridgedResolvedLocVector::BridgedResolvedLocVector(void *opaqueValue)
    : vector(opaqueValue) {}

void *BridgedResolvedLocVector::getOpaqueValue() const { return vector; }
