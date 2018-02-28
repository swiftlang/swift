//===--- DiagnosticConsumer.cpp - Diagnostic Consumer Impl ----------------===//
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
//  This file implements the DiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-basic"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

DiagnosticConsumer::~DiagnosticConsumer() = default;

llvm::SMLoc DiagnosticConsumer::getRawLoc(SourceLoc loc) {
  return loc.Value;
}

RangeSpecificDiagnosticConsumer::RangeSpecificDiagnosticConsumer(
    MutableArrayRef<ConsumerPair> consumers) {
  using MapEntry = std::pair<CharSourceRange, unsigned>;

  // Split up the ConsumerPairs into the "map" (to be sorted) and the actual
  // owning vector (preserving order).
  for (ConsumerPair &pair : consumers) {
    LocationToConsumerMap.emplace_back(pair.first, SubConsumers.size());
    SubConsumers.emplace_back(std::move(pair).second);
  }

  // Sort the "map" by buffer /end/ location, for use with std::lower_bound
  // later. (Sorting by start location would produce the same sort, since the
  // ranges must not be overlapping, but since we need to check end locations
  // later it's consistent to sort by that here.)
  std::sort(LocationToConsumerMap.begin(), LocationToConsumerMap.end(),
            [](const MapEntry &left, const MapEntry &right) -> bool {
    auto compare = std::less<const char *>();
    return compare(getRawLoc(left.first.getEnd()).getPointer(),
                   getRawLoc(right.first.getEnd()).getPointer());
  });

  // Check that the ranges are non-overlapping.
  assert(LocationToConsumerMap.end() ==
           std::adjacent_find(LocationToConsumerMap.begin(),
                              LocationToConsumerMap.end(),
                              [](const MapEntry &left, const MapEntry &right) {
                                return left.first.overlaps(right.first);
                              }) &&
         "overlapping ranges given to RangeSpecificDiagnosticConsumer");
}

DiagnosticConsumer *
RangeSpecificDiagnosticConsumer::consumerForLocation(SourceLoc loc) const {
  // "Find the first range whose end location is greater than or equal to
  // 'loc'."
  auto possiblyContainingRangeIter =
      std::lower_bound(LocationToConsumerMap.begin(),
                       LocationToConsumerMap.end(),
                       loc,
                       [](const std::pair<CharSourceRange, unsigned> &entry,
                          SourceLoc loc) -> bool {
    auto compare = std::less<const char *>();
    return compare(getRawLoc(entry.first.getEnd()).getPointer(),
                   getRawLoc(loc).getPointer());
  });

  if (possiblyContainingRangeIter != LocationToConsumerMap.end() &&
      possiblyContainingRangeIter->first.contains(loc)) {
    return SubConsumers[possiblyContainingRangeIter->second].get();
  }

  return nullptr;
}

void RangeSpecificDiagnosticConsumer::handleDiagnostic(
    SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
    StringRef FormatString, ArrayRef<DiagnosticArgument> FormatArgs,
    const DiagnosticInfo &Info) {

  DiagnosticConsumer *specificConsumer;
  switch (Kind) {
  case DiagnosticKind::Error:
  case DiagnosticKind::Warning:
  case DiagnosticKind::Remark:
    specificConsumer = consumerForLocation(Loc);
    ConsumerForSubsequentNotes = specificConsumer;
    break;
  case DiagnosticKind::Note:
    specificConsumer = ConsumerForSubsequentNotes;
    break;
  }

  if (specificConsumer) {
    specificConsumer->handleDiagnostic(SM, Loc, Kind, FormatString, FormatArgs,
                                       Info);
  } else {
    for (auto &subConsumer : SubConsumers) {
      subConsumer->handleDiagnostic(SM, Loc, Kind, FormatString, FormatArgs,
                                    Info);
    }
  }
}

bool RangeSpecificDiagnosticConsumer::finishProcessing() {
  // Deliberately don't use std::any_of here because we don't want early-exit
  // behavior.
  bool hadError = false;
  for (auto &subConsumer : SubConsumers)
    hadError |= subConsumer->finishProcessing();
  return hadError;
}

void NullDiagnosticConsumer::handleDiagnostic(
    SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
    StringRef FormatString, ArrayRef<DiagnosticArgument> FormatArgs,
    const DiagnosticInfo &Info) {
  DEBUG({
    llvm::dbgs() << "NullDiagnosticConsumer received diagnostic: ";
    DiagnosticEngine::formatDiagnosticText(llvm::dbgs(), FormatString,
                                           FormatArgs);
    llvm::dbgs() << "\n";
  });
}
