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

#define DEBUG_TYPE "swift-ast"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

DiagnosticConsumer::~DiagnosticConsumer() = default;

llvm::SMLoc DiagnosticConsumer::getRawLoc(SourceLoc loc) {
  return loc.Value;
}

LLVM_ATTRIBUTE_UNUSED
static bool hasDuplicateFileNames(
    ArrayRef<FileSpecificDiagnosticConsumer::ConsumerPair> consumers) {
  llvm::StringSet<> seenFiles;
  for (const auto &consumerPair : consumers) {
    if (consumerPair.first.empty()) {
      // We can handle multiple consumers that aren't associated with any file,
      // because they only collect diagnostics that aren't in any of the special
      // files. This isn't an important use case to support, but also SmallSet
      // doesn't handle empty strings anyway!
      continue;
    }

    bool isUnique = seenFiles.insert(consumerPair.first).second;
    if (!isUnique)
      return true;
  }
  return false;
}

FileSpecificDiagnosticConsumer::FileSpecificDiagnosticConsumer(
    SmallVectorImpl<ConsumerPair> &consumers)
  : SubConsumers(std::move(consumers)) {
  assert(!SubConsumers.empty() &&
         "don't waste time handling diagnostics that will never get emitted");
  assert(!hasDuplicateFileNames(SubConsumers) &&
         "having multiple consumers for the same file is not implemented");
}

void FileSpecificDiagnosticConsumer::computeConsumersOrderedByRange(
    SourceManager &SM) {
  // Look up each file's source range and add it to the "map" (to be sorted).
  for (const ConsumerPair &pair : SubConsumers) {
    if (pair.first.empty())
      continue;

    Optional<unsigned> bufferID = SM.getIDForBufferIdentifier(pair.first);
    assert(bufferID.hasValue() && "consumer registered for unknown file");
    CharSourceRange range = SM.getRangeForBuffer(bufferID.getValue());
    ConsumersOrderedByRange.emplace_back(
        ConsumerSpecificInformation(range, pair.second.get()));
  }

  // Sort the "map" by buffer /end/ location, for use with std::lower_bound
  // later. (Sorting by start location would produce the same sort, since the
  // ranges must not be overlapping, but since we need to check end locations
  // later it's consistent to sort by that here.)
  std::sort(ConsumersOrderedByRange.begin(), ConsumersOrderedByRange.end(),
            [](const ConsumerSpecificInformation &left,
               const ConsumerSpecificInformation &right) -> bool {
              auto compare = std::less<const char *>();
              return compare(getRawLoc(left.range.getEnd()).getPointer(),
                             getRawLoc(right.range.getEnd()).getPointer());
            });

  // Check that the ranges are non-overlapping. If the files really are all
  // distinct, this should be trivially true, but if it's ever not we might end
  // up mis-filing diagnostics.
  assert(ConsumersOrderedByRange.end() ==
             std::adjacent_find(ConsumersOrderedByRange.begin(),
                                ConsumersOrderedByRange.end(),
                                [](const ConsumerSpecificInformation &left,
                                   const ConsumerSpecificInformation &right) {
                                  return left.range.overlaps(right.range);
                                }) &&
         "overlapping ranges despite having distinct files");
}

Optional<FileSpecificDiagnosticConsumer::ConsumerSpecificInformation *>
FileSpecificDiagnosticConsumer::consumerSpecificInformationForLocation(
    SourceManager &SM, SourceLoc loc) const {
  // Diagnostics with invalid locations always go to every consumer.
  if (loc.isInvalid())
    return None;

  // This map is generated on first use and cached, to allow the
  // FileSpecificDiagnosticConsumer to be set up before the source files are
  // actually loaded.
  if (ConsumersOrderedByRange.empty()) {

    // It's possible to get here while a bridging header PCH is being
    // attached-to, if there's some sort of AST-reader warning or error, which
    // happens before CompilerInstance::setUpInputs(), at which point _no_
    // source buffers are loaded in yet. In that case we return None, rather
    // than trying to build a nonsensical map (and actually crashing since we
    // can't find buffers for the inputs).
    assert(!SubConsumers.empty());
    if (!SM.getIDForBufferIdentifier(SubConsumers.begin()->first).hasValue()) {
      assert(llvm::none_of(SubConsumers, [&](const ConsumerPair &pair) {
            return SM.getIDForBufferIdentifier(pair.first).hasValue();
          }));
      return None;
    }
    auto *mutableThis = const_cast<FileSpecificDiagnosticConsumer*>(this);
    mutableThis->computeConsumersOrderedByRange(SM);
  }

  // This std::lower_bound call is doing a binary search for the first range
  // that /might/ contain 'loc'. Specifically, since the ranges are sorted
  // by end location, it's looking for the first range where the end location
  // is greater than or equal to 'loc'.
  const ConsumerSpecificInformation *possiblyContainingRangeIter =
      std::lower_bound(
          ConsumersOrderedByRange.begin(), ConsumersOrderedByRange.end(), loc,
          [](const ConsumerSpecificInformation &entry, SourceLoc loc) -> bool {
            auto compare = std::less<const char *>();
            return compare(getRawLoc(entry.range.getEnd()).getPointer(),
                           getRawLoc(loc).getPointer());
          });

  if (possiblyContainingRangeIter != ConsumersOrderedByRange.end() &&
      possiblyContainingRangeIter->range.contains(loc)) {
    return const_cast<ConsumerSpecificInformation *>(
        possiblyContainingRangeIter);
  }

  return None;
}

void FileSpecificDiagnosticConsumer::handleDiagnostic(
    SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
    StringRef FormatString, ArrayRef<DiagnosticArgument> FormatArgs,
    const DiagnosticInfo &Info) {

  HasAnErrorBeenConsumed |= Kind == DiagnosticKind::Error;

  Optional<ConsumerSpecificInformation *> consumerSpecificInfo;
  switch (Kind) {
  case DiagnosticKind::Error:
  case DiagnosticKind::Warning:
  case DiagnosticKind::Remark:
    consumerSpecificInfo = consumerSpecificInformationForLocation(SM, Loc);
    ConsumerSpecificInfoForSubsequentNotes = consumerSpecificInfo;
    break;
  case DiagnosticKind::Note:
    consumerSpecificInfo = ConsumerSpecificInfoForSubsequentNotes;
    break;
  }
  if (!consumerSpecificInfo.hasValue()) {
    for (auto &subConsumer : SubConsumers) {
      if (subConsumer.second) {
        subConsumer.second->handleDiagnostic(SM, Loc, Kind, FormatString,
                                             FormatArgs, Info);
      }
    }
    return;
  }
  if (!consumerSpecificInfo.getValue()->consumer)
    return; // Suppress non-primary diagnostic in batch mode.

  consumerSpecificInfo.getValue()->consumer->handleDiagnostic(
      SM, Loc, Kind, FormatString, FormatArgs, Info);
  consumerSpecificInfo.getValue()->hasAnErrorBeenEmitted |=
      Kind == DiagnosticKind::Error;
}

bool FileSpecificDiagnosticConsumer::finishProcessing() {
  tellSubconsumersToInformDriverOfIncompleteBatchModeCompilation();

  // Deliberately don't use std::any_of here because we don't want early-exit
  // behavior.

  bool hadError = false;
  for (auto &subConsumer : SubConsumers)
    hadError |= subConsumer.second && subConsumer.second->finishProcessing();
  return hadError;
}

void FileSpecificDiagnosticConsumer::
    tellSubconsumersToInformDriverOfIncompleteBatchModeCompilation() const {
  if (!HasAnErrorBeenConsumed)
    return;
  for (auto &info : ConsumersOrderedByRange) {
    if (!info.hasAnErrorBeenEmitted && info.consumer)
      info.consumer->informDriverOfIncompleteBatchModeCompilation();
  }
}

void NullDiagnosticConsumer::handleDiagnostic(
    SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
    StringRef FormatString, ArrayRef<DiagnosticArgument> FormatArgs,
    const DiagnosticInfo &Info) {
  LLVM_DEBUG({
    llvm::dbgs() << "NullDiagnosticConsumer received diagnostic: ";
    DiagnosticEngine::formatDiagnosticText(llvm::dbgs(), FormatString,
                                           FormatArgs);
    llvm::dbgs() << "\n";
  });
}
