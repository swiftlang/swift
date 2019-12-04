//===------ DriverIncrementalRanges.cpp ------------------------------------==//
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

#include "swift/Driver/DriverIncrementalRanges.h"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/SourceComparator.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <string>
#include <unordered_map>

// These are the definitions for managing serializable source locations so that
// the driver can implement incremental compilation based on
// source ranges.

using namespace swift;
using namespace incremental_ranges;
using namespace driver;


//==============================================================================
// MARK: SourceRangeBasedInfo - constructing
//==============================================================================

SourceRangeBasedInfo::SourceRangeBasedInfo(
    StringRef primaryInputPath,
    SwiftRangesFileContents &&swiftRangesFileContents,
    SourceComparator::LRRanges &&changedRanges, Ranges &&nonlocalChangedRanges)
    : primaryInputPath(primaryInputPath),
      swiftRangesFileContents(std::move(swiftRangesFileContents)),
      changedRanges(std::move(changedRanges)),
      nonlocalChangedRanges(std::move(nonlocalChangedRanges)) {
  assert(!primaryInputPath.empty() && "Must be a compile job");
}

SourceRangeBasedInfo::SourceRangeBasedInfo(SourceRangeBasedInfo &&x)
    : primaryInputPath(x.primaryInputPath),
      swiftRangesFileContents(std::move(x.swiftRangesFileContents)),
      changedRanges(std::move(x.changedRanges)),
      nonlocalChangedRanges(std::move(x.nonlocalChangedRanges)) {}

//==============================================================================
// MARK: loading
//==============================================================================

Optional<SourceRangeBasedInfo> SourceRangeBasedInfo::loadInfoForOneJob(
    const Job *cmd, const bool showIncrementalBuildDecisions,
    DiagnosticEngine &diags) {
  StringRef primaryInputPath = cmd->getFirstSwiftPrimaryInput();
  if (primaryInputPath.empty())
    return None;

  const StringRef compiledSourcePath =
      cmd->getOutput().getAdditionalOutputForType(
          file_types::TY_CompiledSource);
  const StringRef swiftRangesPath =
      cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftRanges);

  return loadInfoForOnePrimary(primaryInputPath, compiledSourcePath,
                               swiftRangesPath, showIncrementalBuildDecisions,
                               diags);
}

Optional<SourceRangeBasedInfo> SourceRangeBasedInfo::loadInfoForOnePrimary(
    StringRef primaryInputPath, StringRef compiledSourcePath,
    StringRef swiftRangesPath, bool showIncrementalBuildDecisions,
    DiagnosticEngine &diags) {

  auto removeSupplementaryPaths = [&] {
    if (auto ec = llvm::sys::fs::remove(compiledSourcePath))
      llvm::errs() << "WARNING could not remove: " << compiledSourcePath;
    if (auto ec = llvm::sys::fs::remove(swiftRangesPath))
      llvm::errs() << "WARNING could not remove: " << swiftRangesPath;
  };

  assert(!primaryInputPath.empty() && "Must have a primary to load info.");

  // nonexistant primary -> it was removed since invoking swift?!
  if (!llvm::sys::fs::exists(primaryInputPath)) {
    if (showIncrementalBuildDecisions)
      llvm::outs() << primaryInputPath << " was removed.";
    // so they won't be used if primary gets re-added
    removeSupplementaryPaths();
    return None;
  }

  auto swiftRangesFileContents = loadSwiftRangesFileContents(
      swiftRangesPath, primaryInputPath, showIncrementalBuildDecisions, diags);

  auto changedRanges = loadChangedRanges(compiledSourcePath, primaryInputPath,
                                         showIncrementalBuildDecisions, diags);

  if (!swiftRangesFileContents || !changedRanges) {
    removeSupplementaryPaths();
    return None;
  }

  Ranges nonlocalChangedRanges = computeNonlocalChangedRanges(
      swiftRangesFileContents.getValue(), changedRanges.getValue());
  return SourceRangeBasedInfo(
      primaryInputPath, std::move(swiftRangesFileContents.getValue()),
      std::move(changedRanges.getValue()), std::move(nonlocalChangedRanges));
}

Optional<SwiftRangesFileContents>
SourceRangeBasedInfo::loadSwiftRangesFileContents(
    const StringRef swiftRangesPath, const StringRef primaryInputPath,
    const bool showIncrementalBuildDecisions, DiagnosticEngine &diags) {
  auto bufferOrError = llvm::MemoryBuffer::getFile(swiftRangesPath);
  if (auto ec = bufferOrError.getError()) {
    diags.diagnose(SourceLoc(), diag::warn_unable_to_load_swift_ranges,
                   swiftRangesPath, ec.message());
    return None;
  }
  return SwiftRangesFileContents::load(primaryInputPath, *bufferOrError->get(),
                                       showIncrementalBuildDecisions, diags);
}

Optional<SourceComparator::LRRanges> SourceRangeBasedInfo::loadChangedRanges(
    const StringRef compiledSourcePath, const StringRef primaryInputPath,
    const bool showIncrementalBuildDecisions, DiagnosticEngine &diags) {

  // Shortcut the diff if the saved source is newer than the actual source.
  auto isPreviouslyCompiledNewer =
      isFileNewerThan(compiledSourcePath, primaryInputPath, diags);
  if (!isPreviouslyCompiledNewer)
    return None;
  if (isPreviouslyCompiledNewer.getValue())
    return SourceComparator::LRRanges(); // no changes

  auto whatWasPreviouslyCompiled =
      llvm::MemoryBuffer::getFile(compiledSourcePath);
  if (auto ec = whatWasPreviouslyCompiled.getError()) {
    diags.diagnose(SourceLoc(), diag::warn_unable_to_load_compiled_swift,
                   compiledSourcePath, ec.message());
    return None;
  }

  auto whatIsAboutToBeCompiled = llvm::MemoryBuffer::getFile(primaryInputPath);
  if (auto ec = whatIsAboutToBeCompiled.getError()) {
    diags.diagnose(SourceLoc(), diag::warn_unable_to_load_primary,
                   primaryInputPath, ec.message());
    return None;
  }
  // SourceComparator::test();
  auto comp = SourceComparator(whatWasPreviouslyCompiled->get()->getBuffer(),
                               whatIsAboutToBeCompiled->get()->getBuffer());
  comp.compare();
  // lhs in terms of old version
  return comp.convertAllMismatches();
}

/// Return true if lhs is newer than rhs, or None for error.
Optional<bool> SourceRangeBasedInfo::isFileNewerThan(StringRef lhs,
                                                     StringRef rhs,
                                                     DiagnosticEngine &diags) {
  auto getModTime = [&](StringRef path) -> Optional<llvm::sys::TimePoint<>> {
    llvm::sys::fs::file_status status;
    if (auto statError = llvm::sys::fs::status(path, status)) {
      diags.diagnose(SourceLoc(), diag::warn_cannot_stat_input,
                     llvm::sys::path::filename(path), statError.message());
      return None;
    }
    return status.getLastModificationTime();
  };
  const auto lhsModTime = getModTime(lhs);
  const auto rhsModTime = getModTime(rhs);
  return !lhsModTime || !rhsModTime
             ? None
             : Optional<bool>(lhsModTime.getValue() > rhsModTime.getValue());
}

Optional<SwiftRangesFileContents> SwiftRangesFileContents::load(
    const StringRef primaryPath, const llvm::MemoryBuffer &swiftRangesBuffer,
    const bool showIncrementalBuildDecisions, DiagnosticEngine &diags) {

  if (!swiftRangesBuffer.getBuffer().startswith(header)) {
    diags.diagnose(SourceLoc(), diag::warn_bad_swift_ranges_header,
                   swiftRangesBuffer.getBufferIdentifier());
    return None;
  }

  llvm::yaml::Input yamlReader(llvm::MemoryBufferRef(swiftRangesBuffer),
                               nullptr);

  SwiftRangesFileContents contents;
  yamlReader >> contents;
  if (yamlReader.error()) {
    diags.diagnose(SourceLoc(), diag::warn_bad_swift_ranges_format,
                   swiftRangesBuffer.getBufferIdentifier(),
                   yamlReader.error().message());
    return None;
  }
  return contents;
}

Ranges SourceRangeBasedInfo::computeNonlocalChangedRanges(
    const SwiftRangesFileContents &swiftRangesFileContents,
    const SourceComparator::LRRanges &changedRanges) {
  return SerializableSourceRange::findAllOutliers(
      changedRanges.lhs(), swiftRangesFileContents.noninlinableFunctionBodies);
}
//==============================================================================
// MARK: scheduling
//==============================================================================

static std::string rangeStrings(std::string prefix, const Ranges &ranges) {
  std::string s = prefix;
  interleave(ranges.begin(), ranges.end(),
             [&](const SerializableSourceRange &r) { s += r.printString(); },
             [&] { s += ", "; });
  return s;
}

bool SourceRangeBasedInfo::didInputChangeAtAll(
    DiagnosticEngine &,
    function_ref<void(bool, StringRef)> noteBuilding) const {
  const auto &changesToOldSource = changedRanges.lhs();
  if (changesToOldSource.empty())
    noteBuilding(/*willBeBuilding=*/false, "Did not change at all");
  else
    noteBuilding(/*willBeBuilding=*/true,
                 rangeStrings("changed at ", changesToOldSource));
  return !changesToOldSource.empty();
}

bool SourceRangeBasedInfo::didInputChangeNonlocally(
    DiagnosticEngine &,
    function_ref<void(bool, StringRef)> noteInitiallyCascading) const {
  if (nonlocalChangedRanges.empty())
    noteInitiallyCascading(false, "did not change outside any function bodies");
  else
    noteInitiallyCascading(true,
                           rangeStrings("changed outside a function body at: ",
                                        nonlocalChangedRanges));
  return !nonlocalChangedRanges.empty();
}


//==============================================================================
// MARK: SourceRangeBasedInfo - printing
//==============================================================================

void SourceRangeBasedInfo::dump(const bool dumpCompiledSourceDiffs,
                                const bool dumpSwiftRanges) const {
  if (!dumpSwiftRanges && !dumpCompiledSourceDiffs)
    return;
  if (dumpSwiftRanges)
    swiftRangesFileContents.dump(primaryInputPath);
  if (dumpCompiledSourceDiffs)
    dumpChangedRanges();
}

void SourceRangeBasedInfo::dumpChangedRanges() const {
  const auto primaryFilename = llvm::sys::path::filename(primaryInputPath);

  auto dumpRangeSet = [&](StringRef which, StringRef wrt,
                          const Ranges &ranges) {
    llvm::errs() << "*** " << which << " changed ranges in '" << primaryFilename
                 << "' (w.r.t " << wrt << ") ***\n";
    for (const auto &r : ranges)
      llvm::errs() << "- " << r.printString() << "\n";
  };
  if (changedRanges.empty()) {
    assert(nonlocalChangedRanges.empty() && "A fortiori.");
    dumpRangeSet("no", "previously- or about-to-be-compiled", {});
    return;
  }
  dumpRangeSet("all", "previously-compiled", changedRanges.lhs());
  dumpRangeSet("all", "to-be-compiled", changedRanges.rhs());
  dumpRangeSet("nonlocal", "previously-compiled", nonlocalChangedRanges);
  llvm::errs() << "\n";
}
