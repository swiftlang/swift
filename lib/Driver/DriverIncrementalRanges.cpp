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
    SwiftRangesFileContents &&swiftRangesFileContents,
    SourceComparator::LRRanges &&changedRanges, Ranges &&nonlocalChangedRanges)
    : swiftRangesFileContents(std::move(swiftRangesFileContents)),
      changedRanges(std::move(changedRanges)),
      nonlocalChangedRanges(std::move(nonlocalChangedRanges)) {}

SourceRangeBasedInfo::SourceRangeBasedInfo(SourceRangeBasedInfo &&x)
    : swiftRangesFileContents(std::move(x.swiftRangesFileContents)),
      changedRanges(std::move(x.changedRanges)),
      nonlocalChangedRanges(std::move(x.nonlocalChangedRanges)) {}

/// TODO: optimize by using no entry intead of the wholeFileChanged entry
Optional<SourceRangeBasedInfo> SourceRangeBasedInfo::wholeFileChanged() {
  return SourceRangeBasedInfo(SwiftRangesFileContents(),
                              SourceComparator::LRRanges::wholeFile(),
                              SerializableSourceRange::RangesForWholeFile());
}

//==============================================================================
// MARK: loading
//==============================================================================

llvm::StringMap<SourceRangeBasedInfo>
SourceRangeBasedInfo ::loadAllInfo(const Compilation &Comp) {
  const auto &jobs = Comp.getJobs();
  auto &diags = Comp.getDiags();
  const auto showIncrementalBuildDecisions =
      Comp.getShowIncrementalBuildDecisions();

  llvm::StringMap<SourceRangeBasedInfo> allInfos;

  for (const auto *Cmd : jobs) {
    StringRef primaryPath = Cmd->getFirstSwiftPrimaryInput();

    if (primaryPath.empty())
      continue;

    const StringRef compiledSourcePath =
        Cmd->getOutput().getAdditionalOutputForType(
            file_types::TY_CompiledSource);
    const StringRef swiftRangesPath =
        Cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftRanges);

    auto info =
        loadInfoForOnePrimary(primaryPath, compiledSourcePath, swiftRangesPath,
                              showIncrementalBuildDecisions, diags);
    if (!info)
      continue;
    const auto iter =
        allInfos.insert({primaryPath, std::move(info.getValue())});
    (void)iter;
    assert(iter.second && "should not be already there");
  }
  return allInfos;
}

Optional<SourceRangeBasedInfo> SourceRangeBasedInfo::loadInfoForOnePrimary(
    StringRef primaryPath, StringRef compiledSourcePath,
    StringRef swiftRangesPath, bool showIncrementalBuildDecisions,
    DiagnosticEngine &diags) {

  auto removeSupplementaryPaths = [&] {
    if (auto ec = llvm::sys::fs::remove(compiledSourcePath))
      llvm::errs() << "WARNING could not remove: " << compiledSourcePath;
    if (auto ec = llvm::sys::fs::remove(swiftRangesPath))
      llvm::errs() << "WARNING could not remove: " << swiftRangesPath;
  };

  assert(!primaryPath.empty() && "Must have a primary to load info.");

  // nonexistant primary -> it was removed since invoking swift?!
  if (!llvm::sys::fs::exists(primaryPath)) {
    if (showIncrementalBuildDecisions)
      llvm::outs() << primaryPath << " was removed.";
    // so they won't be used if primary gets re-added
    removeSupplementaryPaths();
    // Force any other file that parsed something in this one to be rebuilt.
    return wholeFileChanged();
  }

  auto swiftRangesFileContents = loadSwiftRangesFileContents(
      swiftRangesPath, primaryPath, showIncrementalBuildDecisions, diags);

  auto changedRanges = loadChangedRanges(compiledSourcePath, primaryPath,
                                         showIncrementalBuildDecisions, diags);

  if (!swiftRangesFileContents || !changedRanges) {
    removeSupplementaryPaths();
    return None;
  }

  Ranges nonlocalChangedRanges = computeNonlocalChangedRanges(
      swiftRangesFileContents.getValue(), changedRanges.getValue());
  return SourceRangeBasedInfo(std::move(swiftRangesFileContents.getValue()),
                              std::move(changedRanges.getValue()),
                              std::move(nonlocalChangedRanges));
}

Optional<SwiftRangesFileContents>
SourceRangeBasedInfo::loadSwiftRangesFileContents(
    const StringRef swiftRangesPath, const StringRef primaryPath,
    const bool showIncrementalBuildDecisions, DiagnosticEngine &diags) {
  auto bufferOrError = llvm::MemoryBuffer::getFile(swiftRangesPath);
  if (auto ec = bufferOrError.getError()) {
    diags.diagnose(SourceLoc(), diag::warn_unable_to_load_swift_ranges,
                   swiftRangesPath, ec.message());
    return None;
  }
  return SwiftRangesFileContents::load(primaryPath, *bufferOrError->get(),
                                       showIncrementalBuildDecisions, diags);
}

Optional<SourceComparator::LRRanges> SourceRangeBasedInfo::loadChangedRanges(
    const StringRef compiledSourcePath, const StringRef primaryPath,
    const bool showIncrementalBuildDecisions, DiagnosticEngine &diags) {

  // Shortcut the diff if the saved source is newer than the actual source.
  auto isPreviouslyCompiledNewer = isFileNewerThan(compiledSourcePath,
                                                   primaryPath, diags);
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

  auto whatIsAboutToBeCompiled = llvm::MemoryBuffer::getFile(primaryPath);
  if (auto ec = whatIsAboutToBeCompiled.getError()) {
    diags.diagnose(SourceLoc(), diag::warn_unable_to_load_primary, primaryPath,
                   ec.message());
    return None;
  }
  // SourceComparator::test();
  auto comp = SourceComparator(whatWasPreviouslyCompiled->get()->getBuffer(),
                               whatIsAboutToBeCompiled->get()->getBuffer());
  comp.compare();
  // lhs in terms of old version
  return comp.convertAllMismatches();
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

bool SourceRangeBasedInfo::shouldScheduleCompileJob(
    const llvm::StringMap<SourceRangeBasedInfo> &allInfos, const Job *Cmd,
    function_ref<void(bool, Twine)> noteBuilding) {
  const auto primary = Cmd->getFirstSwiftPrimaryInput();
  if (primary.empty())
    return false; // not a compile

  auto iter = allInfos.find(primary);
  if (iter == allInfos.end()) {
    noteBuilding(true, "(could not obtain range info from frontend)");
    return true;
  }
  if (!iter->second.changedRanges.empty()) {
    noteBuilding(true, "(this file changed)");
    return true;
  }
  return iter->second.didPrimaryParseAnyNonlocalNonprimaryChanges(
      llvm::sys::path::filename(primary), allInfos, noteBuilding);
}

bool SourceRangeBasedInfo::didPrimaryParseAnyNonlocalNonprimaryChanges(
    StringRef primary, const llvm::StringMap<SourceRangeBasedInfo> &allInfos,
    function_ref<void(bool, Twine)> noteBuilding) const {
  return !wasEveryNonprimaryNonlocalChangeUnparsed(primary, allInfos,
                                                   noteBuilding);
}

bool SourceRangeBasedInfo::wasEveryNonprimaryNonlocalChangeUnparsed(
    StringRef primary, const llvm::StringMap<SourceRangeBasedInfo> &allInfos,
    function_ref<void(bool, Twine)> noteBuilding) const {

  const auto &myUnparsedRangesByNonPri =
      swiftRangesFileContents.unparsedRangesByNonPrimary;
  for (const auto &info : allInfos) {
    const auto nonPri = info.getKey();
    const auto &nonPriInfo = info.getValue();
    if (nonPri == primary || nonPriInfo.nonlocalChangedRanges.empty())
      continue;
    auto unparsedRanges = myUnparsedRangesByNonPri.find(nonPri);
    const auto nonPriFilename = llvm::sys::path::filename(nonPri);
    if (unparsedRanges == myUnparsedRangesByNonPri.end()) {
      noteBuilding(
          true, Twine(nonPriFilename) +
                    " changed non-locally but I have no unparsed ranges there");
      return false;
    }
    const auto whatChanged = SerializableSourceRange::findOutlierIfAny(
        nonPriInfo.nonlocalChangedRanges, unparsedRanges->second);
    if (whatChanged) {
      noteBuilding(true, Twine("(changed: ") + nonPriFilename + ":" +
                             whatChanged->printString() + ")");
      return false;
    }
  }
  noteBuilding(false,
               "nothing that this file parsed changed in any other file");
  return true;
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

//==============================================================================
// MARK: SourceRangeBasedInfo - printing
//==============================================================================

void SourceRangeBasedInfo ::dumpAllInfo(
    const llvm::StringMap<SourceRangeBasedInfo> &allInfos,
    const bool dumpCompiledSourceDiffs, const bool dumpSwiftRanges) {
  if (!dumpSwiftRanges && !dumpCompiledSourceDiffs)
    return;
  for (const auto &info : allInfos) {
    const auto filename = llvm::sys::path::filename(info.getKey());
    if (dumpSwiftRanges)
      info.getValue().swiftRangesFileContents.dump(filename);
    if (dumpCompiledSourceDiffs)
      info.getValue().dumpChangedRanges(filename);
  }
}

void SourceRangeBasedInfo::dumpChangedRanges(
    const StringRef primaryFilename) const {
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
