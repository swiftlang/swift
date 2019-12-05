//===---------------- DriverIncrementalRanges.h ------------------*- C++-*-===//
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

// These are the declarations for managing serializable source locations so that
// the driver can implement incremental compilation based on
// source ranges.

#ifndef SWIFT_DRIVER_DRIVERINCREMENTALRANGES_H
#define SWIFT_DRIVER_DRIVERINCREMENTALRANGES_H

#include "swift/AST/IncrementalRanges.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Driver/SourceComparator.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
namespace driver {
class Job;
class Compilation;
}
namespace incremental_ranges {

//==============================================================================
// MARK: SourceRangeBasedInfo
//==============================================================================

/// A per-primary collection of information about its source ranges.

class SourceRangeBasedInfo {
  SwiftRangesFileContents swiftRangesFileContents;
  /// All changed ranges in the primary as computed by the diff in the driver
  /// Both relative to the previously-compiled and the current source.
  SourceComparator::LRRanges changedRanges;
  /// All of the non-local changes in the previously-compiled code:  those
  /// residing outside function bodies.
  /// (We only have and only need function-body ranges for the
  /// previously-compiled source.)
  Ranges nonlocalChangedRanges;

  //==============================================================================
  // MARK: construction
  //==============================================================================

public:
  ///  return hadError and info
  static llvm::StringMap<SourceRangeBasedInfo>
  loadAllInfo(const driver::Compilation &);

  SourceRangeBasedInfo(SourceRangeBasedInfo &&);

private:
  SourceRangeBasedInfo(SwiftRangesFileContents &&,
                       SourceComparator::LRRanges &&changedRanges,
                       Ranges &&nonlocalChangedRanges);

  static Optional<SourceRangeBasedInfo> wholeFileChanged();

  /// Using supplied paths, interrogate the supplementary outputs and update the
  /// two references. Return None if a file was missing or corrupted.
  static Optional<SourceRangeBasedInfo>
  loadInfoForOnePrimary(StringRef primaryPath, StringRef compiledSourcePath,
                        StringRef swiftRangesPath,
                        bool showIncrementalBuildDecisions,
                        DiagnosticEngine &diags);

  /// Return None for error
  static Optional<SwiftRangesFileContents>
  loadSwiftRangesFileContents(StringRef swiftRangesPath, StringRef primaryPath,
                              const bool showIncrementalBuildDecisions,
                              DiagnosticEngine &);

  /// Return None for error
  /// If no error returns ranges of changes from both lhs and rhs.
  /// Lhs used for scheduling jobs, rhs just for dumping and testing.
  static Optional<SourceComparator::LRRanges>
  loadChangedRanges(StringRef compiledSourcePath, StringRef primaryPath,
                    const bool showIncrementalBuildDecisions,
                    DiagnosticEngine &);

  static Ranges
  computeNonlocalChangedRanges(const SwiftRangesFileContents &,
                               const SourceComparator::LRRanges &);

  //==============================================================================
  // MARK: scheduling jobs
  //==============================================================================

public:
  static bool shouldScheduleCompileJob(
      const llvm::StringMap<SourceRangeBasedInfo> &allInfos,
      const driver::Job *, function_ref<void(bool, Twine)>);

private:
  static Optional<bool> isFileNewerThan(StringRef lhs, StringRef rhs,
                                        DiagnosticEngine&);

private:
  /// Return hadError

  bool didPrimaryParseAnyNonlocalNonprimaryChanges(
      StringRef primary, const llvm::StringMap<SourceRangeBasedInfo> &,
      function_ref<void(bool, Twine)>) const;

  bool wasEveryNonprimaryNonlocalChangeUnparsed(
      StringRef primary, const llvm::StringMap<SourceRangeBasedInfo> &,
      function_ref<void(bool, Twine)>) const;

  //==============================================================================
  // MARK: printing
  //==============================================================================

public:
  static void dumpAllInfo(const llvm::StringMap<SourceRangeBasedInfo> &,
                          bool dumpCompiledSourceDiffs, bool dumpSwiftRanges);

private:
  void dumpChangedRanges(StringRef primary) const;
};

} // namespace incremental_ranges
} // namespace swift

#endif /* SWIFT_DRIVER_DRIVERINCREMENTALRANGES_H */
