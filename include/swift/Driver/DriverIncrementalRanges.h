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
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
namespace driver {
class Job;
}
namespace incremental_ranges {

//==============================================================================
// MARK: SourceRangeBasedInfo
//==============================================================================

/// A per-primary collection of information about its source ranges.

class SourceRangeBasedInfo {
  SwiftRangesFileContents swiftRangesFileContents;
  /// All changed ranges in the primary as computed by the diff in the driver;
  /// really only need to know if there are any
  Ranges changedRanges;
  /// All of the non-local changes: at present only those residing outside
  /// function bodies.
  Ranges nonlocalChangedRanges;

  //==============================================================================
  // MARK: construction
  //==============================================================================

public:
  ///  return hadError and info
  static llvm::StringMap<SourceRangeBasedInfo>
  loadAllInfo(ArrayRef<const driver::Job *>, DiagnosticEngine &,
              bool showIncrementalBuildDecisions);

  SourceRangeBasedInfo(SourceRangeBasedInfo &&);

private:
  SourceRangeBasedInfo(SwiftRangesFileContents &&, Ranges &&changedRanges,
                       Ranges &&nonlocalChangedRanges);

  static Optional<SourceRangeBasedInfo> wholeFileChanged();

  /// Using supplied paths, interrogate the supplementary outputs and update the
  /// two references.
  static Optional<SourceRangeBasedInfo>
  loadInfoForOnePrimary(StringRef primaryPath, StringRef compiledSourcePath,
                        StringRef swiftRangesPath,
                        bool showIncrementalBuildDecisions,
                        DiagnosticEngine &diags);

  static Optional<SwiftRangesFileContents>
  loadSwiftRangesFileContents(StringRef swiftRangesPath, StringRef primaryPath,
                              const bool showIncrementalBuildDecisions,
                              DiagnosticEngine &);

  static Optional<Ranges>
  loadChangedRanges(StringRef compiledSourcePath, StringRef primaryPath,
                    const bool showIncrementalBuildDecisions,
                    DiagnosticEngine &);

  static Ranges computeNonlocalChangedRanges(const SwiftRangesFileContents &,
                                             const Ranges &);

  //==============================================================================
  // MARK: scheduling jobs
  //==============================================================================

public:
  /// return hadError
  static llvm::SmallPtrSet<const driver::Job *, 16>
  neededCompileJobsForRangeBasedIncrementalCompilation(
      const llvm::StringMap<SourceRangeBasedInfo> &allInfos,
      std::vector<const driver::Job *>,
      function_ref<void(const driver::Job *)> schedule,
      function_ref<void(const driver::Job *)> defer,
      function_ref<void(const driver::Job *, Twine)> noteBuilding);

private:
  static bool shouldScheduleCompileJob(
      const llvm::StringMap<SourceRangeBasedInfo> &allInfos,
      const driver::Job *, function_ref<void(Twine)>);

  static Optional<bool> isFileNewerThan(StringRef lhs, StringRef rhs,
                                        DiagnosticEngine&);

private:
  /// Return hadError

  bool didPrimaryParseAnyNonlocalNonprimaryChanges(
      StringRef primary, const llvm::StringMap<SourceRangeBasedInfo> &,
      function_ref<void(Twine)>) const;

  bool wasEveryNonprimaryNonlocalChangeUnparsed(
      StringRef primary, const llvm::StringMap<SourceRangeBasedInfo> &,
      function_ref<void(Twine)>) const;

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
