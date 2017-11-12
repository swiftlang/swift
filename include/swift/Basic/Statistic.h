//===--- Statistic.h - Helpers for llvm::Statistic --------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_STATISTIC_H
#define SWIFT_BASIC_STATISTIC_H

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Timer.h"

#define SWIFT_FUNC_STAT                                                 \
  do {                                                                  \
    static llvm::Statistic FStat =                                      \
      {DEBUG_TYPE, __func__, __func__, {0}, false};                     \
    ++FStat;                                                            \
  } while (0)

// Helper class designed to consolidate reporting of LLVM statistics and timers
// across swift compilations that typically invoke many drivers, each running
// many frontends. Additionally collects some cheap "always-on" statistics,
// beyond those that are (compile-time) parameterized by -DLLVM_ENABLE_STATS
// (LLVM's stats are global and involve some amount of locking and mfences).
//
// Assumes it's given a process name and target name (the latter used as
// decoration for its self-timer), and a directory to collect stats into, then:
//
//  - On construction:
//    - Calls llvm::EnableStatistics(/*PrintOnExit=*/false)
//    - Calls swift::enableCompilationTimers()
//    - Starts an llvm::NamedRegionTimer for this process
//
//  - On destruction:
//    - Add any standard always-enabled stats about the process as a whole
//    - Opens $dir/stats-$timestamp-$name-$random.json for writing
//    - Calls llvm::PrintStatisticsJSON(ostream) and/or its own writer
//
// Generally we make one of these per-process: either early in the life of the
// driver, or early in the life of the frontend.

namespace swift {

class UnifiedStatsReporter {

public:
  struct AlwaysOnDriverCounters
  {
#define DRIVER_STATISTIC(ID) size_t ID;
#include "Statistics.def"
#undef DRIVER_STATISTIC
  };

  struct AlwaysOnFrontendCounters
  {
#define FRONTEND_STATISTIC(NAME, ID) size_t ID;
#include "Statistics.def"
#undef FRONTEND_STATISTIC
  };

  struct AlwaysOnFrontendRecursiveSharedTimers {
    AlwaysOnFrontendRecursiveSharedTimers();
#define FRONTEND_RECURSIVE_SHARED_TIMER(ID) RecursiveSharedTimer ID;
#include "Statistics.def"
#undef FRONTEND_RECURSIVE_SHARED_TIMER

    int dummyInstanceVariableToGetConstructorToParse;
  };

  struct FrontendStatsTracer
  {
    UnifiedStatsReporter *Reporter;
    llvm::TimeRecord SavedTime;
    StringRef Name;
    SourceRange Range;
    FrontendStatsTracer(StringRef Name,
                        SourceRange const &Range,
                        UnifiedStatsReporter *Reporter);
    FrontendStatsTracer();
    FrontendStatsTracer(FrontendStatsTracer&& other);
    FrontendStatsTracer& operator=(FrontendStatsTracer&&);
    ~FrontendStatsTracer();
    FrontendStatsTracer(const FrontendStatsTracer&) = delete;
    FrontendStatsTracer& operator=(const FrontendStatsTracer&) = delete;
  };

  struct FrontendStatsEvent
  {
    uint64_t TimeUSec;
    uint64_t LiveUSec;
    bool IsEntry;
    StringRef EventName;
    StringRef CounterName;
    size_t CounterDelta;
    size_t CounterValue;
    SourceRange SourceRange;
  };

private:
  bool currentProcessExitStatusSet;
  int currentProcessExitStatus;
  SmallString<128> StatsFilename;
  SmallString<128> TraceFilename;
  llvm::TimeRecord StartedTime;
  std::unique_ptr<llvm::NamedRegionTimer> Timer;
  SourceManager *SourceMgr;
  std::unique_ptr<AlwaysOnDriverCounters> DriverCounters;
  std::unique_ptr<AlwaysOnFrontendCounters> FrontendCounters;
  std::unique_ptr<AlwaysOnFrontendCounters> LastTracedFrontendCounters;
  std::vector<FrontendStatsEvent> FrontendStatsEvents;
  std::unique_ptr<AlwaysOnFrontendRecursiveSharedTimers>
      FrontendRecursiveSharedTimers;

  void publishAlwaysOnStatsToLLVM();
  void printAlwaysOnStatsAndTimers(llvm::raw_ostream &OS);

  UnifiedStatsReporter(StringRef ProgramName,
                       StringRef AuxName,
                       StringRef Directory,
                       SourceManager *SM,
                       bool TraceEvents);
public:
  UnifiedStatsReporter(StringRef ProgramName,
                       StringRef ModuleName,
                       StringRef InputName,
                       StringRef TripleName,
                       StringRef OutputType,
                       StringRef OptType,
                       StringRef Directory,
                       SourceManager *SM=nullptr,
                       bool TraceEvents=false);
  ~UnifiedStatsReporter();

  AlwaysOnDriverCounters &getDriverCounters();
  AlwaysOnFrontendCounters &getFrontendCounters();
  AlwaysOnFrontendRecursiveSharedTimers &getFrontendRecursiveSharedTimers();
  void noteCurrentProcessExitStatus(int);
  FrontendStatsTracer getStatsTracer(StringRef N,
                                     SourceRange const &R);
  void saveAnyFrontendStatsEvents(FrontendStatsTracer const& T,
                                  bool IsEntry);
};

}
#endif // SWIFT_BASIC_STATISTIC_H
