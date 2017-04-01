//===--- Statistic.cpp - Swift unified stats reporting --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Statistic.h"
#include "swift/Driver/DependencyGraph.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"
#include <chrono>

namespace swift {
using namespace llvm;
using namespace llvm::sys;

static std::string
makeFileName(StringRef ProcessName) {
  std::string tmp;
  raw_string_ostream stream(tmp);
  auto now = std::chrono::system_clock::now();
  stream << "stats"
         << "-" << now.time_since_epoch().count()
         << "-" << ProcessName
         << "-" << Process::GetRandomNumber()
         << ".json";
  return stream.str();
}

UnifiedStatsReporter::UnifiedStatsReporter(StringRef ProgramName,
                                           StringRef TargetName,
                                           StringRef Directory)
  : Filename(Directory),
    Timer(make_unique<NamedRegionTimer>(TargetName, "Building Target",
                                        ProgramName, "Running Program"))
{
  path::append(Filename, makeFileName(ProgramName));
  EnableStatistics(/*PrintOnExit=*/false);
  SharedTimer::enableCompilationTimers();
}

UnifiedStatsReporter::AlwaysOnDriverCounters &
UnifiedStatsReporter::getDriverCounters()
{
  if (!DriverCounters)
    DriverCounters = make_unique<AlwaysOnDriverCounters>();
  return *DriverCounters;
}

UnifiedStatsReporter::AlwaysOnFrontendCounters &
UnifiedStatsReporter::getFrontendCounters()
{
  if (!FrontendCounters)
    FrontendCounters = make_unique<AlwaysOnFrontendCounters>();
  return *FrontendCounters;
}

#define PUBLISH_STAT(C,TY,NAME)                               \
  do {                                                        \
    static Statistic Stat = {TY, #NAME, #NAME, {0}, false};   \
    Stat += (C).NAME;                                         \
  } while(0)

void
UnifiedStatsReporter::publishAlwaysOnStatsToLLVM() {
  if (FrontendCounters) {
    auto &C = getFrontendCounters();
    PUBLISH_STAT(C, "SILModule", NumSILGenFunctions);
    PUBLISH_STAT(C, "SILModule", NumSILGenVtables);
    PUBLISH_STAT(C, "SILModule", NumSILGenWitnessTables);
    PUBLISH_STAT(C, "SILModule", NumSILGenDefaultWitnessTables);
    PUBLISH_STAT(C, "SILModule", NumSILGenGlobalVariables);

    PUBLISH_STAT(C, "SILModule", NumSILOptFunctions);
    PUBLISH_STAT(C, "SILModule", NumSILOptVtables);
    PUBLISH_STAT(C, "SILModule", NumSILOptWitnessTables);
    PUBLISH_STAT(C, "SILModule", NumSILOptDefaultWitnessTables);
    PUBLISH_STAT(C, "SILModule", NumSILOptGlobalVariables);
  }
  if (DriverCounters) {
    auto &C = getDriverCounters();
    PUBLISH_STAT(C, "Driver", NumDriverJobsRun);
    PUBLISH_STAT(C, "Driver", NumDriverJobsSkipped);

    PUBLISH_STAT(C, "Driver", DriverDepCascadingTopLevel);
    PUBLISH_STAT(C, "Driver", DriverDepCascadingDynamic);
    PUBLISH_STAT(C, "Driver", DriverDepCascadingNominal);
    PUBLISH_STAT(C, "Driver", DriverDepCascadingMember);
    PUBLISH_STAT(C, "Driver", DriverDepCascadingExternal);

    PUBLISH_STAT(C, "Driver", DriverDepTopLevel);
    PUBLISH_STAT(C, "Driver", DriverDepDynamic);
    PUBLISH_STAT(C, "Driver", DriverDepNominal);
    PUBLISH_STAT(C, "Driver", DriverDepMember);
    PUBLISH_STAT(C, "Driver", DriverDepExternal);
  }
}

#define PRINT_STAT(OS,DELIM,C,TY,NAME)                   \
  do {                                                   \
    OS << DELIM << "\t\"" TY "." #NAME "\": " << C.NAME; \
    delim = ",\n";                                       \
  } while(0)

void
UnifiedStatsReporter::printAlwaysOnStatsAndTimers(raw_ostream &OS) {
  // Adapted from llvm::PrintStatisticsJSON
  OS << "{\n";
  const char *delim = "";
  if (FrontendCounters) {
    auto &C = getFrontendCounters();
    PRINT_STAT(OS, delim, C, "SILModule", NumSILGenFunctions);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILGenVtables);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILGenWitnessTables);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILGenDefaultWitnessTables);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILGenGlobalVariables);

    PRINT_STAT(OS, delim, C, "SILModule", NumSILOptFunctions);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILOptVtables);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILOptWitnessTables);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILOptDefaultWitnessTables);
    PRINT_STAT(OS, delim, C, "SILModule", NumSILOptGlobalVariables);
  }
  if (DriverCounters) {
    auto &C = getDriverCounters();
    PRINT_STAT(OS, delim, C, "Driver", NumDriverJobsRun);
    PRINT_STAT(OS, delim, C, "Driver", NumDriverJobsSkipped);

    PRINT_STAT(OS, delim, C, "Driver", DriverDepCascadingTopLevel);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepCascadingDynamic);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepCascadingNominal);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepCascadingMember);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepCascadingExternal);

    PRINT_STAT(OS, delim, C, "Driver", DriverDepTopLevel);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepDynamic);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepNominal);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepMember);
    PRINT_STAT(OS, delim, C, "Driver", DriverDepExternal);
  }
  // Print timers.
  TimerGroup::printAllJSONValues(OS, delim);
  OS << "\n}\n";
  OS.flush();
}

UnifiedStatsReporter::~UnifiedStatsReporter()
{
  // NB: Timer needs to be Optional<> because it needs to be destructed early;
  // LLVM will complain about double-stopping a timer if you tear down a
  // NamedRegionTimer after printing all timers. The printing routines were
  // designed with more of a global-scope, run-at-process-exit in mind, which
  // we're repurposing a bit here.
  Timer.reset();

  std::error_code EC;
  raw_fd_ostream ostream(Filename, EC, fs::F_Append | fs::F_Text);
  if (EC)
    return;

  // We change behaviour here depending on whether -DLLVM_ENABLE_STATS and/or
  // assertions were on in this build; this is somewhat subtle, but turning on
  // all stats for all of LLVM and clang is a bit more expensive and intrusive
  // than we want to be in release builds.
  //
  //  - If enabled: we copy all of our "always-on" local stats into LLVM's
  //    global statistics list, and ask LLVM to manage the printing of them.
  //
  //  - If disabled: we still have our "always-on" local stats to write, and
  //    LLVM's global _timers_ were still enabled (they're runtime-enabled, not
  //    compile-time) so we sequence printing our own stats and LLVM's timers
  //    manually.

#if !defined(NDEBUG) || defined(LLVM_ENABLE_STATS)
  publishAlwaysOnStatsToLLVM();
  PrintStatisticsJSON(ostream);
#else
  printAlwaysOnStatsAndTimers(ostream);
#endif
}

} // namespace swift
