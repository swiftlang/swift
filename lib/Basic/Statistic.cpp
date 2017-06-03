//===--- Statistic.cpp - Swift unified stats reporting --------------------===//
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

#include "swift/Basic/Statistic.h"
#include "swift/Driver/DependencyGraph.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Config/config.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"
#include <chrono>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

namespace swift {
using namespace llvm;
using namespace llvm::sys;

static size_t
getChildrenMaxResidentSetSize() {
#if defined(HAVE_GETRUSAGE)
  struct rusage RU;
  ::getrusage(RUSAGE_CHILDREN, &RU);
  return RU.ru_maxrss;
#else
  return 0;
#endif
}

static std::string
makeFileName(StringRef ProgramName,
             StringRef AuxName) {
  std::string tmp;
  raw_string_ostream stream(tmp);
  auto now = std::chrono::system_clock::now();
  stream << "stats"
         << "-" << now.time_since_epoch().count()
         << "-" << ProgramName
         << "-" << AuxName
         << "-" << Process::GetRandomNumber()
         << ".json";
  return stream.str();
}

// LLVM's statistics-reporting machinery is sensitive to filenames containing
// YAML-quote-requiring characters, which occur surprisingly often in the wild;
// we only need a recognizable and likely-unique name for a target here, not an
// exact filename, so we go with a crude approximation. Furthermore, to avoid
// parse ambiguities when "demangling" counters and filenames we exclude hyphens
// and slashes.
static std::string
cleanName(StringRef n) {
  std::string tmp;
  for (auto c : n) {
    if (('a' <= c && c <= 'z') ||
        ('A' <= c && c <= 'Z') ||
        ('0' <= c && c <= '9') ||
        (c == '.'))
      tmp += c;
    else
      tmp += '_';
  }
  return tmp;
}

static std::string
auxName(StringRef ModuleName,
        StringRef InputName,
        StringRef TripleName,
        StringRef OutputType,
        StringRef OptType) {
  return (cleanName(ModuleName)
          + "-" + cleanName(InputName)
          + "-" + cleanName(TripleName)
          + "-" + cleanName(OutputType)
          + "-" + cleanName(OptType));
}

UnifiedStatsReporter::UnifiedStatsReporter(StringRef ProgramName,
                                           StringRef ModuleName,
                                           StringRef InputName,
                                           StringRef TripleName,
                                           StringRef OutputType,
                                           StringRef OptType,
                                           StringRef Directory)
  : UnifiedStatsReporter(ProgramName,
                         auxName(ModuleName,
                                 InputName,
                                 TripleName,
                                 OutputType,
                                 OptType),
                         Directory)
{
}

UnifiedStatsReporter::UnifiedStatsReporter(StringRef ProgramName,
                                           StringRef AuxName,
                                           StringRef Directory)
  : Filename(Directory),
    Timer(make_unique<NamedRegionTimer>(AuxName,
                                        "Building Target",
                                        ProgramName, "Running Program"))
{
  path::append(Filename, makeFileName(ProgramName, AuxName));
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
  } while (0)

void
UnifiedStatsReporter::publishAlwaysOnStatsToLLVM() {
  if (FrontendCounters) {
    auto &C = getFrontendCounters();

    PUBLISH_STAT(C, "AST", NumSourceBuffers);
    PUBLISH_STAT(C, "AST", NumLinkLibraries);
    PUBLISH_STAT(C, "AST", NumLoadedModules);
    PUBLISH_STAT(C, "AST", NumImportedExternalDefinitions);
    PUBLISH_STAT(C, "AST", NumTotalClangImportedEntities);
    PUBLISH_STAT(C, "AST", NumASTBytesAllocated);
    PUBLISH_STAT(C, "AST", NumDependencies);
    PUBLISH_STAT(C, "AST", NumReferencedTopLevelNames);
    PUBLISH_STAT(C, "AST", NumReferencedDynamicNames);
    PUBLISH_STAT(C, "AST", NumReferencedMemberNames);
    PUBLISH_STAT(C, "AST", NumDecls);
    PUBLISH_STAT(C, "AST", NumLocalTypeDecls);
    PUBLISH_STAT(C, "AST", NumObjCMethods);
    PUBLISH_STAT(C, "AST", NumInfixOperators);
    PUBLISH_STAT(C, "AST", NumPostfixOperators);
    PUBLISH_STAT(C, "AST", NumPrefixOperators);
    PUBLISH_STAT(C, "AST", NumPrecedenceGroups);
    PUBLISH_STAT(C, "AST", NumUsedConformances);

    PUBLISH_STAT(C, "Sema", NumConformancesDeserialized);
    PUBLISH_STAT(C, "Sema", NumConstraintScopes);
    PUBLISH_STAT(C, "Sema", NumDeclsDeserialized);
    PUBLISH_STAT(C, "Sema", NumDeclsValidated);
    PUBLISH_STAT(C, "Sema", NumFunctionsTypechecked);
    PUBLISH_STAT(C, "Sema", NumGenericSignatureBuilders);
    PUBLISH_STAT(C, "Sema", NumLazyGenericEnvironments);
    PUBLISH_STAT(C, "Sema", NumLazyGenericEnvironmentsLoaded);
    PUBLISH_STAT(C, "Sema", NumLazyIterableDeclContexts);
    PUBLISH_STAT(C, "Sema", NumTypesDeserialized);
    PUBLISH_STAT(C, "Sema", NumTypesValidated);
    PUBLISH_STAT(C, "Sema", NumUnloadedLazyIterableDeclContexts);

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

    PUBLISH_STAT(C, "IRModule", NumIRGlobals);
    PUBLISH_STAT(C, "IRModule", NumIRFunctions);
    PUBLISH_STAT(C, "IRModule", NumIRAliases);
    PUBLISH_STAT(C, "IRModule", NumIRIFuncs);
    PUBLISH_STAT(C, "IRModule", NumIRNamedMetaData);
    PUBLISH_STAT(C, "IRModule", NumIRValueSymbols);
    PUBLISH_STAT(C, "IRModule", NumIRComdatSymbols);
    PUBLISH_STAT(C, "IRModule", NumIRBasicBlocks);
    PUBLISH_STAT(C, "IRModule", NumIRInsts);

    PUBLISH_STAT(C, "LLVM", NumLLVMBytesOutput);
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
    PUBLISH_STAT(C, "Driver", ChildrenMaxRSS);
  }
}

#define PRINT_STAT(OS,DELIM,C,TY,NAME)                   \
  do {                                                   \
    OS << DELIM << "\t\"" TY "." #NAME "\": " << C.NAME; \
    delim = ",\n";                                       \
  } while (0)

void
UnifiedStatsReporter::printAlwaysOnStatsAndTimers(raw_ostream &OS) {
  // Adapted from llvm::PrintStatisticsJSON
  OS << "{\n";
  const char *delim = "";
  if (FrontendCounters) {
    auto &C = getFrontendCounters();

    PRINT_STAT(OS, delim, C, "AST", NumSourceBuffers);
    PRINT_STAT(OS, delim, C, "AST", NumLinkLibraries);
    PRINT_STAT(OS, delim, C, "AST", NumLoadedModules);
    PRINT_STAT(OS, delim, C, "AST", NumImportedExternalDefinitions);
    PRINT_STAT(OS, delim, C, "AST", NumTotalClangImportedEntities);
    PRINT_STAT(OS, delim, C, "AST", NumASTBytesAllocated);
    PRINT_STAT(OS, delim, C, "AST", NumDependencies);
    PRINT_STAT(OS, delim, C, "AST", NumReferencedTopLevelNames);
    PRINT_STAT(OS, delim, C, "AST", NumReferencedDynamicNames);
    PRINT_STAT(OS, delim, C, "AST", NumReferencedMemberNames);
    PRINT_STAT(OS, delim, C, "AST", NumDecls);
    PRINT_STAT(OS, delim, C, "AST", NumLocalTypeDecls);
    PRINT_STAT(OS, delim, C, "AST", NumObjCMethods);
    PRINT_STAT(OS, delim, C, "AST", NumInfixOperators);
    PRINT_STAT(OS, delim, C, "AST", NumPostfixOperators);
    PRINT_STAT(OS, delim, C, "AST", NumPrefixOperators);
    PRINT_STAT(OS, delim, C, "AST", NumPrecedenceGroups);
    PRINT_STAT(OS, delim, C, "AST", NumUsedConformances);

    PRINT_STAT(OS, delim, C, "Sema", NumConformancesDeserialized);
    PRINT_STAT(OS, delim, C, "Sema", NumConstraintScopes);
    PRINT_STAT(OS, delim, C, "Sema", NumDeclsDeserialized);
    PRINT_STAT(OS, delim, C, "Sema", NumDeclsValidated);
    PRINT_STAT(OS, delim, C, "Sema", NumFunctionsTypechecked);
    PRINT_STAT(OS, delim, C, "Sema", NumGenericSignatureBuilders);
    PRINT_STAT(OS, delim, C, "Sema", NumLazyGenericEnvironments);
    PRINT_STAT(OS, delim, C, "Sema", NumLazyGenericEnvironmentsLoaded);
    PRINT_STAT(OS, delim, C, "Sema", NumLazyIterableDeclContexts);
    PRINT_STAT(OS, delim, C, "Sema", NumTypesDeserialized);
    PRINT_STAT(OS, delim, C, "Sema", NumTypesValidated);
    PRINT_STAT(OS, delim, C, "Sema", NumUnloadedLazyIterableDeclContexts);

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

    PRINT_STAT(OS, delim, C, "IRModule", NumIRGlobals);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRFunctions);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRAliases);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRIFuncs);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRNamedMetaData);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRValueSymbols);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRComdatSymbols);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRBasicBlocks);
    PRINT_STAT(OS, delim, C, "IRModule", NumIRInsts);

    PRINT_STAT(OS, delim, C, "LLVM", NumLLVMBytesOutput);
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
    PRINT_STAT(OS, delim, C, "Driver", ChildrenMaxRSS);
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

  if (DriverCounters) {
    auto &C = getDriverCounters();
    C.ChildrenMaxRSS = getChildrenMaxResidentSetSize();
  }

  std::error_code EC;
  raw_fd_ostream ostream(Filename, EC, fs::F_Append | fs::F_Text);
  if (EC)
    return;

  // We change behavior here depending on whether -DLLVM_ENABLE_STATS and/or
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
