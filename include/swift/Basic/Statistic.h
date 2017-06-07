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
    size_t NumDriverJobsRun;
    size_t NumDriverJobsSkipped;

    size_t DriverDepCascadingTopLevel;
    size_t DriverDepCascadingDynamic;
    size_t DriverDepCascadingNominal;
    size_t DriverDepCascadingMember;
    size_t DriverDepCascadingExternal;

    size_t DriverDepTopLevel;
    size_t DriverDepDynamic;
    size_t DriverDepNominal;
    size_t DriverDepMember;
    size_t DriverDepExternal;

    size_t ChildrenMaxRSS;
  };

  struct AlwaysOnFrontendCounters
  {
    size_t NumSourceBuffers;
    size_t NumLinkLibraries;
    size_t NumLoadedModules;
    size_t NumImportedExternalDefinitions;
    size_t NumTotalClangImportedEntities;
    size_t NumASTBytesAllocated;
    size_t NumDependencies;
    size_t NumReferencedTopLevelNames;
    size_t NumReferencedDynamicNames;
    size_t NumReferencedMemberNames;
    size_t NumDecls;
    size_t NumLocalTypeDecls;
    size_t NumObjCMethods;
    size_t NumInfixOperators;
    size_t NumPostfixOperators;
    size_t NumPrefixOperators;
    size_t NumPrecedenceGroups;
    size_t NumUsedConformances;

    size_t NumConformancesDeserialized;
    size_t NumConstraintScopes;
    size_t NumDeclsDeserialized;
    size_t NumDeclsValidated;
    size_t NumFunctionsTypechecked;
    size_t NumGenericSignatureBuilders;
    size_t NumLazyGenericEnvironments;
    size_t NumLazyGenericEnvironmentsLoaded;
    size_t NumLazyIterableDeclContexts;
    size_t NumTypesDeserialized;
    size_t NumTypesValidated;
    size_t NumUnloadedLazyIterableDeclContexts;

    size_t NumSILGenFunctions;
    size_t NumSILGenVtables;
    size_t NumSILGenWitnessTables;
    size_t NumSILGenDefaultWitnessTables;
    size_t NumSILGenGlobalVariables;

    size_t NumSILOptFunctions;
    size_t NumSILOptVtables;
    size_t NumSILOptWitnessTables;
    size_t NumSILOptDefaultWitnessTables;
    size_t NumSILOptGlobalVariables;

    size_t NumIRGlobals;
    size_t NumIRFunctions;
    size_t NumIRAliases;
    size_t NumIRIFuncs;
    size_t NumIRNamedMetaData;
    size_t NumIRValueSymbols;
    size_t NumIRComdatSymbols;
    size_t NumIRBasicBlocks;
    size_t NumIRInsts;

    size_t NumLLVMBytesOutput;
  };

private:
  SmallString<128> Filename;
  std::unique_ptr<llvm::NamedRegionTimer> Timer;

  std::unique_ptr<AlwaysOnDriverCounters> DriverCounters;
  std::unique_ptr<AlwaysOnFrontendCounters> FrontendCounters;

  void publishAlwaysOnStatsToLLVM();
  void printAlwaysOnStatsAndTimers(llvm::raw_ostream &OS);

  UnifiedStatsReporter(StringRef ProgramName,
                       StringRef AuxName,
                       StringRef Directory);
public:
  UnifiedStatsReporter(StringRef ProgramName,
                       StringRef ModuleName,
                       StringRef InputName,
                       StringRef TripleName,
                       StringRef OutputType,
                       StringRef OptType,
                       StringRef Directory);
  ~UnifiedStatsReporter();

  AlwaysOnDriverCounters &getDriverCounters();
  AlwaysOnFrontendCounters &getFrontendCounters();
};

}
#endif // SWIFT_BASIC_STATISTIC_H
