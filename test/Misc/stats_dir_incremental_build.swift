// Verify -incremental-build-stats: the stats file carries an "incremental" tag
// in its name, and holds only the counters that IncrementalBuildStatistics.def
// lists, the timers of tracers created as incremental build events, and the
// timer for the whole job.

// --- With the flag, only the statistics in the set. -print-zero-stats makes
// --- the writer print every counter in the set, including the zero ones.
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -primary-file %s -o %t/out.o -stats-output-dir %t -incremental-build-stats -print-zero-stats
// RUN: ls %t | %FileCheck --check-prefix=NAMES --implicit-check-not=clang-module-memory %s
// RUN: cat %t/stats-*-incremental.json | %FileCheck --check-prefix=KEYS %s
// RUN: cat %t/stats-*-incremental.json | %FileCheck --check-prefix=OTHER-KEYS %s

// NAMES: stats-{{[0-9]+}}-swift-frontend-{{.*}}-o-Onone-{{[0-9]+}}-incremental.json

// Statistics outside the set. With no positive check under this prefix, each
// of these lines checks the whole file.
// OTHER-KEYS-NOT: "ClangImporter.
// OTHER-KEYS-NOT: "Frontend.NumInstructionsExecuted"
// OTHER-KEYS-NOT: "Frontend.MaxMallocUsage"
// OTHER-KEYS-NOT: "time.swift.load-stdlib.
// OTHER-KEYS-NOT: "time.swift.SIL verification

// KEYS-DAG: "Frontend.WallClockMicroseconds":
// KEYS-DAG: "Parse.NumFunctionsParsed":
// KEYS-DAG: "Parse.NumIterableDeclContextParsed":
// KEYS-DAG: "Sema.NumDeclsTypechecked":
// KEYS-DAG: "Sema.NumDeclsDeserialized":
// KEYS-DAG: "Sema.NumConstraintScopes":
// KEYS-DAG: "Sema.NumSolverBytesAllocated":
// KEYS-DAG: "AST.NumReferencedTopLevelNames": {{[1-9]}}
// KEYS-DAG: "SILModule.NumSILGenFunctions":
// KEYS-DAG: "SILModule.NumSILGenWitnessTables":
// KEYS-DAG: "SILModule.NumSILGenVtables":
// KEYS-DAG: "SILModule.NumSILOptFunctions":
// KEYS-DAG: "IRModule.NumIRFunctions":
// KEYS-DAG: "IRModule.NumIRBasicBlocks":
// KEYS-DAG: "IRModule.NumIRInsts":
// KEYS-DAG: "LLVM.NumLLVMBytesOutput":
// KEYS-DAG: "time.swift.parse-and-resolve-imports.wall":
// KEYS-DAG: "time.swift.perform-sema.wall":
// KEYS-DAG: "time.swift.SILGen.wall":
// KEYS-DAG: "time.swift.SIL-processing.wall":
// KEYS-DAG: "time.swift.IRGen.wall":
// KEYS-DAG: "time.swift.SIL-and-LLVM-processing.wall":

// --- Without -stats-output-dir, the frontend accepts the flag.
// RUN: %target-swift-frontend -typecheck -incremental-build-stats %s

// --- Without the flag, the stats file has no tag, holds every statistic, and
// --- comes with a Clang memory file.
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -o %t/out.o -stats-output-dir %t %s
// RUN: ls %t | %FileCheck --check-prefix=FULL-NAMES --implicit-check-not=-incremental.json %s
// RUN: cat %t/stats-*.json | %FileCheck --check-prefix=FULL-KEYS %s

// FULL-NAMES: clang-module-memory-
// FULL-NAMES: stats-{{[0-9]+}}-swift-frontend-{{.*}}-o-Onone-{{[0-9]+}}.json

// FULL-KEYS: "AST.NumSourceLines":

func f(_ x: Int) -> Int {
  return x + 1
}
