// Test to verify disjointedness of dependency- and source-range-dependency graphs.
// If they are not disjoint, then an external dependency on a file that would be
// compiled anyway by dependencies gets marked, and then source-ranges fails
// to pick up the external dependency, because it's already marked.



// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/disjoint/* %t

// =============================================================================
// Create the supplementary outputs & build record
// =============================================================================

// RUN: cd %t && %swiftc_driver -driver-compare-incremental-schemes -enable-source-range-dependencies -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental  >& %t/output1

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-1 %s < %t/output1
// CHECK-1: Adding non-batch job to task queue: {compile: main.o <= main.swift}
// CHECK-1: Adding non-batch job to task queue: {compile: fileA.o <= fileA.swift}
// CHECK-1: Adding non-batch job to task queue: {compile: fileB.o <= fileB.swift}
// CHECK-1: Adding non-batch job to task queue: {link: main <= main.o fileA.o fileB.o}
// CHECK-1: Added to TaskQueue: {link: main <= main.o fileA.o fileB.o}
// CHECK-1: *** Incremental build disabled because could not read build record, cannot compare ***


// =============================================================================
// Modify fileB.swift; dependency-scheme will compile fileA.swift,
// source ranges won't.
// =============================================================================

// RUN: cp %S/Inputs/disjoint/fileA.swiftdeps %t
// RUN: cp %S/Inputs/disjoint/fileB2.swift %t/fileB.swift

// RUN: cd %t && %swiftc_driver -driver-compare-incremental-schemes -enable-source-range-dependencies -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental  >& %t/output2
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-2 %s < %t/output2
// CHECK-2: Hypothetically: Skipping <Dependencies> file is up-to-date and output exists: {compile: main.o <= main.swift}
// CHECK-2: Hypothetically: Skipping <Dependencies> file is up-to-date and output exists: {compile: fileA.o <= fileA.swift}
// CHECK-2: Hypothetically: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-2: Skipping <Ranges> nothing that this file parsed changed in any other file: {compile: main.o <= main.swift}
// CHECK-2: Skipping <Ranges> nothing that this file parsed changed in any other file: {compile: fileA.o <= fileA.swift}
// CHECK-2: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}
// CHECK-2: Using ranges
// CHECK-2: After completion of {compile: fileB.o <= fileB.swift}:
// CHECK-2: - Dependencies would now schedule: {compile: fileA.o <= fileA.swift}
// CHECK-2: *** Range benefit: 1 compilations, 1 stages, deps: 2, ranges: 1, total: 3, requested: ranges, used: ranges ***

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-2A %s < %t/output2
// CHECK-2A-NOT: Queuing <Ranges> {{.*}}{compile: fileA.o <= fileA.swift}


// =============================================================================
// Modify fileB.swift again, and external dependency.
// dependency-scheme will compile fileA.swift,
// source ranges should, too.
// =============================================================================

// RUN: cp %S/Inputs/disjoint/{fileA.swiftdeps,imported.swiftmodule} %t
// RUN: cp %S/Inputs/disjoint/fileB3.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -driver-compare-incremental-schemes -enable-source-range-dependencies -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental  >& %t/output3

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-3 %s < %t/output3
// CHECK-3: Hypothetically: Skipping <Dependencies> file is up-to-date and output exists: {compile: main.o <= main.swift}
// CHECK-3: Hypothetically: Skipping <Dependencies> file is up-to-date and output exists: {compile: fileA.o <= fileA.swift}
// CHECK-3: Hypothetically: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-3: Hypothetically: Queuing <Dependencies> because of external dependencies: {compile: fileA.o <= fileA.swift}
// CHECK-3: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}
// CHECK-3: Queuing <Ranges> because of external dependencies: {compile: fileA.o <= fileA.swift}
// CHECK-3: *** Range benefit: 0 compilations, 0 stages, deps: 2, ranges: 2, total: 3, requested: ranges, used: ranges ***

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-3A %s < %t/output3
// CHECK-3A-NOT: Queuing <Ranges> (changed: fileB.swift:{{.*}}): {compile: fileA.o <= fileA.swift}
