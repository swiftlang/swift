// SILModules contribute to -trace-stats-event output.
// The lifetime of SILModules are shorter than required to be captured by the
// UnifiedStatesReporter's destructor's flushTracesAndProfiles. SILModules need
// to be otherwise retained to contribute trace events when the one-time queue
// flush happens. These tests cover the possibility of mismanaging these
// lifetimes while tracing

// RUN: %empty-directory(%t)

// RUN: %empty-directory(%t/emit-module)
// RUN: env MallocScribble=1 %target-swift-frontend -emit-module -primary-file %s -module-name main -o %t/main.swiftmodule -stats-output-dir %t/emit-module -trace-stats-events
// RUN: %FileCheck -input-file %t/emit-module/trace-*.csv %s

// Compile one file: ensure the SILModule is logged into the trace files before
// it is freed. Ensure IRGen is traced too.
// RUN: %empty-directory(%t/primary)
// RUN: env MallocScribble=1 %target-swift-frontend -emit-object -primary-file %s -module-name main -o %t/primary.o -stats-output-dir %t/primary -trace-stats-events
// RUN: %FileCheck -input-file %t/primary/trace-*.csv %s
// RUN: %FileCheck -check-prefix=IRGEN -input-file %t/primary/trace-*.csv %s

// RUN: %empty-directory(%t/wmo)
// RUN: env MallocScribble=1 %target-swift-frontend -emit-object %s -module-name main -num-threads 2 -o %t/wmo.o -stats-output-dir %t/wmo -trace-stats-events
// RUN: %FileCheck -input-file %t/wmo/trace-*.csv %s
// RUN: %FileCheck -check-prefix=IRGEN -input-file %t/wmo/trace-*.csv %s

// CHECK: "SILGen-function",{{.*}},"@$s4main6answerSiyF"
// IRGEN: "exit","IRGen",

public func answer() -> Int { 42 }
