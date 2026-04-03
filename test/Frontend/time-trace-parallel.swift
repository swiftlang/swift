// Test that time trace events are captured from parallel LLVM codegen threads.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -time-trace-granularity=0 \
// RUN:   -time-trace-path %t/trace.json \
// RUN:   -c %t/A.swift %t/B.swift -num-threads 2 -wmo \
// RUN:   -module-name TestModule -o %t/A.o -o %t/B.o

// RUN: %FileCheck %s < %t/trace.json

// The IRGen phase in performParallelIRGeneration should appear.
// CHECK-DAG: "IRGen"

// ObjectFileEmission events should appear from worker threads
// processing the LLVM modules in parallel.
// CHECK-DAG: "ObjectFileEmission"

// Verify that worker threads inherit time-trace-granularity from the parent.
// With an absurdly high granularity, all events shorter than ~1000s are
// filtered out. Worker threads must respect this — ObjectFileEmission from
// workers must NOT appear. Use 8 files and 4 threads to ensure workers
// participate in codegen.
// RUN: %target-swift-frontend -time-trace-granularity=999999999 \
// RUN:   -time-trace-path %t/trace-high-granularity.json \
// RUN:   -c %t/A.swift %t/B.swift %t/C.swift %t/D.swift \
// RUN:      %t/E.swift %t/F.swift %t/G.swift %t/H.swift \
// RUN:   -num-threads 4 -wmo \
// RUN:   -module-name TestModule \
// RUN:   -o %t/A.o -o %t/B.o -o %t/C.o -o %t/D.o \
// RUN:   -o %t/E.o -o %t/F.o -o %t/G.o -o %t/H.o
// RUN: %FileCheck --check-prefix=HIGHGRAN %s < %t/trace-high-granularity.json

// HIGHGRAN: "traceEvents"
// HIGHGRAN-NOT: "ObjectFileEmission"

//--- A.swift
public func greetA() -> String { "Hello from A" }

//--- B.swift
public func greetB() -> String { "Hello from B" }

//--- C.swift
public func greetC() -> String { "Hello from C" }

//--- D.swift
public func greetD() -> String { "Hello from D" }

//--- E.swift
public func greetE() -> String { "Hello from E" }

//--- F.swift
public func greetF() -> String { "Hello from F" }

//--- G.swift
public func greetG() -> String { "Hello from G" }

//--- H.swift
public func greetH() -> String { "Hello from H" }
