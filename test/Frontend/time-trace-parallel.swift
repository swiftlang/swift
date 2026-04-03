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

//--- A.swift
public func greetA() -> String {
  return "Hello from A"
}

//--- B.swift
public func greetB() -> String {
  return "Hello from B"
}
