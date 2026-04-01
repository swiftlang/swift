// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -time-trace -time-trace-granularity=0 \
// RUN:   -time-trace-path %t/trace.json -emit-object -o %t/test.o %s
// RUN: %FileCheck %s < %t/trace.json

// CHECK-DAG: "traceEvents"
// CHECK-DAG: "ExecuteCompiler"
// CHECK-DAG: "SemanticAnalysis"
// CHECK-DAG: "SILLowering"
// CHECK-DAG: "ObjectFileEmission"
// CHECK-DAG: "EmitObjCHeader"
