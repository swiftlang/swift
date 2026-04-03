// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -time-trace-granularity=0 \
// RUN:   -time-trace-path %t/trace.json -emit-object -o %t/test.o %s
// RUN: %FileCheck %s < %t/trace.json

// CHECK-DAG: "traceEvents"
// CHECK-DAG: "ExecuteCompiler"
// CHECK-DAG: "perform-sema"
// CHECK-DAG: "SILGen"
// CHECK-DAG: "SILLowering"
// CHECK-DAG: "IRGen"
// CHECK-DAG: "LLVM pipeline"
// CHECK-DAG: "ObjectFileEmission"
// CHECK-DAG: "EmitObjCHeader"
