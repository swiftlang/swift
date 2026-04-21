// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -time-trace-granularity=0 \
// RUN:   -emit-time-trace-path %t/trace.json -emit-object -o %t/test.o %s
// RUN: %FileCheck %s < %t/trace.json

// CHECK-DAG: "traceEvents"
// CHECK-DAG: "ExecuteCompiler"
// CHECK-DAG: "perform-sema"
// CHECK-DAG: "SILGen"
// CHECK-DAG: "SILLowering"
// CHECK-DAG: "IRGen"
// CHECK-DAG: "LLVM pipeline"
// CHECK-DAG: "ObjectFileEmission"

// WMO mode with supplementary outputs — verify individual trace scopes.
// RUN: %target-swift-frontend -time-trace-granularity=0 \
// RUN:   -emit-time-trace-path %t/trace-wmo.json -wmo -emit-object \
// RUN:   -emit-tbd-path %t/test.tbd -tbd-install_name TestModule \
// RUN:   -emit-objc-header-path %t/test.h \
// RUN:   -module-name TestModule -o %t/test.o %s
// RUN: %FileCheck --check-prefix=WMO %s < %t/trace-wmo.json

// WMO-DAG: "EmitObjCHeader"
// WMO-DAG: "EmitTBD"
