// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/CUSTOM_DIR)

// RUN: %target-swift-frontend  -I %t/lib/swift -typecheck %s %S/Inputs/objc_send_collector_2.swift -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: cat %t/.SWIFT_FINE_DEPENDENCY_TRACE.json | %{python} -c 'import json, sys; json.dump(json.loads(sys.stdin.read()), sys.stdout, sort_keys=True, indent=2)' | %FileCheck %s

// RUN: SWIFT_COMPILER_FINE_GRAINED_TRACE_PATH=%t/given_trace.json %target-swift-frontend  -I %t/lib/swift -typecheck %s %S/Inputs/objc_send_collector_2.swift -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: cat %t/given_trace.json | %{python} -c 'import json, sys; json.dump(json.loads(sys.stdin.read()), sys.stdout, sort_keys=True, indent=2)' | %FileCheck %s

// RUN: SWIFT_COMPILER_FINE_GRAINED_TRACE_PATH=%t/given_trace_2.json %target-swift-frontend  -I %t/lib/swift -emit-module %s %S/Inputs/objc_send_collector_2.swift -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE -enable-library-evolution
// RUN: not ls %t/given_trace_2.json


// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "SkipEmittingFineModuleTrace:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - FooBar" >> %t/blocklist.yml

// RUN: SWIFT_COMPILER_FINE_GRAINED_TRACE_PATH=%t/given_trace_3.json %target-swift-frontend  -I %t/lib/swift -typecheck %s %S/Inputs/objc_send_collector_2.swift -module-name FooBar -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE -blocklist-file %t/blocklist.yml
// RUN: cat %t/given_trace_3.json | %{python} -c 'import json, sys; json.dump(json.loads(sys.stdin.read()), sys.stdout, sort_keys=True, indent=2)' | %FileCheck %s  --check-prefix=CHECK-BLOCKED

// RUN: SWIFT_COMPILER_FINE_GRAINED_TRACE_PATH=%t/given_trace_4.json %target-swift-frontend  -I %t/lib/swift -typecheck %s %S/Inputs/objc_send_collector_2.swift -module-name FooBar -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE -disable-fine-module-tracing
// RUN: not ls %t/given_trace_4.json

// REQUIRES: objc_interop

import Foo

public func testProperties(_ x: FooClassBase, _ y: FooProtocolBase) {
  _ = x.fooBaseInstanceFunc0()
  x.fooBaseInstanceFunc1(1.2)
  _ = FooClassBase.fooBaseClassFunc0()
  y.fooProtoFunc()
}

// CHECK-DAG: "instance_method": "-[FooClassBase fooBaseInstanceFunc0]"
// CHECK-DAG: "instance_method": "-[FooClassBase fooBaseInstanceFunc1:]"
// CHECK-DAG: "class_method": "+[FooClassBase fooBaseClassFunc0]"
// CHECK-DAG: "interface_type": "FooClassBase"
// CHECK-DAG: "protocol_type": "FooProtocolBase"
// CHECK-DAG: "declared_at": "SOURCE_DIR/test/IDE/Inputs/mock-sdk/Foo.framework/Headers/Foo.h
// CHECK-DAG: "referenced_at_file_id": 1
// CHECK-DAG: "referenced_at_file_id": 2
// CHECK-DAG: "file_id": 1,
// CHECK-DAG: "file_path": "SOURCE_DIR/test/IDE/objc_send_collector_1.swift"
// CHECK-DAG: "file_path": "SOURCE_DIR/test/IDE/Inputs/objc_send_collector_2.swift"
// CHECK-DAG: "swift-compiler-version":

// CHECK-BLOCKED-NOT: "FooClassBase"
