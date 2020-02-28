// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S/Inputs
// RUN: find %S -name "*.swift" > %t/Sources.resp
// RUN: cd %t
// RUN: not %target-swiftc_driver -no-color-diagnostics -typecheck -output-file-map %t/output.json -incremental -module-name main -verify-incremental-dependencies @%t/Sources.resp 2>&1 | %FileCheck %s

// N.B. We use CHECK-DAG so we aren't subject to ordering problems caused by the
// driver choosing to reschedule different frontend processes, and various
// terminal emulators choosing to buffer rerouted file descriptors differently.

// CHECK-DAG: unexpected cascading dependency: main.Subclass.init
// CHECK-DAG: unexpected cascading dependency: main.Subclass.deinit
// CHECK-DAG: unexpected provided entity: PublicProtocol
// CHECK-DAG: unexpected provided entity: BaseProtocol
// CHECK-DAG: unexpected provided entity: Base
// CHECK-DAG: unexpected provided entity: Subclass
// CHECK-DAG: unexpected cascading dependency: main.Base.init
// CHECK-DAG: unexpected dependency exists: main.BaseProtocol
