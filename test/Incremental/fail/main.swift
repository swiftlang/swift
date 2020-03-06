// REQUIRES: rdar60030114

// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S/Inputs -r %t.resp
// RUN: cd %t
// RUN: not %target-swiftc_driver -no-color-diagnostics -typecheck -output-file-map %t/output.json -incremental -module-name main -verify-incremental-dependencies @%t.resp 2>&1 | sort | %FileCheck %s

// CHECK: unexpected dependency exists: main.BaseProtocol
// CHECK: unexpected cascading dependency: main.Base.init
// CHECK: unexpected cascading dependency: main.Subclass.deinit
// CHECK: unexpected cascading dependency: main.Subclass.init
// CHECK: unexpected cascading potential member dependency: main.Base
// CHECK: unexpected provided entity: Base
// CHECK: unexpected provided entity: BaseProtocol
// CHECK: unexpected provided entity: PublicProtocol
// CHECK: unexpected provided entity: Subclass
