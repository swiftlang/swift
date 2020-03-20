// REQUIRES: OS=windows-msvc
// This test is special-cased on Windows because dependencies in Two.swift don't
// appear to be registered against the tracker. rdar://60030114

// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S/Inputs -r %t.resp
// RUN: cd %t
// RUN: not %target-swiftc_driver -no-color-diagnostics -typecheck -output-file-map %t/output.json -incremental -module-name main -verify-incremental-dependencies @%t.resp 2>&1 | sort | %FileCheck %s

// CHECK: unexpected cascading member dependency: main.Base.init
// CHECK: unexpected provided entity: Base
// CHECK: unexpected provided entity: BaseProtocol
