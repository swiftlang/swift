// REQUIRES: executable_test
// REQUIRES: swift_tools_extra

// Test that a module-wrapped Swift AST section can be parsed.

// RUN: %empty-directory(%t)

// RUN: echo "public let a0 = 0"  >%t/a0.swift
// RUN: %target-build-swift %t/a0.swift -emit-module -emit-module-path %t/a0.swiftmodule -I %s/Inputs
// RUN: %target-swift-modulewrap %t/a0.swiftmodule -o %t/a0-mod.o

// RUN: %lldb-moduleimport-test -verbose %t/a0-mod.o | %FileCheck %s
// CHECK: Path: {{.*}}/Inputs, framework=false, system=false
// CHECK: Importing a0...
// CHECK: Import successful!
