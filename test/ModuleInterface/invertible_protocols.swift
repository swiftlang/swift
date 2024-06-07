// RUN: %target-swift-frontend %s \
// RUN:     -swift-version 5 \
// RUN:     -enable-library-evolution \
// RUN:     -emit-module -module-name Swift -parse-stdlib \
// RUN:     -o %t/Swift.swiftmodule \
// RUN:     -emit-module-interface-path %t/Swift.swiftinterface

// RUN: %FileCheck %s < %t/Swift.swiftinterface

// CHECK-DAG: @_marker public protocol Copyable {
// CHECK-DAG: @_marker public protocol Escapable {

// This test verifies that:
//   1. When omitted, the an invertible protocol decl gets automatically
//      synthesized into a module named Swift
//   2. These protocol decls do not specify inverses in their inheritance clause
//      when emitted into the interface file.

@_marker public protocol Escapable { }
