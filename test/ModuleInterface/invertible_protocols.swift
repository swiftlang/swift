// RUN: %target-swift-frontend %s \
// RUN:     -swift-version 5 \
// RUN:     -enable-library-evolution \
// RUN:     -emit-module -module-name Swift -parse-stdlib \
// RUN:     -o %t/Swift.swiftmodule \
// RUN:     -emit-module-interface-path %t/Swift.swiftinterface

// RUN: %FileCheck %s < %t/Swift.swiftinterface

// CHECK:      @_marker public protocol Escapable {
// CHECK-NEXT: }

// Compilers without the Deinitable protocol see neither it nor Copyable's
// inheritance from it.
// CHECK-NEXT: #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: @_marker public protocol Deinitable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: @_marker public protocol Copyable : Swift::Deinitable {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @_marker public protocol Copyable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// This test verifies that:
//   1. When omitted, the an invertible protocol decl gets automatically
//      synthesized into a module named Swift
//   2. These protocol decls do not specify inverses in their inheritance clause
//      when emitted into the interface file. Copyable inherits Deinitable.

@_marker public protocol Escapable { }
