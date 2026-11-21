// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s \
// RUN:     -swift-version 5 \
// RUN:     -enable-library-evolution \
// RUN:     -emit-module -module-name Swift -parse-stdlib \
// RUN:     -o %t/Swift.swiftmodule \
// RUN:     -emit-module-interface-path %t/Swift.swiftinterface
// RUN: %FileCheck %s < %t/Swift.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Swift.swiftinterface) -module-name Swift -parse-stdlib

// The stdlib adopts Transferable without NontransferableTypes, so compilers
// that predate the Transferable protocol get the declarations they know.

// CHECK:      #if compiler(>=5.3) && $TransferableProtocol
// CHECK-NEXT: @_marker public protocol SendableMetatype : ~Copyable, ~Escapable, ~Deinitable, ~Transferable {
// CHECK-NEXT: }
// CHECK-NEXT: #elseif compiler(>=5.3) && $DeinitableProtocol
// CHECK-NEXT: @_marker public protocol SendableMetatype : ~Copyable, ~Escapable, ~Deinitable {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @_marker public protocol SendableMetatype : ~Copyable, ~Escapable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@_marker public protocol SendableMetatype: ~Copyable, ~Escapable, ~Deinitable, ~Transferable { }

// CHECK-NEXT: #if compiler(>=5.3) && $TransferableProtocol
// CHECK-NEXT: @_marker public protocol Sendable : Swift::SendableMetatype, Swift::Transferable {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @_marker public protocol Sendable : Swift::SendableMetatype {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@_marker public protocol Sendable: SendableMetatype, Transferable, ~Copyable, ~Escapable, ~Deinitable { }

// Declared after their uses, as in the stdlib.

@_marker public protocol Copyable: Deinitable/*, ~Escapable*/ {}
@_marker public protocol Escapable/*: ~Copyable*/ {}
@_marker public protocol Deinitable/*: ~Copyable, ~Escapable*/ {}

// CHECK:      #if compiler(>=5.3) && $TransferableProtocol
// CHECK-NEXT: @_marker public protocol Transferable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@_marker public protocol Transferable/*: ~Copyable, ~Escapable, ~Deinitable*/ {}
