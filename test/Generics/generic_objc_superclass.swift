// RUN: %target-typecheck-verify-swift %clang-importer-sdk -dump-requirement-machine 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class Generic<T> : NSObject {}

func foo<T : Generic<U>, U>(_: T, _: U) {
  _ = T.self
  _ = U.self
}

// CHECK: Requirement machine for fresh signature < T U >
// CHECK-NEXT: Rewrite system: {
// CHECK-NEXT: - [Copyable].[Copyable] => [Copyable] [permanent]
// CHECK-NEXT: - [Escapable].[Escapable] => [Escapable] [permanent]
// CHECK-NEXT: - τ_0_1.[Copyable] => τ_0_1 [explicit]
// CHECK-NEXT: - τ_0_1.[Escapable] => τ_0_1 [explicit]
// CHECK-NEXT: - τ_0_0.[superclass: Generic<τ_0_1>] => τ_0_0
// CHECK-NEXT: - τ_0_0.[layout: AnyObject] => τ_0_0
// CHECK-NEXT: }
// CHECK: Property map: {
// CHECK-NEXT:   [Copyable] => { conforms_to: [Copyable] }
// CHECK-NEXT:   [Escapable] => { conforms_to: [Escapable] }
// CHECK-NEXT:   τ_0_1 => { conforms_to: [Copyable Escapable] }
// CHECK-NEXT:   τ_0_0 => { layout: AnyObject superclass: [superclass: Generic<τ_0_1>] }
// CHECK-NEXT: }
