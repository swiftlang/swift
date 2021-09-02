// RUN: %target-typecheck-verify-swift %clang-importer-sdk -requirement-machine=verify -dump-requirement-machine 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class Generic<T> : NSObject {}

func foo<T : Generic<U>, U>(_: T, _: U) {
  _ = T.self
  _ = U.self
}

// CHECK-LABEL: Requirement machine for <τ_0_0, τ_0_1 where τ_0_0 : Generic<τ_0_1>>
// CHECK-NEXT: Rewrite system: {
// CHECK-NEXT: - τ_0_0.[superclass: Generic<τ_0_0> with <τ_0_1>] => τ_0_0
// CHECK-NEXT: - τ_0_0.[layout: AnyObject] => τ_0_0
// CHECK-NEXT: }
// CHECK-NEXT: Property map: {
// CHECK-NEXT:   τ_0_0 => { layout: AnyObject superclass: [superclass: Generic<τ_0_0> with <τ_0_1>] }
// CHECK-NEXT: }
