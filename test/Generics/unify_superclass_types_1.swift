// RUN: %target-typecheck-verify-swift -dump-requirement-machine 2>&1 | %FileCheck %s

class Base {}
class Derived : Base {
  func derivedMethod() {}
}

protocol P : Base {}

func takesDerived(_: Derived) {}

extension P where Self : Derived {
  func passesDerived() { derivedMethod() }
}

// CHECK-LABEL: Requirement machine for fresh signature < Self >
// CHECK-NEXT: Rewrite system: {
// CHECK-NEXT: - [P].[P] => [P] [permanent]
// CHECK-NEXT: - [P].[superclass: Base] => [P]
// CHECK-NEXT: - [P].[layout: _NativeClass] => [P]
// CHECK-NEXT: - [Copyable].[Copyable] => [Copyable] [permanent]
// CHECK-NEXT: - [Escapable].[Escapable] => [Escapable] [permanent]
// CHECK-NEXT: - τ_0_0.[P] => τ_0_0
// CHECK-NEXT: - τ_0_0.[superclass: Derived] => τ_0_0
// CHECK-NEXT: - τ_0_0.[superclass: Base] => τ_0_0
// CHECK-NEXT: - τ_0_0.[layout: _NativeClass] => τ_0_0
// CHECK-NEXT: }
// CHECK: Property map: {
// CHECK-NEXT:   [P] => { conforms_to: [P] layout: _NativeClass superclass: [superclass: Base] }
// CHECK-NEXT:   [Copyable] => { conforms_to: [Copyable] }
// CHECK-NEXT:   [Escapable] => { conforms_to: [Escapable] }
// CHECK-NEXT:   τ_0_0 => { conforms_to: [P] layout: _NativeClass superclass: [superclass: Derived] }
// CHECK-NEXT: }
