// RUN: %target-typecheck-verify-swift -dump-ast > %t.ast 2>&1
// RUN: %FileCheck %s < %t.ast

// Test overriding of protocol members.

// CHECK: protocol{{.*}}"P0"
protocol P0 {
  associatedtype A

  // expected-note@+1{{potential overridden instance method 'foo()' here}}
  func foo()

  // expected-note@+1{{attempt to override property here}}
  var prop: A { get }
}

// CHECK: protocol{{.*}}"P1"
protocol P1: P0 {
  // CHECK: associated_type_decl
  // CHECK-SAME: overridden=P0
  associatedtype A

  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P1{{.*}}override={{.*}}P0.foo
  func foo()

  // CHECK: var_decl
  // CHECK-SAME: "prop"
  // CHECK-SAME: override=override.(file).P0.prop
  var prop: A { get }
}

// CHECK: protocol{{.*}}"P2"
protocol P2 {
  associatedtype A

  func foo()

  var prop: A { get }
}

// CHECK: protocol{{.*}}"P3"
protocol P3: P1, P2 {
  // CHECK: associated_type_decl
  // CHECK-SAME: "A"
  // CHECK-SAME: override=override.(file).P1.A
  // CHECK-SAME: override.(file).P2.A
  associatedtype A

  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P3{{.*}}override={{.*}}P2.foo{{.*,.*}}P1.foo
  func foo()

  // CHECK: var_decl
  // CHECK-SAME: "prop"
  // CHECK-SAME: override=override.(file).P2.prop
  // CHECK-SAME: override.(file).P1.prop
  var prop: A { get }
}

// CHECK: protocol{{.*}}"P4"
protocol P4: P0 {
  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P4
  // CHECK-NOT: override=
  // CHECK-SAME: )
  func foo() -> Int

  // CHECK: var_decl
  // CHECK-SAME: "prop"
  // CHECK-NOT: override=
  // CHECK-SAME: immutable
  var prop: Int { get }
}

// CHECK: protocol{{.*}}"P5"
protocol P5: P0 where Self.A == Int {
  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P5
  // CHECK-NOT: override=
  // CHECK-SAME: )
  func foo() -> Int

  // CHECK: var_decl
  // CHECK-SAME: "prop"
  // CHECK-SAME: override=override.(file).P0.prop
  var prop: Int { get }
}

// Allow the 'override' keyword on protocol members (it is not required).
// CHECK: protocol{{.*}}"P6"
protocol P6: P0 {
  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P6{{.*}}override={{.*}}P0.foo
  override func foo()

  // CHECK: var_decl
  // CHECK-SAME: "prop"
  // CHECK-SAME: override=override.(file).P0.prop
  override var prop: A { get }
}

// Complain if 'override' is present but there is no overridden declaration.
protocol P7: P0 {
  // expected-error@+1{{method does not override any method from its superclass}}
  override func foo() -> Int

  // expected-error@+1{{property 'prop' with type 'Int' cannot override a property with type 'Self.A'}}
  override var prop: Int { get }
}

// Suppress overrides.
// CHECK: protocol{{.*}}"P8"
protocol P8: P0 {
  // CHECK: associated_type_decl
  // CHECK-SAME: "A"
  // CHECK-NOT: override
  // CHECK-SAME: )
  @_nonoverride
  associatedtype A

  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P8
  // CHECK-NOT: override=
  // CHECK-SAME: )
  @_nonoverride
  func foo()

  // CHECK: var_decl
  // CHECK-SAME: "prop"
  // CHECK-NOT: override=
  // CHECK-SAME: immutable
  @_nonoverride
  var prop: A { get }
}

class Base { }
class Derived : Base { }

// Protocol overrides require exact type matches.
protocol SubtypingP0 {
  init?()
  func foo() -> Base
}

// CHECK: protocol{{.*}}"SubtypingP1"
protocol SubtypingP1: SubtypingP0 {
  // CHECK: constructor_decl{{.*}}Self : SubtypingP1
  // CHECK-NOT: override=
  // CHECK-SAME: designated
  init()

  // CHECK: func_decl{{.*}}foo(){{.*}}Self : SubtypingP1
  // CHECK-NOT: override=
  // CHECK-SAME: )
  func foo() -> Derived
}

// CHECK: protocol{{.*}}"SubtypingP2"
protocol SubtypingP2: SubtypingP0 {
  // CHECK: constructor_decl{{.*}}Self : SubtypingP2
  // CHECK: override={{.*}}SubtypingP0.init
  // CHECK-SAME: designated
  init?()
}
