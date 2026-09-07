// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P {
  static var foo: Int { get }
  static var counter: Int { get set }
}

struct S {
  init(_ kp: KeyPath<P.Type, Int>) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s25keypath_protocol_metatype4testyyF
func test() {
  // CHECK: keypath $KeyPath<any P.Type, Int>, (root $any P.Type; gettable_property $Int, id #P.foo!getter{{.*}}getter @$s25keypath_protocol_metatype1PP3fooSivpZAaB_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any P.Type) -> @out Int)
  S(\.foo)
}

// The getter thunk opens the existential metatype and dispatches through the
// witness table of the dynamic type.
// CHECK-LABEL: sil shared [thunk] [ossa] @$s25keypath_protocol_metatype1PP3fooSivpZAaB_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any P.Type) -> @out Int
// CHECK: [[OPENED:%.*]] = open_existential_metatype %{{.*}} to $@thick (@opened({{.*}}, any P) Self).Type
// CHECK: witness_method $@opened({{.*}}, any P) Self, #P.foo!getter{{.*}}, [[OPENED]]

// CHECK-LABEL: sil hidden [ossa] @$s25keypath_protocol_metatype12testSettableyyF
func testSettable() {
  // CHECK: keypath $ReferenceWritableKeyPath<any P.Type, Int>, (root $any P.Type; settable_property $Int, id #P.counter!getter{{.*}}getter @$s25keypath_protocol_metatype1PP7counterSivpZAaB_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any P.Type) -> @out Int, setter @$s25keypath_protocol_metatype1PP7counterSivpZAaB_pXpTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed @thick any P.Type) -> ())
  let _: ReferenceWritableKeyPath<any P.Type, Int> = \.counter
}

// CHECK-LABEL: sil shared [thunk] [ossa] @$s25keypath_protocol_metatype1PP7counterSivpZAaB_pXpTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed @thick any P.Type) -> ()
// CHECK: [[OPENED:%.*]] = open_existential_metatype %{{.*}} to $@thick (@opened({{.*}}, any P) Self).Type
// CHECK: witness_method $@opened({{.*}}, any P) Self, #P.counter!setter{{.*}}, [[OPENED]]

class Base {
  class var overridable: Int { 1 }
}
protocol Derived: Base {
  static var k: Int { get }
}

// CHECK-LABEL: sil hidden [ossa] @$s25keypath_protocol_metatype20testClassConstrainedyyF
func testClassConstrained() {
  // CHECK: keypath $KeyPath<any Derived.Type, Int>, (root $any Derived.Type; gettable_property $Int, id #Derived.k!getter{{.*}}getter @$s25keypath_protocol_metatype7DerivedP1kSivpZAaB_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any Derived.Type) -> @out Int)
  let _: KeyPath<any Derived.Type, Int> = \.k
  // CHECK: keypath $KeyPath<any Derived.Type, Int>, (root $any Derived.Type; gettable_property $Int, id #Base.overridable!getter{{.*}}getter @$s25keypath_protocol_metatype4BaseC11overridableSivpZAA7Derived_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any Derived.Type) -> @out Int)
  let _: KeyPath<any Derived.Type, Int> = \.overridable
}

// CHECK-LABEL: sil shared [thunk] [ossa] @$s25keypath_protocol_metatype7DerivedP1kSivpZAaB_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any Derived.Type) -> @out Int
// CHECK: open_existential_metatype
// CHECK: witness_method $@opened({{.*}}, any Derived) Self, #Derived.k!getter

// A member of the superclass bound is reached by upcasting the opened metatype.
// CHECK-LABEL: sil shared [thunk] [ossa] @$s25keypath_protocol_metatype4BaseC11overridableSivpZAA7Derived_pXpTK : $@convention(keypath_accessor_getter) (@in_guaranteed @thick any Derived.Type) -> @out Int
// CHECK: [[OPENED:%.*]] = open_existential_metatype %{{.*}} to $@thick (@opened({{.*}}, any Derived) Self).Type
// CHECK: [[CLASS:%.*]] = upcast [[OPENED]] to $@thick Base.Type
// CHECK: class_method [[CLASS]], #Base.overridable!getter
