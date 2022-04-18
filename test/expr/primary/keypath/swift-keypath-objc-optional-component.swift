// RUN: %target-typecheck-verify-swift -disable-objc-attr-requires-foundation-module -enable-objc-interop

@objc class Object {
  var name: String

  init(name: String) {
    self.name = name
  }
}

@objc protocol P {
  @objc optional var object: Object { get set }

  @objc optional subscript(_: Int) -> Object { get set }
}

func assertExactType<T>(of _: T, is _: T.Type) {}

// An optional storage component makes the key path read-only...
do {
  let kp_property = \P.object
  let kp_subscript = \P.[0]

  var p: P
  // expected-error@+1 {{cannot assign through subscript: 'kp_property' is a read-only key path}}
  p[keyPath: kp_property] = Object(name: "nope")
  // expected-error@+1 {{cannot assign through subscript: 'kp_subscript' is a read-only key path}}
  p[keyPath: kp_subscript] = Object(name: "nope")

  assertExactType(of: kp_property, is: KeyPath<P, Object?>.self)
  assertExactType(of: kp_subscript, is: KeyPath<P, Object?>.self)
}

// ...unless a reference-writable component shows up later.
do {
  let kp_propertyForce_name = \P.object!.name
  let kp_subscriptForce_name = \P.[0]!.name

  let p: P
  p[keyPath: kp_propertyForce_name] = "yes"
  p[keyPath: kp_subscriptForce_name] = "yes"

  assertExactType(of: kp_propertyForce_name, is: ReferenceWritableKeyPath<P, String>.self)
  assertExactType(of: kp_subscriptForce_name, is: ReferenceWritableKeyPath<P, String>.self)
}
