// RUN: %target-typecheck-verify-swift -disable-objc-attr-requires-foundation-module -import-objc-header %swift_src_root/test/Inputs/ObjCOptionalRequirements.h

// REQUIRES: objc_interop

@objc class Object {
  var name: String

  init(name: String) {
    self.name = name
  }
}

@objc protocol SwiftProtocol {
  @objc optional var object: Object { get set }

  @objc optional subscript(_: Bool) -> Object { get set }
}

func assertExactType<T>(of _: T, is _: T.Type) {}

// An optional storage component makes the key path read-only...
do {
  let kp_property = \SwiftProtocol.object
  let kp_subscript = \SwiftProtocol.[false]

  var p: SwiftProtocol
  // expected-error@+1 {{cannot assign through subscript: 'kp_property' is a read-only key path}}
  p[keyPath: kp_property] = Object(name: "nope")
  // expected-error@+1 {{cannot assign through subscript: 'kp_subscript' is a read-only key path}}
  p[keyPath: kp_subscript] = Object(name: "nope")

  assertExactType(of: kp_property, is: KeyPath<SwiftProtocol, Object?>.self)
  assertExactType(of: kp_subscript, is: KeyPath<SwiftProtocol, Object?>.self)
}
do {
  let kp_property_objc = \ObjCProtocol.flag

  var p: ObjCProtocol
  // expected-error@+1 {{cannot assign through subscript: 'kp_property_objc' is a read-only key path}}
  p[keyPath: kp_property_objc] = false

  assertExactType(of: kp_property_objc, is: KeyPath<ObjCProtocol, Bool?>.self)
}

// ...unless a reference-writable component shows up later.
do {
  let kp_propertyForce_name = \SwiftProtocol.object!.name
  let kp_subscriptForce_name = \SwiftProtocol.[true]!.name

  let p: SwiftProtocol
  p[keyPath: kp_propertyForce_name] = "yes"
  p[keyPath: kp_subscriptForce_name] = "yes"

  assertExactType(of: kp_propertyForce_name,
                  is: ReferenceWritableKeyPath<SwiftProtocol, String>.self)
  assertExactType(of: kp_subscriptForce_name,
                  is: ReferenceWritableKeyPath<SwiftProtocol, String>.self)
}
