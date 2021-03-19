// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios
// UNSUPPORTED: CPU=armv7k && OS=ios
// Exclude iOS-based 32-bit platforms because the Foundation overlays introduce
// an extra dependency on _KeyValueCodingAndObservingPublishing only for 64-bit
// platforms.
// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S
// RUN: cd %t && %target-swiftc_driver -typecheck -output-file-map %t/output.json -incremental -module-name main -verify-incremental-dependencies %s

import Foundation

// expected-provides {{LookupFactory}}
// expected-provides {{NSObject}}
// expected-superclass {{ObjectiveC.NSObject}}
// expected-conformance {{ObjectiveC.NSObjectProtocol}}
// expected-conformance {{Foundation._KeyValueCodingAndObserving}}
// expected-conformance {{Foundation._KeyValueCodingAndObservingPublishing}}
// expected-conformance {{Swift.Hashable}}
// expected-conformance {{Swift.Equatable}}
// expected-conformance {{Swift.CustomDebugStringConvertible}}
// expected-conformance {{Swift.CVarArg}}
// expected-conformance {{Swift.CustomStringConvertible}}
// expected-member {{Swift._ExpressibleByBuiltinIntegerLiteral.init}}
@objc private class LookupFactory: NSObject {
  // expected-provides {{AssignmentPrecedence}}
  // expected-provides {{IntegerLiteralType}}
  // expected-provides {{FloatLiteralType}}
  // expected-provides {{Int}}
  // expected-member {{ObjectiveC.NSObject.someMember}}
  // expected-member {{ObjectiveC.NSObject.Int}}
  // expected-member {{ObjectiveC.NSObjectProtocol.someMember}}
  // expected-member {{ObjectiveC.NSObjectProtocol.Int}}
  // expected-member {{main.LookupFactory.Int}}
  @objc var someMember: Int = 0
  // expected-member {{ObjectiveC.NSObject.someMethod}}
  // expected-member {{ObjectiveC.NSObjectProtocol.someMethod}}
  @objc func someMethod() {}

  // expected-member {{ObjectiveC.NSObject.init}}
  // expected-member {{ObjectiveC.NSObjectProtocol.init}}
  // expected-member {{main.LookupFactory.init}}
  // expected-member {{main.LookupFactory.deinit}}
  // expected-member {{main.LookupFactory.someMember}}
  // expected-member {{main.LookupFactory.someMethod}}
}

// expected-member {{Swift.ExpressibleByNilLiteral.callAsFunction}}
// expected-member {{Swift.CustomReflectable.callAsFunction}}
// expected-member {{Swift._ObjectiveCBridgeable.callAsFunction}}
// expected-member {{Swift.Optional<Wrapped>.callAsFunction}}
// expected-member {{Swift.CustomDebugStringConvertible.callAsFunction}}
// expected-member {{Swift.Equatable.callAsFunction}}
// expected-member {{Swift.Hashable.callAsFunction}}
// expected-member {{Swift.Encodable.callAsFunction}}
// expected-member {{Swift.Decodable.callAsFunction}}
// expected-member {{Foundation._OptionalForKVO.callAsFunction}}

// expected-provides {{AnyObject}}
func lookupOnAnyObject(object: AnyObject) { // expected-provides {{lookupOnAnyObject}}
  _ = object.someMember // expected-dynamic-member {{someMember}}
  object.someMethod() // expected-dynamic-member {{someMethod}}
}

// expected-member {{Swift.Hashable.someMethod}}
// expected-member {{Foundation._KeyValueCodingAndObserving.someMethod}}
// expected-member {{Foundation._KeyValueCodingAndObservingPublishing.someMethod}}
// expected-member {{Swift.Equatable.someMethod}}
// expected-member {{Swift.CVarArg.someMethod}}
// expected-member {{Swift.CustomStringConvertible.someMethod}}
// expected-member {{Swift.CustomDebugStringConvertible.someMethod}}
// expected-member {{Swift.Equatable.someMember}}
// expected-member{{Swift.CVarArg.someMember}}
// expected-member{{Foundation._KeyValueCodingAndObservingPublishing.someMember}}
// expected-member{{Foundation._KeyValueCodingAndObserving.someMember}}
// expected-member{{Swift.CustomDebugStringConvertible.someMember}}
// expected-member{{Swift.CustomStringConvertible.someMember}}
// expected-member{{Swift.Hashable.someMember}}
// expected-member{{Swift.Sendable.callAsFunction}}
