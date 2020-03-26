// REQUIRES: rdar60050653
// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S
// RUN: cd %t && %target-swiftc_driver -typecheck -output-file-map %t/output.json -incremental -module-name main -verify-incremental-dependencies %s

import Foundation

// expected-provides {{LookupFactory}}
// expected-provides {{NSObject}}
// expected-private-superclass {{__C.NSObject}}
// expected-private-conformance {{Foundation._KeyValueCodingAndObserving}}
// expected-private-conformance {{Swift.Hashable}}
// expected-private-conformance {{Swift.Equatable}}
// expected-private-conformance {{Swift.CustomDebugStringConvertible}}
// expected-private-conformance {{Swift.CVarArg}}
// expected-private-conformance {{Swift.CustomStringConvertible}}
// expected-cascading-superclass {{main.LookupFactory}}
@objc private class LookupFactory: NSObject {
  // expected-provides {{AssignmentPrecedence}}
  // expected-provides {{IntegerLiteralType}}
  // expected-provides {{FloatLiteralType}}
  // expected-provides {{Int}}
  // expected-cascading-member {{__C.NSObject.someMember}}
  // expected-cascading-member {{__C.NSObject.Int}}
  // expected-cascading-member {{main.LookupFactory.Int}}
  @objc var someMember: Int = 0
  // expected-cascading-member {{__C.NSObject.someMethod}}
  @objc func someMethod() {}

  // expected-cascading-member {{__C.NSObject.init}}
  // expected-cascading-member {{main.LookupFactory.init}}
  // expected-private-member {{main.LookupFactory.deinit}}
  // expected-cascading-member {{main.LookupFactory.someMember}}
  // expected-cascading-member {{main.LookupFactory.someMethod}}
}

// expected-private-member {{Swift.ExpressibleByNilLiteral.callAsFunction}}
// expected-private-member {{Swift.CustomReflectable.callAsFunction}}
// expected-private-member {{Swift._ObjectiveCBridgeable.callAsFunction}}
// expected-private-member {{Swift.Optional.callAsFunction}}
// expected-private-member {{Swift.CustomDebugStringConvertible.callAsFunction}}
// expected-private-member {{Swift.Equatable.callAsFunction}}
// expected-private-member {{Swift.Hashable.callAsFunction}}
// expected-private-member {{Swift.Encodable.callAsFunction}}
// expected-private-member {{Swift.Decodable.callAsFunction}}
// expected-private-member {{Foundation._OptionalForKVO.callAsFunction}}

// expected-provides {{AnyObject}}
func lookupOnAnyObject(object: AnyObject) { // expected-provides {{lookupOnAnyObject}}
  _ = object.someMember // expected-private-dynamic-member {{someMember}}
  object.someMethod() // expected-private-dynamic-member {{someMethod}}
}
