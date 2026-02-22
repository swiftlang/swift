// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -typecheck -verify %s -I %S/Inputs/custom-modules

import CXXInterop

// Basic structs
do {
  var tmp: Basic = makeA()
  tmp.a = 3
  tmp.b = nil
}

// Namespace lookup
func namespaceLookup() -> UnsafeMutablePointer<ns.T> {
  let tmp: UnsafeMutablePointer<ns.T> = ns.doMakeT()!
  return tmp
}

func unimportedSwiftPrivateDataMember() {
  let obj = Methods()
  _ = obj.some_value // Sanity check
  _ = obj.private_value // expected-error {{value of type 'Methods' has no member 'private_value'}}
}

func unimportedSwiftPrivateMethod() {
  var obj = Methods()
  _ = obj.SimpleMethod(0) // Sanity check
  _ = obj.PrivateSimpleMethod(0) // expected-error {{value of type 'Methods' has no member 'PrivateSimpleMethod'}}
}

func unimportedSwiftPrivateFunction() {
  PrivateFunction() // expected-error {{cannot find 'PrivateFunction' in scope}}
}

func unimportedSwiftPrivateClass() {
  _ = PrivateClass() // expected-error {{cannot find 'PrivateClass' in scope}}
}
