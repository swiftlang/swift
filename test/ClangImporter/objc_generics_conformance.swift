// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify -swift-version 4 -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

// Objective-C generic classes can conform to protocols, but for type witnesses
// (for associated types) cannot depend on the type parameters.

import Foundation
import objc_generics

// rdar://problem/34979938
protocol WithAssocT {
  associatedtype T
}

extension GenericClass : WithAssocT { // expected-error{{type 'T' involving Objective-C type parameter 'T' cannot be used for associated type 'T' of protocol 'WithAssocT'}}
}

protocol WithAssocOther {
  associatedtype Other
}

extension GenericClass : WithAssocOther {
  typealias Other = [T] // expected-error{{type 'GenericClass<T>.Other' involving Objective-C type parameter 'T' cannot be used for associated type 'Other' of protocol 'WithAssocOther'}}
}

protocol WithAssocSeparate {
  associatedtype Separate
}

extension GenericClass {
  typealias Separate = T // expected-note {{'Separate' declared here}}
}
extension GenericClass : WithAssocSeparate { // expected-error {{type 'GenericClass<T>.Separate' involving Objective-C type parameter 'T' cannot be used for associated type 'Separate' of protocol 'WithAssocSeparate'}}
}

protocol WithAssocElement {
  associatedtype Element
}

extension GenericClass : WithAssocElement {
  typealias Element = Int
}
