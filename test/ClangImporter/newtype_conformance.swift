// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify

import SimpleSwiftNewtypes

func acceptEquatable<T: Equatable>(_: T) {}
func acceptHashable<T: Hashable>(_: T) {}
func acceptComparable<T: Comparable>(_: T) {}
// expected-note@-1{{where 'T' = 'Radians'}}

func testNewTypeWrapper(x: Radians, y: Radians, z: Float) {
  acceptEquatable(x)
  acceptHashable(x)
  acceptComparable(x) // expected-error {{global function 'acceptComparable' requires that 'Radians' conform to 'Comparable'}}
  // expected-note@-1 {{did you mean to use '.rawValue'?}} {{19-19=.rawValue}}

  _ = x == y
  _ = x != y
  _ = x.hashValue
}
