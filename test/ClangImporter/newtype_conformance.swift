// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify

import Foundation

func acceptEquatable<T: Equatable>(_: T) {}
func acceptHashable<T: Hashable>(_: T) {}
func acceptComparable<T: Comparable>(_: T) {}
// expected-note@-1 {{where 'T' = 'NSNotification.Name'}}

func testNewTypeWrapper(x: NSNotification.Name, y: NSNotification.Name, z: NSFileAttributeKey) {
  acceptEquatable(x)
  acceptHashable(x)
  acceptComparable(x) // expected-error {{global function 'acceptComparable' requires that 'NSNotification.Name' conform to 'Comparable'}}
  // expected-note@-1 {{did you mean to use '.rawValue'?}} {{19-19=.rawValue}}

  _ = x == y
  _ = x != y
  _ = x.hashValue

#if _runtime(_ObjC)
  _ = x as NSString

  _ = z as NSString
#endif
}
