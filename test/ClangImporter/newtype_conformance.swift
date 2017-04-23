// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -primary-file %S/Inputs/MoreSwiftNewtypes_conformances.swift %S/Inputs/MoreSwiftNewtypes_tests.swift -module-name MoreSwiftNewtypes
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %S/Inputs/MoreSwiftNewtypes_conformances.swift -primary-file %S/Inputs/MoreSwiftNewtypes_tests.swift -module-name MoreSwiftNewtypes

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -I %S/Inputs/custom-modules -o %t %S/Inputs/MoreSwiftNewtypes_conformances.swift %S/Inputs/MoreSwiftNewtypes_tests.swift -module-name MoreSwiftNewtypes
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -I %t %s -verify

// REQUIRES: objc_interop

import Foundation
import MoreSwiftNewtypes

func acceptEquatable<T: Equatable>(_: T) {}
func acceptHashable<T: Hashable>(_: T) {}
func acceptComparable<T: Comparable>(_: T) {}

func testNewTypeWrapperComparable(x: NSNotification.Name, y: NSNotification.Name) {
  acceptEquatable(x)
  acceptHashable(x)
  acceptComparable(x)

  _ = x == y
  _ = x != y
  _ = x.hashValue
  _ = x < y
  _ = x > y
  _ = x <= y
  _ = x >= y
  _ = x as NSString
}


func testCustomWrappers(wrappedRef: WrappedRef, wrappedValue: WrappedValue) {
  acceptEquatable(wrappedRef)
  acceptEquatable(wrappedValue)
  acceptHashable(wrappedRef) // expected-error {{does not conform to expected type 'Hashable'}}
  acceptHashable(wrappedValue) // expected-error {{does not conform to expected type 'Hashable'}}
}
