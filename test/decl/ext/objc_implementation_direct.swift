// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementation
// REQUIRES: swift_feature_ObjCDirect

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -import-objc-header %S/Inputs/objc_implementation_direct.h \
// RUN:   -enable-experimental-feature ObjCImplementation \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -target %target-stable-abi-triple \
// RUN:   -Xcc -Wno-nullability-completeness %t/feature.swift

// Directness has to match even without the feature, which '@objcDirect' needs.
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -import-objc-header %S/Inputs/objc_implementation_direct.h \
// RUN:   -enable-experimental-feature ObjCImplementation \
// RUN:   -target %target-stable-abi-triple \
// RUN:   -Xcc -Wno-nullability-completeness %t/nofeature.swift

//--- feature.swift

import Foundation

@objc @implementation extension DirectImplClass {
  @objcDirect func directMethod() -> Int32 { 0 }

  func plainMethod() {}

  func anotherDirectMethod() -> Int32 { 0 }
  // expected-error@-1 {{should be '@objcDirect'}}

  @objcDirect func anotherPlainMethod() {}
  // expected-error@-1 {{should not be '@objcDirect'}}

  // '@property (direct)' imports as a direct property.
  @objcDirect var directProperty: Int32 { get { 0 } set {} }

  var plainProperty: Int32 { get { 0 } set {} }

  var anotherDirectProperty: Int32 { get { 0 } set {} }
  // expected-error@-1 {{should be '@objcDirect'}}

  @objcDirect var anotherPlainProperty: Int32 { get { 0 } set {} }
  // expected-error@-1 {{should not be '@objcDirect'}}
}

//--- nofeature.swift

import Foundation

@objc @implementation extension DirectImplClass {
  func directMethod() -> Int32 { 0 }
  // expected-error@-1 {{instance method 'directMethod()' should be '@objcDirect' to match instance method declared by the header}}
  // expected-note@-2 {{'@objcDirect' is an experimental feature; use '-enable-experimental-feature ObjCDirect'}}

  func plainMethod() {}

  func anotherDirectMethod() -> Int32 { 0 }
  // expected-error@-1 {{should be '@objcDirect'}}
  // expected-note@-2 {{'@objcDirect' is an experimental feature}}

  func anotherPlainMethod() {}

  var directProperty: Int32 { get { 0 } set {} }
  // expected-error@-1 {{property 'directProperty' should be '@objcDirect' to match property declared by the header}}
  // expected-note@-2 {{'@objcDirect' is an experimental feature}}

  var plainProperty: Int32 { get { 0 } set {} }

  var anotherDirectProperty: Int32 { get { 0 } set {} }
  // expected-error@-1 {{should be '@objcDirect'}}
  // expected-note@-2 {{'@objcDirect' is an experimental feature}}

  var anotherPlainProperty: Int32 { get { 0 } set {} }
}
