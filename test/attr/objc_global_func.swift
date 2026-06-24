// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop

import Foundation

// A plain top-level @objc function is accepted (SE-0495).
@objc func basic() {}

// @objc accepts an explicit, simple C name.
@objc(customName) func named() {}

// The @objc(name) argument only sets the C/linker symbol; the Swift-visible
// name remains the base identifier. The C name is not in Swift scope.
func callsBySwiftName() {
  named()        // OK: the Swift base name is visible.
  customName()   // expected-error {{cannot find 'customName' in scope}}
}

// Unlike @c, @objc accepts Objective-C class types in the signature.
@objc class ObjCClass: NSObject {}
@objc func objcClassReturn() -> ObjCClass { fatalError() }
@objc func objcClassParams(a: ObjCClass, b: ObjCClass) {}

// ... @objc protocols ...
@objc protocol ObjCProtocol {}
@objc func objcProtocolParam(a: ObjCProtocol) {}

// ... and @objc enums.
@objc enum ObjCEnum: Int { case A, B }
@objc func objcEnumParam(a: ObjCEnum) {}

// Primitive C types remain representable.
@objc func primitives(x: Int, y: Double) -> Int { x }

// Bridged Swift types are accepted: String <-> NSString.
@objc func takesString(s: String) {}
@objc func returnsString() -> String { "" }

// Bridged collection types: [NSObject] <-> NSArray.
@objc func takesArray(a: [NSObject]) {}
@objc func returnsArray() -> [NSObject] { [] }

// Bridged dictionary types: [String: NSObject] <-> NSDictionary.
@objc func takesDictionary(d: [String: NSObject]) {}

// Only a simple (nullary) C name is allowed; selector-style names are rejected.
@objc(doSomething:with:) func selectorName(a: Int, b: Int) {}
// expected-error @-1 {{'@objc' global function must have a simple name}}

// Swift-only types are still not representable in Objective-C.
struct SwiftStruct {}
@objc func swiftStructParam(a: SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
// expected-note @-2 {{Swift structs cannot be represented in Objective-C}}

// @objc on a member function still requires a class or @objc protocol context.
struct NonClass {
  @objc func method() {}
  // expected-error @-1 {{'@objc' can only be used with members of classes, '@objc' protocols, and concrete extensions of classes}}
}

// @objc and @c cannot be combined on the same global function.
@c @objc func both() {}
// expected-error @-1 {{cannot apply both '@c' and '@objc' to global function}}

// When @c rejects a type that @objc would accept, suggest @objc.
@c(bridgedFunc) func bridged(name: String) -> String { name }
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}
// expected-note @-3 {{use '@objc' to expose this function to Objective-C}} {{1-16=@objc(bridgedFunc)}}

@c func bridgedNoName(name: String) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}
// expected-note @-3 {{use '@objc' to expose this function to Objective-C}} {{1-3=@objc}}

// No @objc suggestion when the type is not valid in Objective-C either.
struct PureSwiftStruct {}
@c(pureSwift) func pureSwift(s: PureSwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}
