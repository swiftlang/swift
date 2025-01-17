// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-stable-abi-triple -I %S/Inputs/abi -F %clang-importer-sdk-path/frameworks %s -import-objc-header %S/Inputs/objc_implementation.h -emit-ir -o %t.ir -enable-library-evolution -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-stable-abi-triple -I %S/Inputs/abi -F %clang-importer-sdk-path/frameworks %s -import-objc-header %S/Inputs/objc_implementation.h -emit-ir -o %t.ir -enable-library-evolution -enable-experimental-feature ObjCImplementationWithResilientStorage -target %target-future-triple
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementationWithResilientStorage

@objc @implementation extension ImplClassWithResilientStoredProperty {
  @objc var beforeInt: Int32 = 0    // no-error
  final var a: Mirror?              // expected-error {{does not support stored properties whose size can change due to library evolution; store this value in an object or 'any' type}}
  final var b: AnyKeyPath?          // no-error
  final var c: Int = 0              // no-error
  final var d: S1?                  // no-error
  final var e: S2?                  // expected-error {{does not support stored properties whose size can change due to library evolution; store this value in an object or 'any' type}}
  final var f: C1?                  // no-error
  @objc var afterInt: Int32 = 0     // no-error
}

public struct S1 {
  var int = 1
}

public struct S2 {
  var mirror: Mirror?
}

public class C1 {
  var mirror: Mirror?
}
