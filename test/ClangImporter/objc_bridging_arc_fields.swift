// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -enable-experimental-feature ImportCStructsWithArcFields -verify -verify-ignore-unrelated %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -module-to-print=objc_structs -source-filename=x -enable-experimental-feature ImportCStructsWithArcFields -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// The existing StrongsInAStruct (which has no nullability annotation) is also
// importable with the flag. Its __strong NSString * field gets bridged to
// String! with the stored property renamed to _nsstr.
// CHECK-IDE-TEST: struct StrongsInAStruct {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(nsstr: String!)
// CHECK-IDE-TEST:   var _nsstr: NSString!
// CHECK-IDE-TEST:   var nsstr: String!
// CHECK-IDE-TEST: }

// CHECK-IDE-TEST: struct StrongsInAStructArc {
// CHECK-IDE-TEST:   init(myobj: MYObject)
// CHECK-IDE-TEST:   var myobj: MYObject
// CHECK-IDE-TEST: }
// CHECK-IDE-TEST: func takeStrongArcStruct(_ s: StrongsInAStructArc)
// CHECK-IDE-TEST: func returnStrongArcStruct() -> StrongsInAStructArc

// Structs with non-trivial copy/destroy should be imported when the flag is on.
func testStrongStructImport() -> StrongsInAStructArc {
  let anObject = MYObject()
  let aStrongInAStruct = StrongsInAStructArc(myobj: anObject)
  return aStrongInAStruct
}

// The existing StrongsInAStruct becomes available with the feature flag.
func testExistingStrongStructImport() {
  var s = StrongsInAStruct()
  s.nsstr = "hello"
  _ = s
}

// Strong fields with a Swift-bridged ObjC type (NSString -> String) should
// import as computed properties backed by private stored properties.
// CHECK-IDE-TEST: struct StrongNSStringArc {
// CHECK-IDE-TEST:   init(name: String, tag: Int32)
// CHECK-IDE-TEST:   var _name: NSString
// CHECK-IDE-TEST:   var tag: Int32
// CHECK-IDE-TEST:   var name: String
// CHECK-IDE-TEST: }

func bridgedFieldAccess(_ s: StrongNSStringArc) -> String {
  return s.name
}

func bridgedFieldConstruction() -> StrongNSStringArc {
  return StrongNSStringArc(name: "hello", tag: 42)
}

func bridgedFieldMutation() {
  var s = StrongNSStringArc(name: "hello", tag: 1)
  s.name = "world"
  _ = s
}

// An ARC struct marked NS_SWIFT_UNAVAILABLE is still imported but cannot be used.
func unavailableArcStructIsRejected() {
  _ = UnavailableArcStruct(myobj: MYObject()) // expected-error {{'UnavailableArcStruct' is unavailable in Swift: Use MySwiftType instead}}
}

// An ARC struct with swift_name is imported under its Swift name.
// CHECK-IDE-TEST: struct RenamedArcStruct {
// CHECK-IDE-TEST:   init(myobj: MYObject)
// CHECK-IDE-TEST:   var myobj: MYObject
// CHECK-IDE-TEST: }

func renamedArcStructUsesSwiftName() {
  _ = RenamedArcStruct(myobj: MYObject())
}

func renamedArcStructRejectsCName() {
  _ = CNameForRenamedArcStruct(myobj: MYObject()) // expected-error {{'CNameForRenamedArcStruct' has been renamed to 'RenamedArcStruct'}}
}

// Passing ARC structs to and from C functions.
func passStrongStructToCFunction() {
  let s = StrongsInAStructArc(myobj: MYObject())
  takeStrongArcStruct(s)
}

func receiveStrongStructFromCFunction() {
  let s = returnStrongArcStruct()
  let _: MYObject = s.myobj
}
