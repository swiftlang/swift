// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -enable-experimental-feature ImportCStructsWithArcFields -verify -verify-ignore-unrelated %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -module-to-print=objc_structs -source-filename=x -enable-experimental-feature ImportCStructsWithArcFields -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// CHECK-IDE-TEST: struct StrongsInAStructArc {
// CHECK-IDE-TEST:   init(myobj: MYObject)
// CHECK-IDE-TEST:   var myobj: MYObject
// CHECK-IDE-TEST: }
// CHECK-IDE-TEST: func takeStrongArcStruct(_ s: StrongsInAStructArc)
// CHECK-IDE-TEST: func returnStrongArcStruct() -> StrongsInAStructArc

// CHECK-IDE-TEST: struct WeaksInAStructArc {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(myobj: MYObject?)
// CHECK-IDE-TEST:   weak var myobj: @sil_weak MYObject?
// CHECK-IDE-TEST: }

// WeakAndNonnull should not be imported at all because its only field is
// __weak + _Nonnull, which can't be represented in Swift, and partial import
// would produce an incorrect layout.
// CHECK-IDE-TEST-NOT: struct WeakAndNonnull

// A const __weak field imports as weak var with a private setter.
// CHECK-IDE-TEST: struct ConstWeakInAStruct {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(myobj: MYObject?)
// CHECK-IDE-TEST:   weak var myobj: @sil_weak MYObject? { get }
// CHECK-IDE-TEST: }

// Structs with non-trivial copy/destroy should be imported when the flag is on.
func objcStructsWithArcPointers(
  withWeaks weaks: WeaksInAStructArc,
  strongs: StrongsInAStructArc
) -> StrongsInAStructArc {
  let anObject: MYObject = weaks.myobj ?? MYObject()
  _ = WeaksInAStructArc(myobj: anObject)
  return StrongsInAStructArc(myobj: anObject)
}

func objcStructWithWeakNonnullIsNotImported() {
  _ = WeakAndNonnull() // expected-error {{cannot find 'WeakAndNonnull' in scope}}
}

func constWeakIsReadOnly(_ s: ConstWeakInAStruct) {
  let _: MYObject? = s.myobj
}

// Mixed strong + weak + trivial fields in one struct.
// CHECK-IDE-TEST: struct MixedStrongWeakArc {
// CHECK-IDE-TEST:   init(strong: MYObject, weak: MYObject?, tag: CInt)
// CHECK-IDE-TEST:   var strong: MYObject
// CHECK-IDE-TEST:   weak var weak: @sil_weak MYObject?
// CHECK-IDE-TEST:   var tag: CInt
// CHECK-IDE-TEST: }
// CHECK-IDE-TEST: func takeMixedArcStruct(_ s: MixedStrongWeakArc)

func mixedStructFieldAccess(_ s: MixedStrongWeakArc) -> (MYObject, MYObject?, Int32) {
  return (s.strong, s.weak, s.tag)
}

func mixedStructConstruction() -> MixedStrongWeakArc {
  return MixedStrongWeakArc(strong: MYObject(), weak: MYObject(), tag: 42)
}

// Weak fields with a Swift-bridged ObjC type (NSString) should NOT be bridged.
// Bridging would require loading the weak reference (consuming a +1 retain),
// converting to the bridged type, then releasing -- losing the weak semantics.
// CHECK-IDE-TEST: struct WeakNSStringArc {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(name: NSString?, tag: CInt)
// CHECK-IDE-TEST:   weak var name: @sil_weak NSString?
// CHECK-IDE-TEST:   var tag: CInt
// CHECK-IDE-TEST: }

// Weak NSString field is accessed as NSString?, not String.
func weakNSStringFieldAccess(_ s: WeakNSStringArc) -> NSString? {
  return s.name
}

func weakNSStringFieldStore(_ str: NSString) {
  var s = WeakNSStringArc()
  s.name = str
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

func passMixedStructToCFunction() {
  let s = MixedStrongWeakArc(strong: MYObject(), weak: MYObject(), tag: 7)
  takeMixedArcStruct(s)
}

// Nested structs with strong ARC fields should be imported.
// CHECK-IDE-TEST: struct OuterArcStruct {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(nested: InnerArcStruct, tag: CInt)
// CHECK-IDE-TEST:   var nested: InnerArcStruct
// CHECK-IDE-TEST:   var tag: CInt
// CHECK-IDE-TEST: }
func testNestedArcStructImport() {
  let inner = InnerArcStruct(inner: MYObject())
  let outer = OuterArcStruct(nested: inner, tag: 42)
  let _: MYObject = outer.nested.inner
}

// Nested structs with weak ARC fields should also be imported.
// CHECK-IDE-TEST: struct WeakInAStructArc {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(weakobj: MYObject?)
// CHECK-IDE-TEST:   weak var weakobj: @sil_weak MYObject?
// CHECK-IDE-TEST: }
// CHECK-IDE-TEST: struct OuterWithWeakInner {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(nested: WeakInAStructArc)
// CHECK-IDE-TEST:   var nested: WeakInAStructArc
// CHECK-IDE-TEST: }
func testNestedWeakArcStructImport() {
  let inner = WeakInAStructArc(weakobj: MYObject())
  _ = OuterWithWeakInner(nested: inner)
}

// Union with strong ARC fields should not be imported.
func unionWithStrongIsRejected() {
  _ = UnionWithStrong() // expected-error {{cannot find 'UnionWithStrong' in scope}}
}

// Struct containing a union with strong ARC fields should not be imported.
func structWithArcUnionIsRejected() {
  _ = StructWithArcUnion() // expected-error {{cannot find 'StructWithArcUnion' in scope}}
}
