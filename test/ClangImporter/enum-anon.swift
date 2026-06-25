// RUN: %target-swift-frontend -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/enum-anon.h -verify

func testDiags() {
#if _runtime(_ObjC)
  let us2 = USConstant2
#else
  let us2: UInt16 = 0
#endif
  let _: String = us2 // expected-error {{cannot convert value of type 'UInt16' to specified type 'String'}}

#if _runtime(_ObjC)
  let usVar2 = USVarConstant2
#else
  let usVar2: UInt16 = 0
#endif
  let _: String = usVar2 // expected-error {{cannot convert value of type 'UInt16' to specified type 'String'}}

  // The nested anonymous enum value appears as a member of its enclosing struct.
  let nestedConstant2 = Struct.NestedConstant2

  // We historically imported these at top-level scope. Check that this still works for backwards compat.
  let _ = NestedConstant2
  // expected-warning@-1 {{'NestedConstant2' is deprecated: nested C-style enums are imported as members of the enclosing struct}}
  // expected-note@-2 {{use 'Struct.NestedConstant2' instead}}

  let _ = TdStruct.TdNestedInStruct
  let _ = TdNestedInStruct
  // expected-warning@-1 {{'TdNestedInStruct' is deprecated: nested C-style enums are imported as members of the enclosing struct}}
  // expected-note@-2 {{use 'TdStruct.TdNestedInStruct' instead}}

  let _ = TdUnion.TdNestedInUnion
  let _ = TdNestedInUnion
  // expected-warning@-1 {{'TdNestedInUnion' is deprecated: nested C-style enums are imported as members of the enclosing union}}
  // expected-note@-2 {{use 'TdUnion.TdNestedInUnion' instead}}

  // These don't have deprecation notices (yet) because the alternative is to
  // write something like 'NestedTagless.__Unnamed_union.TLEnumField' which is
  // not good. Ideally this should be written 'NestedTagless.TLEnumField' but
  // no such shim exists today.
  let _ = TLEnumField
  let _ = TLEnumDecl
  let _ = TL2EnumField
  let _ = TL2EnumDecl

  let _ = AnonEnumField
  let _ = AnonEnumDecl
  let _ = Anon2EnumField
  let _ = Anon2EnumDecl
}

