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
}

