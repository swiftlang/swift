// RUN: %target-swift-frontend -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/enum-anon.h -DDIAGS -verify

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

  // The nested anonymous enum value should still have top-level scope, because
  // that's how C works. It should also have the same type as the field (above).
  let _: String = SR2511.SR2511B // expected-error {{type 'SR2511' has no member 'SR2511B'}}
}

