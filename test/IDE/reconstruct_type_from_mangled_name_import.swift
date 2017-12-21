// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s -import-objc-header %S/Inputs/reconstruct_type_helper.h | %FileCheck -check-prefix CHECK -check-prefix CHECK-%target-runtime %s

func test(
  a: Wrapped,
  b: SomeStruct,
  c: MyInt,
  d: SNColorChoice,
  e: Unwrapped,
  f: TTCollisionTypedef,
  g: TTCollisionTag
) {
  // CHECK: type: Wrapped	for 'a' mangled=$SSo7WrappedaD
  _ = a
  // CHECK: type: SomeStruct	for 'b' mangled=$SSo12SNSomeStructVD
  _ = b
  // CHECK: type: MyInt	for 'c' mangled=$SSo13SNIntegerTypeaD
  _ = c
  // CHECK: type: SNColorChoice	for 'd' mangled=$SSo13SNColorChoiceVD
  _ = d
  // CHECK: type: Unwrapped	for 'e' mangled=$SSo9UnwrappedaD
  _ = e
  // CHECK: type: TTCollisionTypedef	for 'f' mangled=$SSo19TagTypedefCollisionaD
  _ = f
  // CHECK: type: TTCollisionTag	for 'g' mangled=$SSo19TagTypedefCollisionVD
  _ = g
}

#if _runtime(_ObjC)
func testObjC(
  a: SomeClass,
  b: SomeProtocol,
  c: SNCollision,
  d: SNCollisionProtocol,
  e: CFTypeRef,
  f: CCItem,
  g: SomeClassAlias
) {
  // CHECK-objc: type: SomeClass	for 'a' mangled=$SSo11SNSomeClassCD
  _ = a
  // CHECK-objc: type: SomeProtocol	for 'b' mangled=$SSo14SNSomeProtocol_pD
  _ = b
  // CHECK-objc: type: SNCollision	for 'c' mangled=$SSo11SNCollisionCD
  _ = c
  // CHECK-objc: type: SNCollisionProtocol	for 'd' mangled=$SSo11SNCollision_pD
  _ = d
  // CHECK-objc: type: CFTypeRef	for 'e' mangled=$SSo9CFTypeRefaD
  _ = e
  // CHECK-objc: type: CCItem	for 'f' mangled=$SSo9CCItemRefaD
  _ = f
  // CHECK-objc: type: SomeClassAlias	for 'g' mangled=$SSo14SomeClassAliasaD
  _ = g
}
#endif // _ObjC
