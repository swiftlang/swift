// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s -import-objc-header %S/Inputs/reconstruct_type_helper.h | %FileCheck -check-prefix CHECK -check-prefix CHECK-%target-runtime %s

func test(
  a: Wrapped,
  b: SomeStruct,
  c: MyInt,
  d: SNColorChoice,
  e: Unwrapped,
  f: TTCollisionTypedef,
  g: TTCollisionTag,
  h: EnumByTag,
  i: EnumByTypedef,
  j: EnumByBoth
) {
  // CHECK: type: Wrapped	for 'a' mangled=$sSo7WrappedaD
  _ = a
  // CHECK: type: SomeStruct	for 'b' mangled=$sSo12SNSomeStructVD
  _ = b
  // CHECK: type: MyInt	for 'c' mangled=$sSo13SNIntegerTypeaD
  _ = c
  // CHECK: type: SNColorChoice	for 'd' mangled=$sSo13SNColorChoiceVD
  _ = d
  // CHECK: type: Unwrapped	for 'e' mangled=$sSo9UnwrappedaD
  _ = e
  // CHECK: type: TTCollisionTypedef	for 'f' mangled=$sSo19TagTypedefCollisionaD
  _ = f
  // CHECK: type: TTCollisionTag	for 'g' mangled=$sSo19TagTypedefCollisionVD
  _ = g
  // CHECK: type: EnumByTag	for 'h' mangled=$sSo9EnumByTagVD
  _ = h
  // CHECK: type: EnumByTypedef	for 'i' mangled=$sSo13EnumByTypedefaD
  _ = i
  // CHECK: type: EnumByBoth	for 'j' mangled=$sSo10EnumByBothVD
  _ = j
}

#if _runtime(_ObjC)
func testObjC(
  a: SomeClass,
  b: SomeProtocol,
  c: SNCollision,
  d: SNCollisionProtocol,
  e: CFTypeRef,
  f: CCItem,
  g: SomeClassAlias,
  h: SomeError,
  i: SomeError.Code,
  j: SomeRenamedError,
  k: SomeRenamedError.Code
) {
  // CHECK-objc: type: SomeClass	for 'a' mangled=$sSo11SNSomeClassCD
  _ = a
  // CHECK-objc: type: SomeProtocol	for 'b' mangled=$sSo14SNSomeProtocol_pD
  _ = b
  // CHECK-objc: type: SNCollision	for 'c' mangled=$sSo11SNCollisionCD
  _ = c
  // CHECK-objc: type: SNCollisionProtocol	for 'd' mangled=$sSo11SNCollision_pD
  _ = d
  // CHECK-objc: type: CFTypeRef	for 'e' mangled=$sSo9CFTypeRefaD
  _ = e
  // CHECK-objc: type: CCItem	for 'f' mangled=$sSo9CCItemRefaD
  _ = f
  // CHECK-objc: type: SomeClassAlias	for 'g' mangled=$sSo14SomeClassAliasaD
  _ = g
  // CHECK-objc: type: SomeError	for 'h' mangled=$sSC9SomeErrorLeVD
  _ = h
  // CHECK-objc: type: SomeError.Code	for 'i' mangled=$sSo9SomeErrorVD
  _ = i
  // CHECK-objc: type: SomeRenamedError	for 'j' mangled=$sSC14SomeOtherErrorLEVD
  _ = j
  // CHECK-objc: type: SomeRenamedError.Code	for 'k' mangled=$sSo14SomeOtherErroraD
  _ = k
}
#endif // _ObjC
