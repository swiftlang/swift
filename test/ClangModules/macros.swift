// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// XFAIL: linux

@exported import macros

func circle_area(radius: CDouble) -> CDouble {
  return M_PI * radius * radius
}

func circle_area2(radius: CDouble) -> CDouble {
  return A_PI * radius * radius
}

func circle_area3(radius: CFloat) -> CFloat {
  return M_PIf * radius * radius
}

func convertGLBool(b: CInt) -> Bool {
  return b != GL_FALSE
}

func pixelFormat(alpha: Bool) -> CInt {
  if alpha {
    return GL_RGBA
  } else {
    return GL_RGB
  }
}

func boundsCheckU32(x: CUnsignedInt) -> Bool {
  return x >= 0 && x <= UINT32_MAX
}

func boundsCheckS64(x: CLongLong) -> Bool {
  return x <= INT64_MAX
}

func isEOF(c: CInt) -> Bool {
  return c == EOF
}

func subThree(x: CInt) -> CInt {
  return x + MINUS_THREE
}

// true/false are keywords, so they shouldn't conflict with the true/false in
// the C header.
func testTrueFalse() {
  var x : Bool = true
  var y : Bool = false

  _ = true // should not result in ambiguous use error
  _ = false

  _ = TRUE // expected-error {{use of unresolved identifier 'TRUE'}}
  _ = FALSE // expected-error {{use of unresolved identifier 'FALSE'}}

  _ = `true` // expected-error {{use of unresolved identifier 'true'}}
  _ = `false` // expected-error {{use of unresolved identifier 'false'}}
}

func testCStrings() -> Bool {
  var str: String = UTF8_STRING
  str = VERSION_STRING
  _ = str
}

func testObjCString() -> String {
  let str: String = OBJC_STRING
  return str
}

func testCFString() -> String {
  let str: String = CF_STRING
  return str
}

func testInvalidIntegerLiterals() {
  var l1 = INVALID_INTEGER_LITERAL_1 // expected-error {{use of unresolved identifier 'INVALID_INTEGER_LITERAL_1'}}
  // FIXME: <rdar://problem/16445608> Swift should set up a DiagnosticConsumer for Clang
  // var l2 = INVALID_INTEGER_LITERAL_2 // FIXME {{use of unresolved identifier 'INVALID_INTEGER_LITERAL_2'}}
}

func testUsesMacroFromOtherModule() {
  let m1 = USES_MACRO_FROM_OTHER_MODULE_1
  let m2 = macros.USES_MACRO_FROM_OTHER_MODULE_1
  let m3 = USES_MACRO_FROM_OTHER_MODULE_2 // expected-error {{use of unresolved identifier 'USES_MACRO_FROM_OTHER_MODULE_2'}}
  let m4 = macros.USES_MACRO_FROM_OTHER_MODULE_2 // expected-error {{module 'macros' has no member named 'USES_MACRO_FROM_OTHER_MODULE_2'}}
}

func testSuppressed() {
  let m1 = NS_BLOCKS_AVAILABLE // expected-error {{use of unresolved identifier 'NS_BLOCKS_AVAILABLE'}}
  let m2 = CF_USE_OSBYTEORDER_H // expected-error {{use of unresolved identifier 'CF_USE_OSBYTEORDER_H'}}
}

func testNil() {
  var localNil: ()
  localNil = NULL_VIA_NAME    // expected-error {{'NULL_VIA_NAME' is unavailable: use 'nil' instead of this imported macro}}
  localNil = NULL_VIA_VALUE    // expected-error {{'NULL_VIA_VALUE' is unavailable: use 'nil' instead of this imported macro}}
  localNil = NULL_AS_NIL       // expected-error {{'NULL_AS_NIL' is unavailable: use 'nil' instead of this imported macro}}
  localNil = NULL_AS_CLASS_NIL // expected-error {{'NULL_AS_CLASS_NIL' is unavailable: use 'nil' instead of this imported macro}}

  localNil = Nil // expected-error {{use of unresolved identifier 'Nil'}}
}

func testBitwiseOps() {
  let _: CUnsignedLongLong = DISPATCH_TIME_FOREVER
  let _: CInt = BIT_SHIFT_1 | BIT_SHIFT_2
  let _: CLongLong = BIT_SHIFT_3
  let _: CUnsignedInt = BIT_SHIFT_4
}

func testRecursion() {
  _ = RECURSION // expected-error {{use of unresolved identifier 'RECURSION'}}
  _ = REF_TO_RECURSION // expected-error {{use of unresolved identifier 'REF_TO_RECURSION'}}
}
