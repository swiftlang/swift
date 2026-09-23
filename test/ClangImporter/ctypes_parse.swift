// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated %clang-importer-sdk -verify-ignore-unknown -verify-additional-prefix legacy-c-array-
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated %clang-importer-sdk -verify-ignore-unknown -verify-additional-prefix modern-c-array- -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple

// REQUIRES: swift_feature_ModernImportedCArrays

import ctypes

func checkRawRepresentable<T: RawRepresentable>(_: T) {}
func checkEquatable<T: Equatable>(_: T) -> Bool {}
func checkEquatablePattern(_ c: Color) {
  switch c {
    case red: return
    case green: return
    case blue: return
    default: return
  }
}

func testColor() {
  var c: Color = red
  c = blue
  _ = c.rawValue
  checkRawRepresentable(c)
  _ = checkEquatable(c)
  checkEquatablePattern(c)
}

func testTribool() {
  var b = Indeterminate
  b = True
  _ = b.rawValue
}

func verifyIsInt(_: inout Int) { }
func verifyIsUInt(_: inout UInt) { }
func verifyIsUInt64(_: inout UInt64) { }

func testAnonEnum() {
  var a = AnonConst1
  a = AnonConst2
#if os(Windows)
  verifyIsInt(&a)
#elseif _pointerBitWidth(_32)
  verifyIsUInt64(&a)
#elseif _pointerBitWidth(_64)
  verifyIsUInt(&a)
#else
#error("Unknown platform")
#endif
}

func testAnonEnumSmall() {
  var a = AnonConstSmall1
  a = AnonConstSmall2
  _ = a as Int
}

func testPoint() -> Float {
  var p: Point
  p.x = 1.0
  return p.y
}

func testAnonStructs() {
  var a_s: AnonStructs
  a_s.a = 5
  a_s.b = 3.14
  a_s.c = 7.5
}

func testUnnamedStructs() {
  var u_s: UnnamedStructs
  u_s.x.a = 1
  u_s.x.b = 3.14
  u_s.x.c = "error" // expected-error{{value of type 'UnnamedStructs.__Unnamed_struct_x' has no member 'c'}}
  u_s.y.a = 3.14
  u_s.y.b = 2
  u_s.y.c = "error" // expected-error{{value of type 'UnnamedStructs.__Unnamed_struct_y' has no member 'c'}}
  u_s.y.z.c = 3
  u_s.y.z.d = "error" // expected-error{{value of type 'UnnamedStructs.__Unnamed_struct_y.__Unnamed_struct_z' has no member 'd'}}

  let _ = u_s.x
  let _: UnnamedStructs.__Unnamed_struct_x = u_s.x
}

// FIXME: Import pointers to opaque types as unique types.

func testPointers() {
  _ = HWND(bitPattern: 0)
}

// Ensure that imported structs can be extended, even if typedef'ed on the C
// side.

func sqrt(_ x: Float) -> Float {}
func atan2(_ x: Float, _ y: Float) -> Float {}

extension Point {
  func asPolar() -> (rho: Float, theta: Float) {
    return (sqrt(x*x + y*y), atan2(x, y))
  }
}

extension AnonStructs {
  func frob() -> Double {
    return Double(a) + Double(b) + c
  }
}

func testFuncStructDisambiguation() {
  let a : funcOrStruct
  var i = funcOrStruct()
  i = 5
  _ = i
  var a2 = funcOrStruct(i: 5)
  a2 = a
  _ = a2
}

func testVoid() {
  var x: MyVoid // expected-error{{cannot find type 'MyVoid' in scope}}
  returnsMyVoid()
}

var word: Int = 0
var uword: UInt = 0

func testImportStdintTypes() {
  var t9_unqual : Int = intptr_t_test
  var t10_unqual : UInt = uintptr_t_test
  t9_unqual = word
  t10_unqual = uword
  _ = t9_unqual
  _ = t10_unqual

  var t9_qual : intptr_t = 0 // no-warning
  var t10_qual : uintptr_t = 0 // no-warning
  t9_qual = word
  t10_qual = uword
  _ = t9_qual
  _ = t10_qual
}

func testImportStddefTypes() {
  let t1_unqual: Int = ptrdiff_t_test
  let t2_unqual: Int = size_t_test
  let t3_unqual: Int = rsize_t_test

  _ = t1_unqual as ctypes.ptrdiff_t
  _ = t2_unqual as ctypes.size_t
  _ = t3_unqual as ctypes.rsize_t
}

func testImportSysTypesTypes() {
  let t1_unqual: Int = ssize_t_test
  _ = t1_unqual as ctypes.ssize_t
}

func testImportOSTypesTypes() {
  var t1_unqual: CInt = SInt_test
  var t2_unqual: CUnsignedInt = UInt_test

  var t1_qual: ctypes.SInt = t1_unqual // expected-error {{no type named 'SInt' in module 'ctypes'}}
  var t2_qual: ctypes.UInt = t2_unqual // expected-error {{no type named 'UInt' in module 'ctypes'}}
}

func testImportTagDeclsAndTypedefs() {
  var t1 = FooStruct1(x: 0, y: 0.0)
  t1.x = 0
  t1.y = 0.0

  var t2 = FooStruct2(x: 0, y: 0.0)
  t2.x = 0
  t2.y = 0.0

  var t3 = FooStruct3(x: 0, y: 0.0)
  t3.x = 0
  t3.y = 0.0

  var t4 = FooStruct4(x: 0, y: 0.0)
  t4.x = 0
  t4.y = 0.0

  var t5 = FooStruct5(x: 0, y: 0.0)
  t5.x = 0
  t5.y = 0.0

  var t6 = FooStruct6(x: 0, y: 0.0)
  t6.x = 0
  t6.y = 0.0
}


func testNoReturnStuff() {
  couldReturnFunction()  // not dead
  couldReturnFunction()  // not dead
  noreturnFunction()

  couldReturnFunction()  // dead
}

func testFunctionPointers() {
  let fp = getFunctionPointer()
  useFunctionPointer(fp)

  _ = fp as (@convention(c) (CInt) -> CInt)?

  let wrapper: FunctionPointerWrapper = FunctionPointerWrapper(a: nil, b: nil)
  _ = FunctionPointerWrapper(a: fp, b: fp)
  useFunctionPointer(wrapper.a)
  _ = wrapper.b as (@convention(c) (CInt) -> CInt)

  var anotherFP: @convention(c) (Int, CLong, UnsafeMutableRawPointer?) -> Void
    = getFunctionPointer2()

  var sizedFP: (@convention(c) (CInt, CInt, UnsafeMutableRawPointer?) -> Void)?

  useFunctionPointer2(anotherFP)
  sizedFP = fp // expected-error {{cannot assign value of type 'fptr?' (aka 'Optional<@convention(c) (Int32) -> Int32>') to type '(@convention(c) (CInt, CInt, UnsafeMutableRawPointer?) -> Void)?'}}
  // expected-note@-1 {{arguments to generic parameter 'Wrapped' ('fptr' (aka '@convention(c) (Int32) -> Int32') and '@convention(c) (CInt, CInt, UnsafeMutableRawPointer?) -> Void'}}
}

func testStructDefaultInit() {
  let _ = AnonStructs()
  let _ = ModRM()
  let _ = AnonUnion()
  let _ = GLKVector4()
}

func testArrays() {
  nonnullArrayParameters([], [], [])
  nonnullArrayParameters(nil, [], []) // expected-error {{'nil' is not compatible with expected argument type 'UnsafePointer<CChar>'}}
  nonnullArrayParameters([], nil, []) // expected-error {{'nil' is not compatible with expected argument type 'UnsafePointer<UnsafeMutableRawPointer?>'}}
  nonnullArrayParameters([], [], nil) // expected-error {{'nil' is not compatible with expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}

  nullableArrayParameters([], [], [])
  nullableArrayParameters(nil, nil, nil)

  // It would also be nice to warn here about the arrays being too short, but
  // that's probably beyond us for a while.
  staticBoundsArray([])
  staticBoundsArray(nil) // expected-error {{'nil' is not compatible with expected argument type 'UnsafePointer<CChar>'}}
}

func testVaList() {
  withVaList([]) {
    hasVaList($0) // okay
  }
  hasVaList(nil) // expected-error {{'nil' is not compatible with expected argument type 'CVaListPointer'}}
}

func testNestedForwardDeclaredStructs() {
  // Check that we still have a memberwise initializer despite the forward-
  // declared nested type. rdar://problem/30449400
  _ = StructWithForwardDeclaredStruct(ptr: nil)
}

protocol HasOptionalPointer {
  var ptr: OpaquePointer? { get }
  // expected-note@-1 {{protocol requires property 'ptr' with type 'OpaquePointer?'}}
}

extension StructWithForwardDeclaredStruct: HasOptionalPointer {}
// expected-error@-1 {{type 'StructWithForwardDeclaredStruct' does not conform to protocol 'HasOptionalPointer'}}
// expected-note@-2 {{add stubs for conformance}}

typealias IntTuple4096 = (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)

@available(anyAppleOS 26, *)
typealias IntArray4096 = [4096 of Int8]

@available(anyAppleOS 26, *)
func testBigArrayInStruct(_ maxSizeTuple: IntTuple4096, _ maxSizeArray: IntArray4096) {
  var structWithBigArray = StructWithBigArray()
  structWithBigArray.max_size = maxSizeTuple    // expected-modern-c-array-error {{cannot assign value of type 'IntTuple4096' (aka '(Int8 /* ... repeated 4096 times ... */)') to type '[4096 of CChar]' (aka 'InlineArray<4096, Int8>')}}
  structWithBigArray.max_size = maxSizeArray    // expected-legacy-c-array-error {{cannot assign value of type 'IntArray4096' (aka 'InlineArray<4096, Int8>') to type '(CChar /* ... repeated 4096 times ... */)' (aka '(Int8 /* ... repeated 4096 times ... */)')}}
  _ = structWithBigArray.max_size_plus_one      // expected-legacy-c-array-error {{internal}}
}

// Test the initializers and properties available in structs and unions with
// various combinations of ordinary members, C array members small enough to
// have a legacy projection, and large C array members with only a modern
// projection. Some lines are only expected to work in legacy mode, others only
// in modern mode.

@available(anyAppleOS 26, *)
func testStructWithPlainAndArrayFields(
    smallTuple: (Int32, Int32, Int32, Int32), smallArray: InlineArray<4, Int32>,
    s: StructWithPlainAndArrayFields
) {
  _ = StructWithPlainAndArrayFields(plain: 1, small: smallTuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type '(Int32, Int32, Int32, Int32)' to expected argument type '[4 of CInt]' (aka 'InlineArray<4, Int32>')}}

  _ = StructWithPlainAndArrayFields(plain: 1, small: smallArray)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'InlineArray<4, Int32>' to expected argument type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')}}

  let _: Int32 = s.plain

  let _: (Int32, Int32, Int32, Int32) = s.small
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}

  let _: InlineArray<4, Int32> = s.small
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}
}

@available(anyAppleOS 26, *)
func testStructWithSmallAndHugeArrayFields(
    smallTuple: (Int32, Int32, Int32, Int32), smallArray: InlineArray<4, Int32>,
    huge: InlineArray<5000, Int32>, s: StructWithSmallAndHugeArrayFields
) {
  _ = StructWithSmallAndHugeArrayFields(small: smallTuple)
  // expected-modern-c-array-error@-1 {{initializer expects 2 separate arguments}}
  // expected-modern-c-array-error@-2 {{cannot convert value of type '(Int32, Int32, Int32, Int32)' to expected argument type '[4 of CInt]' (aka 'InlineArray<4, Int32>')}}

  _ = StructWithSmallAndHugeArrayFields(small: smallArray, huge: huge)
  // expected-legacy-c-array-error@-1 {{extra argument 'huge' in call}}
  // expected-legacy-c-array-error@-2 {{cannot convert value of type 'InlineArray<4, Int32>' to expected argument type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')}}

  let _: (Int32, Int32, Int32, Int32) = s.small
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}

  let _: InlineArray<4, Int32> = s.small
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}

  let _: InlineArray<5000, Int32> = s.huge
  // expected-legacy-c-array-error@-1 {{'huge' is inaccessible due to 'internal' protection level}}
}

@available(anyAppleOS 26, *)
func testStructWithAllHugeArrayFields(
    huge1: InlineArray<5000, Int32>, huge2: InlineArray<6000, Int32>,
    s: StructWithAllHugeArrayFields
) {
  _ = StructWithAllHugeArrayFields(huge1: huge1, huge2: huge2)
  // expected-legacy-c-array-error@-1 {{argument passed to call that takes no arguments}}

  _ = StructWithAllHugeArrayFields()

  let _: InlineArray<5000, Int32> = s.huge1
  // expected-legacy-c-array-error@-1 {{'huge1' is inaccessible due to 'internal' protection level}}

  let _: InlineArray<6000, Int32> = s.huge2
  // expected-legacy-c-array-error@-1 {{'huge2' is inaccessible due to 'internal' protection level}}
}

@available(anyAppleOS 26, *)
func testUnionWithSmallAndHugeArrayFields(
    smallTuple: (Int32, Int32, Int32, Int32), smallArray: InlineArray<4, Int32>,
    huge: InlineArray<5000, Int32>, u: UnionWithSmallAndHugeArrayFields
) {
  _ = UnionWithSmallAndHugeArrayFields(small: smallTuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type '(Int32, Int32, Int32, Int32)' to expected argument type '[4 of CInt]' (aka 'InlineArray<4, Int32>')}}

  _ = UnionWithSmallAndHugeArrayFields(small: smallArray)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'InlineArray<4, Int32>' to expected argument type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')}}

  _ = UnionWithSmallAndHugeArrayFields(huge: huge)
  // expected-legacy-c-array-error@-1 {{argument passed to call that takes no arguments}}

  let _: (Int32, Int32, Int32, Int32) = u.small
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}

  let _: InlineArray<4, Int32> = u.small
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}

  let _: InlineArray<5000, Int32> = u.huge
  // expected-legacy-c-array-error@-1 {{'huge' is inaccessible due to 'internal' protection level}}
}

@available(anyAppleOS 26, *)
func testStructWithNestedArrayField(
    tupleTuple: ((Int32, Int32), (Int32, Int32)), arrArr: InlineArray<2, InlineArray<2, Int32>>,
    s: StructWithNestedArrayField
) {
  _ = StructWithNestedArrayField(elems: tupleTuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type '((Int32, Int32), (Int32, Int32))' to expected argument type '[2 of [2 of CInt]]' (aka 'InlineArray<2, InlineArray<2, Int32>>')}}

  _ = StructWithNestedArrayField(elems: arrArr)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'InlineArray<2, InlineArray<2, Int32>>' to expected argument type '((CInt, CInt), (CInt, CInt))' (aka '((Int32, Int32), (Int32, Int32))')}}

  let _: ((Int32, Int32), (Int32, Int32)) = s.elems
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[2 of [2 of CInt]]' (aka 'InlineArray<2, InlineArray<2, Int32>>') to specified type '((Int32, Int32), (Int32, Int32))'}}

  let _: InlineArray<2, InlineArray<2, Int32>> = s.elems
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '((CInt, CInt), (CInt, CInt))' (aka '((Int32, Int32), (Int32, Int32))') to specified type 'InlineArray<2, InlineArray<2, Int32>>'}}
}

@available(anyAppleOS 26, *)
func testStructWithArrayTypedefField(
    smallTuple: (Int32, Int32, Int32, Int32), smallArray: InlineArray<4, Int32>,
    s: StructWithArrayTypedefField
) {
  _ = StructWithArrayTypedefField(small: smallTuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type '(Int32, Int32, Int32, Int32)' to expected argument type 'SmallArrayTypedef' (aka 'InlineArray<4, Int32>')}}

  _ = StructWithArrayTypedefField(small: smallArray)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'InlineArray<4, Int32>' to expected argument type 'SmallArrayTypedef' (aka '(Int32, Int32, Int32, Int32)')}}

  let _: (Int32, Int32, Int32, Int32) = s.small
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'SmallArrayTypedef' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}

  let _: InlineArray<4, Int32> = s.small
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'SmallArrayTypedef' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}
}

@available(anyAppleOS 26, *)
func testStructWithHugeArrayTypedefField(s: StructWithHugeArrayTypedefField) {
  let _: InlineArray<5000, Int32> = s.huge
  // expected-legacy-c-array-error@-1 {{'huge' is inaccessible due to 'internal' protection level}}
}

// Check that a struct field whose type is a `swift_newtype` doesn't have a
// legacy/modern distinction of its own (only the newtype's `rawValue` does),
// so it works the same way in both modes.

@available(anyAppleOS 26, *)
func testStructWithSmallArrayNewtypeField(
    small: SmallArrayNewtype, s: StructWithSmallArrayNewtypeField
) {
  _ = StructWithSmallArrayNewtypeField(small: small)
  let _: SmallArrayNewtype = s.small
}

@available(anyAppleOS 26, *)
func testStructWithHugeArrayNewtypeField(s: StructWithHugeArrayNewtypeField) {
  // `HugeArrayNewtype`'s raw value can't be imported in legacy mode, so both
  // the property and the type itself are marked as modern projections and are
  // not available in legacy mode.
  let _: HugeArrayNewtype = s.huge
  // expected-legacy-c-array-error@-1 {{cannot find type 'HugeArrayNewtype' in scope}}
  // expected-legacy-c-array-error@-2 {{'huge' is inaccessible due to 'internal' protection level}}

  let _: InlineArray<9000, Int32> = s.huge.rawValue
  // expected-legacy-c-array-error@-1 {{'huge' is inaccessible due to 'internal' protection level}}
}

@available(anyAppleOS 26, *)
func testStructWithStructArrayFields(
    smallTuple: (FooStruct1, FooStruct1, FooStruct1, FooStruct1), smallArray: InlineArray<4, FooStruct1>,
    huge: InlineArray<5000, FooStruct1>, s: StructWithStructArrayFields
) {
  _ = StructWithStructArrayFields(small: smallTuple)
  // expected-modern-c-array-error@-1 {{initializer expects 2 separate arguments}}
  // expected-modern-c-array-error@-2 {{cannot convert value of type '(FooStruct1, FooStruct1, FooStruct1, FooStruct1)' to expected argument type '[4 of FooStruct1]'}}

  _ = StructWithStructArrayFields(small: smallArray, huge: huge)
  // expected-legacy-c-array-error@-1 {{extra argument 'huge' in call}}
  // expected-legacy-c-array-error@-2 {{cannot convert value of type 'InlineArray<4, FooStruct1>' to expected argument type '(FooStruct1, FooStruct1, FooStruct1, FooStruct1)'}}

  let _: (FooStruct1, FooStruct1, FooStruct1, FooStruct1) = s.small
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of FooStruct1]' to specified type '(FooStruct1, FooStruct1, FooStruct1, FooStruct1)'}}

  let _: InlineArray<4, FooStruct1> = s.small
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(FooStruct1, FooStruct1, FooStruct1, FooStruct1)' to specified type 'InlineArray<4, FooStruct1>'}}

  let _: InlineArray<5000, FooStruct1> = s.huge
  // expected-legacy-c-array-error@-1 {{'huge' is inaccessible due to 'internal' protection level}}
}

@available(anyAppleOS 26, *)
func testSmallArrayNewtype(small: SmallArrayNewtype) {
  let _: (Int32, Int32, Int32, Int32) = small.rawValue
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}

  let _: InlineArray<4, Int32> = small.rawValue
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}

  _ = SmallArrayNewtype(rawValue: (0, 0, 0, 0))
  // expected-modern-c-array-error@-1 {{cannot convert value of type '(Int, Int, Int, Int)' to expected argument type '[4 of CInt]' (aka 'InlineArray<4, Int32>')}}

  _ = SmallArrayNewtype(rawValue: [0, 0, 0, 0])
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '[Int]' to expected argument type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')}}
}

@available(anyAppleOS 26, *)
func testHugeArrayNewtype(huge: HugeArrayNewtype) {
  // expected-legacy-c-array-error@-1 {{cannot find type 'HugeArrayNewtype' in scope}}
  // expected-legacy-c-array-note@-2 {{did you mean 'testHugeArrayNewtype'?}}

  let _: InlineArray<9000, Int32> = huge.rawValue

  _ = SmallArrayNewtype(rawValue: InlineArray<9000, Int32>(repeating: 0))
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'InlineArray<9000, Int32>' to expected argument type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')}}
  // expected-modern-c-array-error@-2 {{cannot convert value of type 'InlineArray<9000, Int32>' to expected argument type '[4 of CInt]' (aka 'InlineArray<4, Int32>')}}
  // expected-modern-c-array-note@-3 {{arguments to generic parameter 'count' ('9000' and '4') are expected to be equal}}
}

// Check that a global array too large to have a legacy projection is simply
// not visible in legacy mode, rather than being invalid or causing a crash.

@available(anyAppleOS 26, *)
func testHugeGlobalArray() {
  let _: InlineArray<5000, Int32> = hugeGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot find 'hugeGlobalArray' in scope}}
  // expected-legacy-c-array-note@-3 {{did you mean 'testHugeGlobalArray'?}}
}

@available(anyAppleOS 26, *)
func testSmallGlobalArray() {
  let _: InlineArray<4, Int32> = smallGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}

  let _: (Int32, Int32, Int32, Int32) = smallGlobalArray
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}
}

@available(anyAppleOS 26, *)
func testSmallNestedGlobalArray() {
  let _: InlineArray<2, InlineArray<2, Int32>> = smallNestedGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '((CInt, CInt), (CInt, CInt))' (aka '((Int32, Int32), (Int32, Int32))') to specified type 'InlineArray<2, InlineArray<2, Int32>>'}}

  let _: ((Int32, Int32), (Int32, Int32)) = smallNestedGlobalArray
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[2 of [2 of CInt]]' (aka 'InlineArray<2, InlineArray<2, Int32>>') to specified type '((Int32, Int32), (Int32, Int32))'}}
}

@available(anyAppleOS 26, *)
func testHugeNestedGlobalArray() {
  let _: InlineArray<2, InlineArray<5000, Int32>> = hugeNestedGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot find 'hugeNestedGlobalArray' in scope}}
  // expected-legacy-c-array-note@-3 {{did you mean 'testHugeNestedGlobalArray'?}}
}

@available(anyAppleOS 26, *)
func testHugeOuterNestedGlobalArray() {
  let _: InlineArray<5000, InlineArray<2, Int32>> = hugeOuterNestedGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot find 'hugeOuterNestedGlobalArray' in scope}}
  // expected-legacy-c-array-note@-3 {{'testHugeOuterNestedGlobalArray' declared here}}
}

@available(anyAppleOS 26, *)
func testHugeTypedefGlobalArray() {
  let _: InlineArray<5000, Int32> = hugeTypedefGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot find 'hugeTypedefGlobalArray' in scope}}
  // expected-legacy-c-array-note@-3 {{did you mean 'testHugeTypedefGlobalArray'?}}
}

@available(anyAppleOS 26, *)
func testSmallTypedefGlobalArray() {
  let _: InlineArray<4, Int32> = smallTypedefGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'SmallArrayTypedef' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}

  let _: (Int32, Int32, Int32, Int32) = smallTypedefGlobalArray
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'SmallArrayTypedef' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}
}

@available(anyAppleOS 26, *)
func testSmallNewtypeGlobalArray() {
  let _: SmallArrayNewtype = SmallArrayNewtype.smallNewtypeGlobalArray

  let _: SmallArrayNewtype = smallNewtypeGlobalArray
  // expected-error@-1 {{'smallNewtypeGlobalArray' has been renamed to 'SmallArrayNewtype.smallNewtypeGlobalArray'}}
}

@available(anyAppleOS 26, *)
func testHugeNewtypeGlobalArray() {
  // expected-legacy-c-array-note@-1 {{did you mean 'testHugeNewtypeGlobalArray'?}}

  // Expected for `HugeArrayNewtype` to be invisible in legacy mode--it's
  // unimportable.

  let _: HugeArrayNewtype = HugeArrayNewtype.hugeNewtypeGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot find type 'HugeArrayNewtype' in scope}}
  // expected-legacy-c-array-error@-2 {{cannot find 'HugeArrayNewtype' in scope}}

  let _: HugeArrayNewtype = hugeNewtypeGlobalArray
  // expected-legacy-c-array-error@-1 {{cannot find type 'HugeArrayNewtype' in scope}}
  // expected-legacy-c-array-error@-2 {{cannot find 'hugeNewtypeGlobalArray' in scope}}
  // expected-modern-c-array-error@-3 {{'hugeNewtypeGlobalArray' has been renamed to 'HugeArrayNewtype.hugeNewtypeGlobalArray'}}
}

@available(anyAppleOS 26, *)
func testPointerToHugeGlobalArray() {
  // FIXME: Type checker emits diag::failed_to_produce_diagnostic unless these types are Optional

  let _: UnsafeMutablePointer<InlineArray<5000, Int32>>? = globalPointerToHugeArray
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'OpaquePointer?' to type 'UnsafeMutablePointer<InlineArray<5000, Int32>>?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Wrapped' ('OpaquePointer' and 'UnsafeMutablePointer<InlineArray<5000, Int32>>') are expected to be equal}}

  let _: OpaquePointer? = globalPointerToHugeArray
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[5000 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<5000, Int32>>>') to type 'OpaquePointer?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Wrapped' ('UnsafeMutablePointer<[5000 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<5000, Int32>>') and 'OpaquePointer') are expected to be equal}}
}

@available(anyAppleOS 26, *)
func testFunctionPointerWithHugeArrayParam() {
  let _: ((UnsafeMutablePointer<InlineArray<5000, Int32>>?) -> Void)? = globalFunctionPointerWithHugeArrayParam
  // expected-legacy-c-array-error@-1 {{cannot assign value of type '(@convention(c) (OpaquePointer?) -> Void)?' to type '((UnsafeMutablePointer<InlineArray<5000, Int32>>?) -> Void)?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Wrapped' ('@convention(c) (OpaquePointer?) -> Void' and '(UnsafeMutablePointer<InlineArray<5000, Int32>>?) -> Void') are expected to be equal}}

  let _: ((OpaquePointer?) -> Void)? = globalFunctionPointerWithHugeArrayParam
  // expected-modern-c-array-error@-1 {{cannot assign value of type '(@convention(c) (UnsafeMutablePointer<[5000 of CInt]>?) -> Void)?' (aka 'Optional<@convention(c) (Optional<UnsafeMutablePointer<InlineArray<5000, Int32>>>) -> ()>') to type '((OpaquePointer?) -> Void)?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Wrapped' ('@convention(c) (UnsafeMutablePointer<[5000 of CInt]>?) -> Void' (aka '@convention(c) (Optional<UnsafeMutablePointer<InlineArray<5000, Int32>>>) -> ()') and '(OpaquePointer?) -> Void') are expected to be equal}}
}
