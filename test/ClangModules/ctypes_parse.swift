// RUN: %target-parse-verify-swift %clang-importer-sdk

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import ctypes
import CoreGraphics
import Foundation
import CoreFoundation

var cgPointVar: CGPoint

func checkRawRepresentable<T: RawRepresentable>(_: T) {}

func testColor() {
  var c: Color = red
  c = blue
  _ = c.rawValue
  checkRawRepresentable(c)
}

func testTribool() {
  var b = Indeterminate
  b = True
  _ = b.rawValue
}

func testAnonEnum() {
  var a = AnonConst1
  a = AnonConst2
#if arch(i386) || arch(arm)
  _ = a as CUnsignedLongLong
#elseif arch(x86_64) || arch(arm64)
  _ = a as CUnsignedLong
#else
  __portMe()
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

func testBitfieldMembers() -> StructWithBitfields {
  return StructWithBitfields()
  // TODO: Expose the bitfields as properties.
}

// FIXME: Import arrays as real array-looking things.

func testArrays() {
  let fes: NSFastEnumerationState
  var ulong: CUnsignedLong
  var pulong: UnsafeMutablePointer<CUnsignedLong>

  ulong = fes.state
  pulong = fes.mutationsPtr
  ulong = fes.extra.0
  ulong = fes.extra.1
  ulong = fes.extra.2
  ulong = fes.extra.3
  ulong = fes.extra.4
  _ = ulong; _ = pulong
}

// FIXME: Import pointers to opaque types as unique types.

func testPointers() {
  _ = nil as HWND
  let cfstr: CFString? = nil
  _ = cfstr as CFTypeRef?
}

// Ensure that imported structs can be extended, even if typedef'ed on the C
// side.

func sqrt(x: Float) -> Float {}
func atan2(x: Float, _ y: Float) -> Float {}

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

extension NSFastEnumerationState {
  func reset() {}
}

extension CGRectTy {
  init(x: Double, y: Double, w: Double, h: Double) {
    origin.x = CGFloat(x)
    origin.y = CGFloat(y)
    size.width = CGFloat(w)
    size.height = CGFloat(h)
  }
}

extension CGRect {
  func printAsX11Geometry() {
    print("\(size.width)x\(size.height)+\(origin.x)+\(origin.y)", appendNewline: false)
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
  var x: MyVoid // expected-error{{use of undeclared type 'MyVoid'}}
  returnsMyVoid()
}

func testImportMacTypes() {
  // Test that we import integer and floating-point types as swift stdlib
  // types.
  var a : UInt32 = UInt32_test
  a = a + 1

  var b : Float64 = Float64_test
  b = b + 1

  var t9_unqual : Float32 = Float32_test
  var t10_unqual : Float64 = Float64_test

  var t9_qual : ctypes.Float32 = 0.0  // expected-error {{no type named 'Float32' in module 'ctypes'}}
  var t10_qual : ctypes.Float64 = 0.0 // expected-error {{no type named 'Float64' in module 'ctypes'}}
}

var word: Word = 0
var uword: UWord = 0

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

func testImportCFTypes() {
  let t1_unqual: Int = CFIndex_test
  _ = t1_unqual as CoreFoundation.CFIndex
}

func testImportOSTypesTypes() {
  var t1_unqual: CInt = SInt_test
  var t2_unqual: CUnsignedInt = UInt_test

  var t1_qual: ctypes.SInt = t1_unqual // expected-error {{no type named 'SInt' in module 'ctypes'}}
  var t2_qual: ctypes.UInt = t2_unqual // expected-error {{no type named 'UInt' in module 'ctypes'}}
}

func testImportSEL() {
  var t1 : SEL // expected-error {{use of undeclared type 'SEL'}}
  var t2 : ctypes.SEL // expected-error {{no type named 'SEL' in module 'ctypes'}}
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

  _ = fp as (@convention(c) (CInt) -> CInt)

  let wrapper: FunctionPointerWrapper = FunctionPointerWrapper(a: nil, b: nil)
  _ = FunctionPointerWrapper(a: fp, b: fp)
  useFunctionPointer(wrapper.a)
  _ = wrapper.b as (@convention(c) (CInt) -> CInt)

  var anotherFP: @convention(c) (CInt, CLong, UnsafeMutablePointer<Void>) -> Void
    = getFunctionPointer2()

  useFunctionPointer2(anotherFP)
  anotherFP = fp // expected-error {{cannot assign a value of type 'fptr!' to a value of type '@convention(c) (CInt, CLong, UnsafeMutablePointer<Void>) -> Void'}}
}

func testStructDefaultInit() {
  let a_s = AnonStructs()
  let modrm = ModRM()
  let union = AnonUnion()
  let v4 = GLKVector4()

  let nonNilable = NonNilableReferences() // expected-error{{missing argument}}
}
