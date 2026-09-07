// Verifies that each `@cxx @implementation` function body is emitted under the
// matched C++ declaration's mangled symbol, and that Swift-side calls target
// the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -Xcc -Wno-nullability-completeness \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import Functions


// Primitives

// void takesPrimitives(int i, long l, char c, float f, double d, bool b);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z15takesPrimitivesilcfdb
// CHECK-WIN-LABEL: define{{.*}} void @"?takesPrimitives@@YAXHJDMN_N@Z"
@cxx @implementation
public func takesPrimitives(_ i: CInt, _ l: CLong, _ c: CChar, _ f: CFloat, _ d: CDouble, _ b: CBool) {}

// int returnsInt();
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z10returnsIntv
// CHECK-WIN-LABEL: define{{.*}} i32 @"?returnsInt@@YAHXZ"
@cxx @implementation
public func returnsInt() -> CInt { return 42 }

// int64_t takesInt64(int64_t x);
// int64_t is long on LP64 Linux but long long elsewhere, so allow either
// Itanium mangling; MSVC spells both _J.
// CHECK-SYSV-LABEL: define{{.*}} i64 @_Z10takesInt64{{[lx]}}
// CHECK-WIN-LABEL: define{{.*}} i64 @"?takesInt64@@YA_J_J@Z"
@cxx @implementation
public func takesInt64(_ x: Int64) -> Int64 { return x }


// Pointers

// void takesPtrToInt(int *p);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z13takesPtrToIntPi
// CHECK-WIN-LABEL: define{{.*}} void @"?takesPtrToInt@@YAXPEAH@Z"
@cxx @implementation
public func takesPtrToInt(_ p: UnsafeMutablePointer<CInt>?) {}

// void takesPtrToConstInt(const int *p);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z18takesPtrToConstIntPKi
// CHECK-WIN-LABEL: define{{.*}} void @"?takesPtrToConstInt@@YAXPEBH@Z"
@cxx @implementation
public func takesPtrToConstInt(_ p: UnsafePointer<CInt>?) {}

// void takesPtrToVoid(void *p);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z14takesPtrToVoidPv
// CHECK-WIN-LABEL: define{{.*}} void @"?takesPtrToVoid@@YAXPEAX@Z"
@cxx @implementation
public func takesPtrToVoid(_ p: UnsafeMutableRawPointer?) {}

// void takesFuncPtr(int (*fn)(int));
// CHECK-SYSV-LABEL: define{{.*}} void @_Z12takesFuncPtrPFiiE
// CHECK-WIN-LABEL: define{{.*}} void @"?takesFuncPtr@@YAXP6AHH@Z@Z"
@cxx @implementation
public func takesFuncPtr(_ fn: (@convention(c) (CInt) -> CInt)?) {}

// int *returnsPtrToInt();
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z15returnsPtrToIntv
// CHECK-WIN-LABEL: define{{.*}} ptr @"?returnsPtrToInt@@YAPEAHXZ"
@cxx @implementation
public func returnsPtrToInt() -> UnsafeMutablePointer<CInt>? { return nil }

// void takesNonnullPtrToInt(int *_Nonnull p);
// The nullability annotation changes the imported Swift type (non-optional
// pointer) but not the mangled symbol.
// CHECK-SYSV-LABEL: define{{.*}} void @_Z20takesNonnullPtrToIntPi
// CHECK-WIN-LABEL: define{{.*}} void @"?takesNonnullPtrToInt@@YAXPEAH@Z"
@cxx @implementation
public func takesNonnullPtrToInt(_ p: UnsafeMutablePointer<CInt>) {}

// void swapInts(int *p, int *q);
// Two pointer parameters of the same type exercise substitution compression
// in the mangled name: Itanium's S_ and MSVC's 0 back-reference.
// CHECK-SYSV-LABEL: define{{.*}} void @_Z8swapIntsPiS_
// CHECK-WIN-LABEL: define{{.*}} void @"?swapInts@@YAXPEAH0@Z"
@cxx @implementation
public func swapInts(_ p: UnsafeMutablePointer<CInt>?, _ q: UnsafeMutablePointer<CInt>?) {
  let tmp = p!.pointee
  p!.pointee = q!.pointee
  q!.pointee = tmp
}


// Trivial struct

// void takesTrivialStruct(TrivialStruct s);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z18takesTrivialStruct13TrivialStruct
// CHECK-WIN-LABEL: define{{.*}} void @"?takesTrivialStruct@@YAXUTrivialStruct@@@Z"
@cxx @implementation
public func takesTrivialStruct(_ s: TrivialStruct) {}

// TrivialStruct returnsTrivialStruct();
// The struct-return ABI differs by target (direct vs. sret), so don't pin the
// return type.
// CHECK-SYSV-LABEL: define{{.*}} @_Z20returnsTrivialStructv
// CHECK-WIN-LABEL: define{{.*}} @"?returnsTrivialStruct@@YA?AUTrivialStruct@@XZ"
@cxx @implementation
public func returnsTrivialStruct() -> TrivialStruct {
  return TrivialStruct(x: 1, y: 2)
}


// Overloads

// int overloadedByArity(int x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z17overloadedByArityi
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArity@@YAHH@Z"
@cxx @implementation
public func overloadedByArity(_ x: CInt) -> CInt { return x }

// int overloadedByArity(int x, int y);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z17overloadedByArityii
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArity@@YAHHH@Z"
@cxx @implementation
public func overloadedByArity(_ x: CInt, _ y: CInt) -> CInt { return x + y }


// Default arg

// int withDefaultArg(int a, int b = 10);
// C++ default arguments are substituted at C++ call sites and don't affect the
// definition. The implementation matches the full two-parameter signature.
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z14withDefaultArgii
// CHECK-WIN-LABEL: define{{.*}} i32 @"?withDefaultArg@@YAHHH@Z"
@cxx @implementation
public func withDefaultArg(_ a: CInt, _ b: CInt) -> CInt { return a + b }


// Linkage

// extern "C" int externCFunc(int x);
// We need to respect the C language linkage of a function declared in a C++
// header, so the implementation is emitted under the unmangled name.
// CHECK-LABEL: define{{.*}} i32 @externCFunc(
@cxx @implementation
public func externCFunc(_ x: CInt) -> CInt { return x }


// Renamed function

// int foo(int x);
// `@cxx(foo)` matches `int foo(int)` by that C++ name even though the Swift
// function is named differently. The body is still emitted under the matched
// declaration's mangled symbol.
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z3fooi
// CHECK-WIN-LABEL: define{{.*}} i32 @"?foo@@YAHH@Z"
@cxx(foo) @implementation
public func swiftRenamedFoo(_ x: CInt) -> CInt { return x }


// Swift-side calls

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}12callCxxFuncsyyF"
// CHECK-SYSV:   invoke void @_Z15takesPrimitivesilcfdb
// CHECK-SYSV:   invoke i32 @_Z10returnsIntv
// CHECK-SYSV:   invoke i64 @_Z10takesInt64{{[lx]}}
// CHECK-SYSV:   invoke void @_Z13takesPtrToIntPi
// CHECK-SYSV:   invoke void @_Z18takesPtrToConstIntPKi
// CHECK-SYSV:   invoke void @_Z14takesPtrToVoidPv
// CHECK-SYSV:   invoke void @_Z12takesFuncPtrPFiiE
// CHECK-SYSV:   invoke ptr @_Z15returnsPtrToIntv
// CHECK-SYSV:   invoke void @_Z20takesNonnullPtrToIntPi
// CHECK-SYSV:   invoke void @_Z8swapIntsPiS_
// CHECK-SYSV:   invoke void @_Z18takesTrivialStruct13TrivialStruct
// CHECK-SYSV:   invoke {{.*}} @_Z20returnsTrivialStructv
// CHECK-SYSV:   invoke i32 @_Z17overloadedByArityi
// CHECK-SYSV:   invoke i32 @_Z17overloadedByArityii
// CHECK-SYSV:   invoke i32 @_Z14withDefaultArgii
// CHECK-SYSV:   invoke i32 @externCFunc
// CHECK-SYSV:   invoke i32 @_Z3fooi
public func callCxxFuncs() {
  takesPrimitives(1, 2, 3, 4, 5, true)
  _ = returnsInt()
  _ = takesInt64(42)

  var x: CInt = 42
  var y: CInt = 67
  takesPtrToInt(&x)
  takesPtrToConstInt(&x)
  takesPtrToVoid(&x)
  takesFuncPtr(foo)
  _ = returnsPtrToInt()
  takesNonnullPtrToInt(&x)
  swapInts(&x, &y)

  takesTrivialStruct(TrivialStruct(x: 1, y: 2))
  _ = returnsTrivialStruct()

  _ = overloadedByArity(42)
  _ = overloadedByArity(42, 67)

  _ = withDefaultArg(42, 67)

  _ = externCFunc(42)

  _ = foo(42)
}
