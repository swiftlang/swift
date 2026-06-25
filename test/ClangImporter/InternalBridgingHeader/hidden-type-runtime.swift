// Runtime test: values of encapsulated types round-trip correctly across the
// dylib boundary, proving the layout-table-driven codegen is correct.

// REQUIRES: swift_feature_AbstractStoredPropertyLayout
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64

// UNSUPPORTED: remote_run || device_run

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build Library as a dylib. -wmo skips the merge-modules step (whose verifier
// can't round-trip HiddenType placeholders yet).
// RUN: %target-build-swift \
// RUN:   -Xfrontend -internal-import-bridging-header \
// RUN:   -Xfrontend %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -wmo -parse-as-library -emit-library \
// RUN:   -emit-module -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library \
// RUN:   %t/Library.swift \
// RUN:   -o %t/%target-library-name(Library)

// Build Client with -Onone to prevent the optimizer from folding away copies.
// RUN: %target-build-swift -Onone \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -I %t -L %t -lLibrary \
// RUN:   %target-rpath(%t) \
// RUN:   -o %t/main %t/Client.swift
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main | %FileCheck %s

//--- Utility.h
typedef struct {
  int value;
} Wrapper;

typedef struct {
  double d;
} BigWrapper;

//--- Library.swift
public struct S {
  private var w: Wrapper
  public init(value: Int32) {
    w = Wrapper(value: value)
  }
  public func payload() -> Int32 {
    return w.value
  }
}

// Multiple hidden fields.
public struct S2 {
  private var a: Wrapper
  private var b: BigWrapper
  public init(a: Int32, b: Double) {
    self.a = Wrapper(value: a)
    self.b = BigWrapper(d: b)
  }
  public func payloadA() -> Int32 { return a.value }
  public func payloadB() -> Double { return b.d }
}

// Hidden and visible fields interleaved.
public struct S3 {
  private var w: Wrapper
  public var x: Int
  public init(value: Int32, x: Int) {
    self.w = Wrapper(value: value)
    self.x = x
  }
  public func hiddenValue() -> Int32 { return w.value }
}

//--- Client.swift
import Library

@inline(never)
func roundTrip() -> Int32 {
  let s = S(value: 42)
  var s2 = s
  s2 = S(value: 99)
  s2 = s
  return s2.payload()
}

@inline(never)
func roundTripS2() -> (Int32, Double) {
  let s = S2(a: 42, b: 2.5)
  var s2 = s
  s2 = S2(a: 99, b: 99.0)
  s2 = s
  return (s2.payloadA(), s2.payloadB())
}

@inline(never)
func roundTripS3() -> (Int32, Int) {
  let s = S3(value: 42, x: 100)
  var s2 = s
  s2 = S3(value: 99, x: 200)
  s2 = s
  return (s2.hiddenValue(), s2.x)
}

@inline(never)
func roundTripOptional() -> Int32 {
  var opt: S? = S(value: 42)
  opt = nil
  opt = S(value: 77)
  return opt!.payload()
}

// CHECK: size=4 alignment=4 stride=4
print("size=\(MemoryLayout<S>.size) "
    + "alignment=\(MemoryLayout<S>.alignment) "
    + "stride=\(MemoryLayout<S>.stride)")

// CHECK-NEXT: payload=42
print("payload=\(roundTrip())")

// CHECK-NEXT: s2 a=42 b=2.5
let (a, b) = roundTripS2()
print("s2 a=\(a) b=\(b)")

// CHECK-NEXT: s3 w=42 x=100
let (w, x) = roundTripS3()
print("s3 w=\(w) x=\(x)")

// CHECK-NEXT: optional=77
print("optional=\(roundTripOptional())")
