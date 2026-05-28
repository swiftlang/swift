// Test that a Client importing a Library with -internal-import-bridging-header
// can allocate, copy, and destroy values whose stored properties are hidden.

// REQUIRES: swift_feature_AbstractStoredPropertyLayout
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-module -module-name Library \
// RUN:   -parse-as-library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-ir -module-name Client \
// RUN:   -parse-as-library \
// RUN:   -I %t \
// RUN:   %t/Client.swift | %FileCheck --check-prefix=IR %s

//--- Utility.h
typedef struct {
  int value;
} Wrapper;

typedef struct {
  double d;
} BigWrapper;

// char + long long forces 8-byte alignment the client must honor.
typedef struct {
  char c;
  long long v;
} MixWrapper;

//--- Library.swift
public struct S {
  private var w: Wrapper
  public init(value: Int32) {
    w = Wrapper(value: value)
  }
}

// Two hidden fields: size=16, alignment=8.
public struct S2 {
  private var a: Wrapper
  private var b: BigWrapper
  public init() {
    a = Wrapper(value: 0)
    b = BigWrapper(d: 0.0)
  }
}

// Hidden and visible fields interleaved: size=32, alignment=8.
public struct S3 {
  private var w: Wrapper
  public var x: Int
  private var w2: Wrapper
  public var y: Double
  public init(value: Int32) {
    w = Wrapper(value: value)
    x = 0
    w2 = Wrapper(value: value)
    y = 0.0
  }
}

// Hidden field with non-obvious 8-byte alignment (from long long).
public struct S4 {
  private var m: MixWrapper
  public init() {
    m = MixWrapper(c: 0, v: 0)
  }
}

//--- Client.swift
import Library

// IR: %T7Library1SV = type <{ [4 x i8] }>

// IR-LABEL: define {{.*}} @"$s6Client3useyS2iF"
// IR: alloca %T7Library1SV, align 4
// IR: alloca %T7Library1SV, align 4
// IR: call void @llvm.memcpy.{{.*}}(ptr {{[^,]+}}, ptr {{[^,]+}}, i64 4, i1 false)
public func use(_ value: Int) -> Int {
  let s = S(value: Int32(value))
  var s2 = s
  s2 = s
  _ = s2
  return MemoryLayout<S>.size
}

// IR-LABEL: define {{.*}}useS2
// IR: alloca %T7Library2S2V, align 8
// IR: call void @llvm.memset.{{.*}}(ptr align 8 {{[^,]+}}, i8 0, i64 16, i1 false)
// IR: call {{.*}}@"$s7Library2S2VWOc"
public func useS2() -> Int {
  let s = S2()
  var s2 = s
  s2 = s
  _ = s2
  return MemoryLayout<S2>.size
}

// IR-LABEL: define {{.*}}useS3
// IR: alloca %T7Library2S3V, align 8
// IR: call void @llvm.memset.{{.*}}(ptr align 8 {{[^,]+}}, i8 0, i64 32, i1 false)
// IR: call {{.*}}@"$s7Library2S3VWOc"
public func useS3() -> Int {
  let s = S3(value: 1)
  var s2 = s
  s2 = s
  _ = s2
  return MemoryLayout<S3>.size
}

// IR-LABEL: define {{.*}}useS4
// IR: alloca %T7Library2S4V, align 8
// IR: call void @llvm.memset.{{.*}}(ptr align 8 {{[^,]+}}, i8 0, i64 16, i1 false)
// IR: call {{.*}}@"$s7Library2S4VWOc"
public func useS4() -> Int {
  let s = S4()
  var s2 = s
  s2 = s
  _ = s2
  return MemoryLayout<S4>.size
}

// Client IR must not reference the hidden C types.
// IR-NOT: Utility.h
// IR-NOT: SC7Wrapper
// IR-NOT: SC10BigWrapper
// IR-NOT: SC10MixWrapper
