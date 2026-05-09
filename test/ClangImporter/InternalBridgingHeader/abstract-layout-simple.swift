// Test that a public struct can have a private stored property of a C type
// imported via an internal bridging header when AbstractStoredPropertyLayout is enabled.

// REQUIRES: swift_feature_AbstractStoredPropertyLayout

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-module -module-name Library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -typecheck -module-name Library \
// RUN:   -dump-abstract-layout \
// RUN:   %t/Library.swift | %FileCheck %s

// CHECK: S:
// CHECK:   size: 4
// CHECK:   alignment: 4
// CHECK:   stride: 4
// CHECK:   bitwiseCopyable: true
// CHECK:   isOpaque: false
// CHECK:   fields:
// CHECK:     w: $sSo7Wrappera, offset=0, size=4, isOpaque=false, bitwiseCopyable=true

// CHECK: S2:
// CHECK:   size: 16
// CHECK:   alignment: 8
// CHECK:   stride: 16
// CHECK:   bitwiseCopyable: true
// CHECK:   isOpaque: false
// CHECK:   fields:
// CHECK:     a: $sSo7Wrappera, offset=0, size=4, isOpaque=false, bitwiseCopyable=true
// CHECK:     b: $sSo10BigWrappera, offset=8, size=8, isOpaque=false, bitwiseCopyable=true

// CHECK: S3:
// CHECK:   size: 16
// CHECK:   alignment: 8
// CHECK:   stride: 16
// CHECK:   bitwiseCopyable: true
// CHECK:   isOpaque: false
// CHECK:   fields:
// CHECK:     w: $sSo7Wrappera, offset=0, size=4, isOpaque=false, bitwiseCopyable=true
// CHECK:     x: $sSi, offset=8, size=8, isOpaque=false, bitwiseCopyable=true

// CHECK: S4:
// CHECK:   size: 16
// CHECK:   alignment: 8
// CHECK:   stride: 16
// CHECK:   bitwiseCopyable: false
// CHECK:   isOpaque: false
// CHECK:   fields:
// CHECK:     s: $sSS, offset=0, size=16, isOpaque=false, bitwiseCopyable=false

//--- Utility.h

typedef struct { int value; } Wrapper;
typedef struct { double d; } BigWrapper;

//--- Library.swift

public struct S {
  private var w: Wrapper

  public init(value: Int32) {
    self.w = Wrapper(value: value)
  }

  public var storedValue: Int32 { w.value }
}

public struct S2 {
  private var a: Wrapper
  private var b: BigWrapper

  public init() {
    self.a = Wrapper(value: 0)
    self.b = BigWrapper(d: 0.0)
  }
}

public struct S3 {
  private var w: Wrapper
  public var x: Int

  public init(value: Int32, x: Int) {
    self.w = Wrapper(value: value)
    self.x = x
  }
}

public struct S4 {
  public var s: String

  public init(s: String) {
    self.s = s
  }
}
