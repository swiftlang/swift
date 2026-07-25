// Test that -dump-hidden-type-layouts prints hidden-type layouts serialized
// into the module being emitted.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-module -module-name Library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   -dump-hidden-type-layouts \
// RUN:   %t/Library.swift | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize %s

//--- Utility.h
typedef struct {
  int value;
} Wrapper;

typedef struct {
  long a;
  long b;
} BigWrapper;

//--- Library.swift
public struct S {
  private var w: Wrapper
  public init(value: Int32) {
    w = Wrapper(value: value)
  }
}

public struct S2 {
  private var a: Wrapper
  private var b: BigWrapper
  public init() {
    self.a = Wrapper(value: 0)
    self.b = BigWrapper(a: 0, b: 0)
  }
}

// CHECK: Module: Library
// CHECK-NEXT: $sSo7Wrappera: kind=LoadableTrivialHiddenType, mangledName=$sSo7Wrappera, size=4, alignment=4, stride=4, bitwiseCopyable=true, opaque=false, typeProperties=8704, nativeParameterRequiresIndirect=false, nativeResultRequiresIndirect=false, nativeComponents={{[0-9]+}}
// CHECK-64-NEXT: $sSo10BigWrappera: kind=LoadableTrivialHiddenType, mangledName=$sSo10BigWrappera, size=16, alignment=8, stride=16, bitwiseCopyable=true, opaque=false, typeProperties=8704, nativeParameterRequiresIndirect=false, nativeResultRequiresIndirect=false, nativeComponents={{[0-9]+}}
// CHECK-32-NEXT: $sSo10BigWrappera: kind=LoadableTrivialHiddenType, mangledName=$sSo10BigWrappera, size=8, alignment=4, stride=8, bitwiseCopyable=true, opaque=false, typeProperties=8704, nativeParameterRequiresIndirect=false, nativeResultRequiresIndirect=false, nativeComponents={{[0-9]+}}
