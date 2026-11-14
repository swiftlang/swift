// Test that a recovered layout for a missing class field preserves a complete
// superclass layout for client subclasses.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Hidden.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   -I %t %t/Client.swift
// RUN: %FileCheck %s < %t/Client.ll

//--- Hidden.h
typedef struct {
  long long first;
  long long second;
} HiddenCStruct;

//--- Library.swift
open class Base {
  private final var hidden: HiddenCStruct
  public var visible: Int64

  public init() {
    hidden = HiddenCStruct(first: 0, second: 0)
    visible = 0
  }
}

//--- Client.swift
import Library

public class Derived: Base {
  public var clientField: Int64 = 0
}

// The client can lay out the imported superclass and directly address its own
// field after the two-word hidden field and the visible field.
// CHECK-DAG: %T6Client7DerivedC = type <{ %swift.refcounted, <{ <{ i64 }>, <{ i64 }> }>, %Ts5Int64V, %Ts5Int64V }>
// CHECK-DAG: @"$s6Client7DerivedC11clientFields5Int64VvpWvd" = constant i64 40
// CHECK: define swiftcc i64 @"$s6Client7DerivedC11clientFields5Int64Vvg"
// CHECK: getelementptr inbounds nuw %T6Client7DerivedC, ptr %{{.*}}, i32 0, i32 3
