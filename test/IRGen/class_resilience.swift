// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience %s | FileCheck %s
// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience -O %s

// CHECK: %Si = type <{ [[INT:i32|i64]] }>

import resilient_struct
import resilient_enum

// Concrete class with resilient stored property

public class MyRectangle {
  public let p: Point
  public let s: Size
  public let color: Int

  public init(p: Point, s: Size, color: Int) {
    self.p = p
    self.s = s
    self.color = color
  }
}

// Concrete class with non-fixed size stored property

public class ClassWithResilientLayout {
  public let r: Rectangle
  public let color: Int

  public init(r: Rectangle, color: Int) {
    self.r = r
    self.color = color
  }
}

// Enums with indirect payloads are fixed-size

public class ClassWithIndirectResilientEnum {
  public let s: FunnyShape
  public let color: Int

  public init(s: FunnyShape, color: Int) {
    self.s = s
    self.color = color
  }
}

// FIXME: This is bogus since we don't emit code to initialize the
// global ivar offsets yet.


// CHECK-LABEL: define {{i32|i64}} @_TFC16class_resilience11MyRectangleg5colorSi(%C16class_resilience11MyRectangle*)
// CHECK: [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience11MyRectangle5colorSi
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience11MyRectangle* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Si*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Si, %Si* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret [[INT]] [[FIELD_VALUE]]


// CHECK-LABEL: define {{i32|i64}} @_TFC16class_resilience24ClassWithResilientLayoutg5colorSi(%C16class_resilience24ClassWithResilientLayout*)
// CHECK: [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience24ClassWithResilientLayout5colorSi
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience24ClassWithResilientLayout* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Si*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Si, %Si* %.color, i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret [[INT]] [[FIELD_VALUE]]


// CHECK-LABEL: define {{i32|i64}} @_TFC16class_resilience30ClassWithIndirectResilientEnumg5colorSi(%C16class_resilience30ClassWithIndirectResilientEnum*)
// CHECK: [[FIELD_PTR:%.*]] = getelementptr inbounds %C16class_resilience30ClassWithIndirectResilientEnum, %C16class_resilience30ClassWithIndirectResilientEnum* %0, i32 0, i32 2
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Si, %Si* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load [[INT]], [[INT]]* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret [[INT]] [[FIELD_VALUE]]
