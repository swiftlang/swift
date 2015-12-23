// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience %s | FileCheck %s
// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience -O %s

// CHECK: %swift.type = type { [[INT:i32|i64]] }

// CHECK: @_TWvdvC16class_resilience11MyRectangle1sV16resilient_struct4Size = global [[INT]] 0
// CHECK: @_TWvdvC16class_resilience11MyRectangle5colorVs5Int32 = global [[INT]] 0

// CHECK: @_TWvdvC16class_resilience24ClassWithResilientLayout1rV16resilient_struct9Rectangle = global [[INT]] 0
// CHECK: @_TWvdvC16class_resilience24ClassWithResilientLayout5colorVs5Int32 = global [[INT]] 0

// CHECK: @_TWvdvC16class_resilience14ResilientChild5fieldVs5Int32 = global [[INT]] {{8|16}}
// CHECK: @_TWvivC16class_resilience21ResilientGenericChild5fieldVs5Int32 = global [[INT]] {{44|88}}

import resilient_class
import resilient_struct
import resilient_enum

// Concrete class with resilient stored property

public class MyRectangle {
  public let p: Point
  public let s: Size
  public let color: Int32

  public init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}

// Concrete class with non-fixed size stored property

public class ClassWithResilientLayout {
  public let r: Rectangle
  public let color: Int32

  public init(r: Rectangle, color: Int32) {
    self.r = r
    self.color = color
  }
}

// Enums with indirect payloads are fixed-size

public class ClassWithIndirectResilientEnum {
  public let s: FunnyShape
  public let color: Int32

  public init(s: FunnyShape, color: Int32) {
    self.s = s
    self.color = color
  }
}

// Superclass is resilient, so the number of fields and their
// offsets is not known at compile time

public class ResilientChild : ResilientOutsideParent {
  public let field: Int32 = 0
}

// Superclass is resilient, so the number of fields and their
// offsets is not known at compile time

public class ResilientGenericChild<T> : ResilientGenericOutsideParent<T> {
  public let field: Int32 = 0
}

// Superclass is resilient and has a resilient value type payload,
// but everything is in one module

public struct MyResilientStruct {}

public class MyResilientParent {
  public let s: MyResilientStruct = MyResilientStruct()  
}

public class MyResilientChild : MyResilientParent {
  public let field: Int32 = 0
}


// FIXME: This is bogus since we don't emit code to initialize the
// global ivar offsets yet.


// MyRectangle.color getter

// CHECK-LABEL: define i32 @_TFC16class_resilience11MyRectangleg5colorVs5Int32(%C16class_resilience11MyRectangle*)
// CHECK: [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience11MyRectangle5colorVs5Int32
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience11MyRectangle* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ClassWithResilientLayout.color getter

// CHECK-LABEL: define i32 @_TFC16class_resilience24ClassWithResilientLayoutg5colorVs5Int32(%C16class_resilience24ClassWithResilientLayout*)
// CHECK: [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience24ClassWithResilientLayout5colorVs5Int32
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience24ClassWithResilientLayout* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ClassWithIndirectResilientEnum.color getter

// CHECK-LABEL: define i32 @_TFC16class_resilience30ClassWithIndirectResilientEnumg5colorVs5Int32(%C16class_resilience30ClassWithIndirectResilientEnum*)
// CHECK: [[FIELD_PTR:%.*]] = getelementptr inbounds %C16class_resilience30ClassWithIndirectResilientEnum, %C16class_resilience30ClassWithIndirectResilientEnum* %0, i32 0, i32 2
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ResilientChild.field getter

// CHECK-LABEL: define i32 @_TFC16class_resilience14ResilientChildg5fieldVs5Int32(%C16class_resilience14ResilientChild*)
// CHECK: [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience14ResilientChild5fieldVs5Int32
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience14ResilientChild* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ResilientGenericChild.field getter


// CHECK-LABEL: define i32 @_TFC16class_resilience21ResilientGenericChildg5fieldVs5Int32(%C16class_resilience21ResilientGenericChild*)

// FIXME: we could eliminate the unnecessary isa load by lazily emitting
// metadata sources in EmitPolymorphicParameters

// CHECK:      [[T_BOX:%.*]] = alloca %swift.type*
// CHECK:      store {{.*}}, %swift.type** [[T_BOX]]

// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds %C16class_resilience21ResilientGenericChild, %C16class_resilience21ResilientGenericChild* %0, i32 0, i32 0, i32 0
// CHECK-NEXT: [[ISA:%.*]] = load %swift.type*, %swift.type** [[ADDR]]
// CHECK-NEXT: [[INDIRECT_OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvivC16class_resilience21ResilientGenericChild5fieldVs5Int32
// CHECK-NEXT: [[ISA_ADDR:%.*]] = bitcast %swift.type* [[ISA]] to i8*
// CHECK-NEXT: [[FIELD_OFFSET_TMP:%.*]] = getelementptr inbounds i8, i8* [[ISA_ADDR]], [[INT]] [[INDIRECT_OFFSET]]
// CHECK-NEXT: [[FIELD_OFFSET_ADDR:%.*]] = bitcast i8* [[FIELD_OFFSET_TMP]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_ADDR:%.*]]
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %C16class_resilience21ResilientGenericChild* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[RESULT:%.*]] = load i32, i32* [[PAYLOAD_ADDR]]
// CHECK-NEXT: ret i32 [[RESULT]]


// MyResilientChild.field getter

// CHECK-LABEL: define i32 @_TFC16class_resilience16MyResilientChildg5fieldVs5Int32(%C16class_resilience16MyResilientChild*)
// CHECK:      [[FIELD_ADDR:%.*]] = getelementptr inbounds %C16class_resilience16MyResilientChild, %C16class_resilience16MyResilientChild* %0, i32 0, i32 1
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[RESULT:%.*]] = load i32, i32* [[PAYLOAD_ADDR]]
// CHECK-NEXT: ret i32 [[RESULT]]
