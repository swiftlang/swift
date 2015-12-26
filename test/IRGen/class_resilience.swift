// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience %s | FileCheck %s
// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience -O %s

// CHECK: %swift.type = type { [[INT:i32|i64]] }

// CHECK: @_TWvdvC16class_resilience26ClassWithResilientProperty1sV16resilient_struct4Size = global [[INT]] 0
// CHECK: @_TWvdvC16class_resilience26ClassWithResilientProperty5colorVs5Int32 = global [[INT]] 0

// CHECK: @_TWvdvC16class_resilience33ClassWithResilientlySizedProperty1rV16resilient_struct9Rectangle = global [[INT]] 0
// CHECK: @_TWvdvC16class_resilience33ClassWithResilientlySizedProperty5colorVs5Int32 = global [[INT]] 0

// CHECK: @_TWvdvC16class_resilience14ResilientChild5fieldVs5Int32 = global [[INT]] {{12|16}}
// CHECK: @_TWvivC16class_resilience21ResilientGenericChild5fieldVs5Int32 = global [[INT]] {{56|88}}

// CHECK: @_TWvdvC16class_resilience28ClassWithMyResilientProperty1rVS_17MyResilientStruct = constant [[INT]] {{12|16}}
// CHECK: @_TWvdvC16class_resilience28ClassWithMyResilientProperty5colorVs5Int32 = constant [[INT]] {{16|20}}

// CHECK: @_TWvdvC16class_resilience30ClassWithIndirectResilientEnum1sO14resilient_enum10FunnyShape = constant [[INT]] {{12|16}}
// CHECK: @_TWvdvC16class_resilience30ClassWithIndirectResilientEnum5colorVs5Int32 = constant [[INT]] {{16|24}}

import resilient_class
import resilient_struct
import resilient_enum


// Concrete class with resilient stored property

public class ClassWithResilientProperty {
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

public class ClassWithResilientlySizedProperty {
  public let r: Rectangle
  public let color: Int32

  public init(r: Rectangle, color: Int32) {
    self.r = r
    self.color = color
  }
}


// Concrete class with resilient stored property that
// is fixed-layout inside this resilience domain

public struct MyResilientStruct {
  public let x: Int32
}

public class ClassWithMyResilientProperty {
  public let r: MyResilientStruct
  public let color: Int32

  public init(r: MyResilientStruct, color: Int32) {
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


public class MyResilientParent {
  public let s: MyResilientStruct = MyResilientStruct(x: 0)
}

public class MyResilientChild : MyResilientParent {
  public let field: Int32 = 0
}


// ClassWithResilientProperty metadata accessor

// CHECK-LABEL: define %swift.type* @_TMaC16class_resilience26ClassWithResilientProperty()
// CHECK:      [[CACHE:%.*]] = load %swift.type*, %swift.type** @_TMLC16class_resilience26ClassWithResilientProperty
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:    cacheIsNull:
// CHECK-NEXT: [[METADATA:%.*]] = call %swift.type* @swift_getResilientMetadata(
// CHECK-NEXT: store %swift.type* [[METADATA]], %swift.type** @_TMLC16class_resilience26ClassWithResilientProperty
// CHECK-NEXT: br label %cont

// CHECK:    cont:
// CHECK-NEXT: [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT: ret %swift.type* [[RESULT]]


// ClassWithResilientProperty.color getter

// CHECK-LABEL: define i32 @_TFC16class_resilience26ClassWithResilientPropertyg5colorVs5Int32(%C16class_resilience26ClassWithResilientProperty*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience26ClassWithResilientProperty5colorVs5Int32
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience26ClassWithResilientProperty* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ClassWithResilientlySizedProperty metadata accessor

// CHECK-LABEL: define %swift.type* @_TMaC16class_resilience33ClassWithResilientlySizedProperty()
// CHECK:      [[CACHE:%.*]] = load %swift.type*, %swift.type** @_TMLC16class_resilience33ClassWithResilientlySizedProperty
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:    cacheIsNull:
// CHECK-NEXT: [[METADATA:%.*]] = call %swift.type* @swift_getResilientMetadata(
// CHECK-NEXT: store %swift.type* [[METADATA]], %swift.type** @_TMLC16class_resilience33ClassWithResilientlySizedProperty
// CHECK-NEXT: br label %cont

// CHECK:    cont:
// CHECK-NEXT: [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT: ret %swift.type* [[RESULT]]


// ClassWithResilientlySizedProperty.color getter

// CHECK-LABEL: define i32 @_TFC16class_resilience33ClassWithResilientlySizedPropertyg5colorVs5Int32(%C16class_resilience33ClassWithResilientlySizedProperty*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience33ClassWithResilientlySizedProperty5colorVs5Int32
// CHECK-NEXT: [[PTR:%.*]] = bitcast %C16class_resilience33ClassWithResilientlySizedProperty* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ClassWithIndirectResilientEnum.color getter

// CHECK-LABEL: define i32 @_TFC16class_resilience30ClassWithIndirectResilientEnumg5colorVs5Int32(%C16class_resilience30ClassWithIndirectResilientEnum*)
// CHECK:      [[FIELD_PTR:%.*]] = getelementptr inbounds %C16class_resilience30ClassWithIndirectResilientEnum, %C16class_resilience30ClassWithIndirectResilientEnum* %0, i32 0, i32 2
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]


// ResilientChild.field getter

// CHECK-LABEL: define i32 @_TFC16class_resilience14ResilientChildg5fieldVs5Int32(%C16class_resilience14ResilientChild*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC16class_resilience14ResilientChild5fieldVs5Int32
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
// CHECK:      [[FIELD_ADDR:%.*]] = getelementptr inbounds %C16class_resilience16MyResilientChild, %C16class_resilience16MyResilientChild* %0, i32 0, i32 2
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[RESULT:%.*]] = load i32, i32* [[PAYLOAD_ADDR]]
// CHECK-NEXT: ret i32 [[RESULT]]


// ClassWithResilientProperty metadata instantiation function

// FIXME: This is bogus since we don't emit code to initialize the
// global ivar offsets yet.


// CHECK-LABEL: define private %swift.type* @create_generic_metadata_ClassWithResilientProperty(%swift.type_pattern*, i8**)
// CHECK:      [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(
// CHECK:      [[SIZE_METADATA:%.*]] = call %swift.type* @_TMaV16resilient_struct4Size()
// CHECK:      call void @swift_initClassMetadata_UniversalStrategy(
// CHECK:      ret %swift.type* [[METADATA]]


// ClassWithResilientlySizedProperty metadata instantiation function

// CHECK-LABEL: define private %swift.type* @create_generic_metadata_ClassWithResilientlySizedProperty(%swift.type_pattern*, i8**)
// CHECK:      [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(
// CHECK:      [[RECTANGLE_METADATA:%.*]] = call %swift.type* @_TMaV16resilient_struct9Rectangle()
// CHECK:      call void @swift_initClassMetadata_UniversalStrategy(
// CHECK:      ret %swift.type* [[METADATA]]
