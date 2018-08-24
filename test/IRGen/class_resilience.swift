// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/class_resilience.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -enable-class-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -enable-class-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -enable-class-resilience -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -enable-class-resilience %t/class_resilience.swift | %FileCheck %t/class_resilience.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-%target-runtime -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -enable-class-resilience -O %t/class_resilience.swift

// CHECK: @"$S16class_resilience26ClassWithResilientPropertyC1s16resilient_struct4SizeVvpWvd" = hidden global [[INT]] 0
// CHECK: @"$S16class_resilience26ClassWithResilientPropertyC5colors5Int32VvpWvd" = hidden global [[INT]] 0

// CHECK: @"$S16class_resilience33ClassWithResilientlySizedPropertyC1r16resilient_struct9RectangleVvpWvd" = hidden global [[INT]] 0
// CHECK: @"$S16class_resilience33ClassWithResilientlySizedPropertyC5colors5Int32VvpWvd" = hidden global [[INT]] 0

// CHECK: @"$S16class_resilience14ResilientChildC5fields5Int32VvpWvd" = hidden global [[INT]] {{8|16}}

// CHECK: @"$S16class_resilience21ResilientGenericChildCMo" = {{(protected )?}}{{(dllexport )?}}global [[BOUNDS:{ (i32|i64), i32, i32 }]] zeroinitializer

// CHECK: @"$S16class_resilience26ClassWithResilientPropertyCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-SAME-32: { [[INT]] 52, i32 2, i32 13 }
// CHECK-SAME-64: { [[INT]] 80, i32 2, i32 10 }

// CHECK: @"$S16class_resilience28ClassWithMyResilientPropertyC1rAA0eF6StructVvpWvd" = hidden constant [[INT]] {{8|16}}
// CHECK: @"$S16class_resilience28ClassWithMyResilientPropertyC5colors5Int32VvpWvd" = hidden constant [[INT]] {{12|20}}

// CHECK: @"$S16class_resilience30ClassWithIndirectResilientEnumC1s14resilient_enum10FunnyShapeOvpWvd" = hidden constant [[INT]] {{8|16}}
// CHECK: @"$S16class_resilience30ClassWithIndirectResilientEnumC5colors5Int32VvpWvd" = hidden constant [[INT]] {{12|24}}

// CHECK: [[RESILIENTCHILD_NAME:@.*]] = private constant [15 x i8] c"ResilientChild\00"

// CHECK: @"$S16class_resilience14ResilientChildCMo" = {{(protected )?}}{{(dllexport )?}}global [[BOUNDS]] zeroinitializer

// CHECK: @"$S16class_resilience14ResilientChildCMn" = {{(protected )?}}{{(dllexport )?}}constant <{{.*}}> <{
// --       flags: class, unique, has vtable, in-place initialization, has resilient superclass
// CHECK-SAME:   <i32 0xC401_0050>
// --       parent:
// CHECK-SAME:   @"$S16class_resilienceMXM"
// --       name:
// CHECK-SAME:   [15 x i8]* [[RESILIENTCHILD_NAME]]
// --       metadata accessor function:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMa"
// --       field descriptor:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMF"
// -- superclass:
// CHECK-SAME:   @"got.$S15resilient_class22ResilientOutsideParentCMn"
// -- metadata bounds:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMo"
// --       metadata positive size in words (not used):
// CHECK-SAME:   i32 0,
// --       num immediate members:
// CHECK-SAME:   i32 4,
// --       num fields:
// CHECK-SAME:   i32 1,
// --       field offset vector offset:
// CHECK-SAME:   i32 3,
// --       singleton metadata initialization cache:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMl"
// --       resilient pattern:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMP"
// --       completion function:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMr"
// CHECK-SAME: }>

// CHECK: @"$S16class_resilience14ResilientChildCMP" = internal constant <{{.*}}> <{
// --       instantiation function:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCMi"
// --       destructor:
// CHECK-SAME:   @"$S16class_resilience14ResilientChildCfD"
// --       ivar destroyer:
// CHECK-SAME:   i32 0,
// --       flags:
// CHECK-SAME:   i32 3,
// --       RO data:
// CHECK-objc-SAME: @_DATA__TtC16class_resilience14ResilientChild
// CHECK-native-SAME: i32 0,
// --       metaclass:
// CHECK-objc-SAME: @"$S16class_resilience14ResilientChildCMm"
// CHECK-native-SAME: i32 0

// CHECK: @"$S16class_resilience16FixedLayoutChildCMo" = {{(protected )?}}{{(dllexport )?}}global [[BOUNDS]] zeroinitializer

// CHECK: @"$S16class_resilience17MyResilientParentCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-SAME-32: { [[INT]] 52, i32 2, i32 13 }
// CHECK-SAME-64: { [[INT]] 80, i32 2, i32 10 }

// CHECK: @"$S16class_resilience16MyResilientChildCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-SAME-32: { [[INT]] 60, i32 2, i32 15 }
// CHECK-SAME-64: { [[INT]] 96, i32 2, i32 12 }

// CHECK: @"$S16class_resilience24MyResilientGenericParentCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-SAME-32: { [[INT]] 52, i32 2, i32 13 }
// CHECK-SAME-64: { [[INT]] 80, i32 2, i32 10 }

// CHECK: @"$S16class_resilience24MyResilientConcreteChildCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-SAME-32: { [[INT]] 64, i32 2, i32 16 }
// CHECK-SAME-64: { [[INT]] 104, i32 2, i32 13 }

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
  public var field: Int32 = 0

  public override func getValue() -> Int {
    return 1
  }
}

// Superclass is resilient, but the class is fixed-layout.
// This simulates a user app subclassing a class in a resilient
// framework. In this case, we still want to emit a base offset
// global.

@_fixed_layout public class FixedLayoutChild : ResilientOutsideParent {
  public var field: Int32 = 0
}

// Superclass is resilient, so the number of fields and their
// offsets is not known at compile time

public class ResilientGenericChild<T> : ResilientGenericOutsideParent<T> {
  public var field: Int32 = 0
}


// Superclass is resilient and has a resilient value type payload,
// but everything is in one module


public class MyResilientParent {
  public let s: MyResilientStruct = MyResilientStruct(x: 0)
}

public class MyResilientChild : MyResilientParent {
  public let field: Int32 = 0
}


public class MyResilientGenericParent<T> {
  public let t: T

  public init(t: T) {
    self.t = t
  }
}

public class MyResilientConcreteChild : MyResilientGenericParent<Int> {
  public let x: Int

  public init(x: Int) {
    self.x = x
    super.init(t: x)
  }
}

extension ResilientGenericOutsideParent {
  public func genericExtensionMethod() -> A.Type {
    return A.self
  }
}

// ClassWithResilientProperty.color getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience26ClassWithResilientPropertyC5colors5Int32Vvg"(%T16class_resilience26ClassWithResilientPropertyC* swiftself)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @"$S16class_resilience26ClassWithResilientPropertyC5colors5Int32VvpWvd"
// CHECK-NEXT: [[PTR:%.*]] = bitcast %T16class_resilience26ClassWithResilientPropertyC* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Ts5Int32V*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK: ret i32 [[FIELD_VALUE]]

// ClassWithResilientProperty metadata accessor

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$S16class_resilience26ClassWithResilientPropertyCMa"(
// CHECK:      [[CACHE:%.*]] = load %swift.type*, %swift.type** getelementptr inbounds ({ %swift.type*, i8* }, { %swift.type*, i8* }* @"$S16class_resilience26ClassWithResilientPropertyCMl", i32 0, i32 0)
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK: cacheIsNull:
// CHECK-NEXT: [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$S16class_resilience26ClassWithResilientPropertyCMn" to %swift.type_descriptor*))
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 1
// CHECK-NEXT: br label %cont

// CHECK: cont:
// CHECK-NEXT: [[NEW_METADATA:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT: [[NEW_STATUS:%.*]] = phi [[INT]] [ 0, %entry ], [ [[STATUS]], %cacheIsNull ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[NEW_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]


// ClassWithResilientlySizedProperty.color getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience33ClassWithResilientlySizedPropertyC5colors5Int32Vvg"(%T16class_resilience33ClassWithResilientlySizedPropertyC* swiftself)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @"$S16class_resilience33ClassWithResilientlySizedPropertyC5colors5Int32VvpWvd"
// CHECK-NEXT: [[PTR:%.*]] = bitcast %T16class_resilience33ClassWithResilientlySizedPropertyC* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Ts5Int32V*
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK:      ret i32 [[FIELD_VALUE]]

// ClassWithResilientlySizedProperty metadata accessor

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$S16class_resilience33ClassWithResilientlySizedPropertyCMa"(
// CHECK:      [[CACHE:%.*]] = load %swift.type*, %swift.type** getelementptr inbounds ({ %swift.type*, i8* }, { %swift.type*, i8* }* @"$S16class_resilience33ClassWithResilientlySizedPropertyCMl", i32 0, i32 0)
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK: cacheIsNull:
// CHECK-NEXT: [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$S16class_resilience33ClassWithResilientlySizedPropertyCMn" to %swift.type_descriptor*))
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 1
// CHECK-NEXT: br label %cont

// CHECK: cont:
// CHECK-NEXT: [[NEW_METADATA:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT: [[NEW_STATUS:%.*]] = phi [[INT]] [ 0, %entry ], [ [[STATUS]], %cacheIsNull ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[NEW_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]


// ClassWithIndirectResilientEnum.color getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience30ClassWithIndirectResilientEnumC5colors5Int32Vvg"(%T16class_resilience30ClassWithIndirectResilientEnumC* swiftself)
// CHECK:      [[FIELD_PTR:%.*]] = getelementptr inbounds %T16class_resilience30ClassWithIndirectResilientEnumC, %T16class_resilience30ClassWithIndirectResilientEnumC* %0, i32 0, i32 2
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK: ret i32 [[FIELD_VALUE]]


// ResilientChild.field getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience14ResilientChildC5fields5Int32Vvg"(%T16class_resilience14ResilientChildC* swiftself)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @"$S16class_resilience14ResilientChildC5fields5Int32VvpWvd"
// CHECK-NEXT: [[PTR:%.*]] = bitcast %T16class_resilience14ResilientChildC* %0 to i8*
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = getelementptr inbounds i8, i8* [[PTR]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_PTR:%.*]] = bitcast i8* [[FIELD_ADDR]] to %Ts5Int32V*
// CHECK: call void @swift_beginAccess
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, i32* [[FIELD_PAYLOAD]]
// CHECK-NEXT: call void @swift_endAccess
// CHECK: ret i32 [[FIELD_VALUE]]


// ResilientGenericChild.field getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience21ResilientGenericChildC5fields5Int32Vvg"(%T16class_resilience21ResilientGenericChildC* swiftself)

// FIXME: we could eliminate the unnecessary isa load by lazily emitting
// metadata sources in EmitPolymorphicParameters

// CHECK:      load %swift.type*

// CHECK:      [[ADDR:%.*]] = getelementptr inbounds %T16class_resilience21ResilientGenericChildC, %T16class_resilience21ResilientGenericChildC* %0, i32 0, i32 0, i32 0
// CHECK-NEXT: [[ISA:%.*]] = load %swift.type*, %swift.type** [[ADDR]]
// CHECK-NEXT: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ([[BOUNDS]], [[BOUNDS]]* @"$S16class_resilience21ResilientGenericChildCMo", i32 0, i32 0)
// CHECK-NEXT: [[METADATA_OFFSET:%.*]] = add [[INT]] [[BASE]], {{16|32}}
// CHECK-NEXT: [[ISA_ADDR:%.*]] = bitcast %swift.type* [[ISA]] to i8*
// CHECK-NEXT: [[FIELD_OFFSET_TMP:%.*]] = getelementptr inbounds i8, i8* [[ISA_ADDR]], [[INT]] [[METADATA_OFFSET]]
// CHECK-NEXT: [[FIELD_OFFSET_ADDR:%.*]] = bitcast i8* [[FIELD_OFFSET_TMP]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_ADDR:%.*]]
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T16class_resilience21ResilientGenericChildC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
// CHECK:      call void @swift_beginAccess
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[RESULT:%.*]] = load i32, i32* [[PAYLOAD_ADDR]]
// CHECK-NEXT: call void @swift_endAccess
// CHECK:      ret i32 [[RESULT]]


// MyResilientChild.field getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience16MyResilientChildC5fields5Int32Vvg"(%T16class_resilience16MyResilientChildC* swiftself)
// CHECK:      [[FIELD_ADDR:%.*]] = getelementptr inbounds %T16class_resilience16MyResilientChildC, %T16class_resilience16MyResilientChildC* %0, i32 0, i32 2
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[RESULT:%.*]] = load i32, i32* [[PAYLOAD_ADDR]]
// CHECK:      ret i32 [[RESULT]]


// ResilientGenericOutsideParent.genericExtensionMethod()

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.type* @"$S15resilient_class29ResilientGenericOutsideParentC0B11_resilienceE22genericExtensionMethodxmyF"(%T15resilient_class29ResilientGenericOutsideParentC* swiftself) #0 {
// CHECK:      [[ISA_ADDR:%.*]] = bitcast %T15resilient_class29ResilientGenericOutsideParentC* %0 to %swift.type**
// CHECK-NEXT: [[ISA:%.*]] = load %swift.type*, %swift.type** [[ISA_ADDR]]
// CHECK:      [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ([[BOUNDS]], [[BOUNDS]]* @"$S15resilient_class29ResilientGenericOutsideParentCMo", i32 0, i32 0)
// CHECK-NEXT: [[GENERIC_PARAM_OFFSET:%.*]] = add [[INT]] [[BASE]], 0
// CHECK-NEXT: [[ISA_TMP:%.*]] = bitcast %swift.type* [[ISA]] to i8*
// CHECK-NEXT: [[GENERIC_PARAM_TMP:%.*]] = getelementptr inbounds i8, i8* [[ISA_TMP]], [[INT]] [[GENERIC_PARAM_OFFSET]]
// CHECK-NEXT: [[GENERIC_PARAM_ADDR:%.*]] = bitcast i8* [[GENERIC_PARAM_TMP]] to %swift.type**
// CHECK-NEXT: [[GENERIC_PARAM:%.*]] = load %swift.type*, %swift.type** [[GENERIC_PARAM_ADDR]]
// CHECK:       ret %swift.type* [[GENERIC_PARAM]]


// ResilientChild.field getter dispatch thunk

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$S16class_resilience14ResilientChildC5fields5Int32VvgTj"(%T16class_resilience14ResilientChildC* swiftself)
// CHECK:      [[ISA_ADDR:%.*]] = getelementptr inbounds %T16class_resilience14ResilientChildC, %T16class_resilience14ResilientChildC* %0, i32 0, i32 0, i32 0
// CHECK-NEXT: [[ISA:%.*]] = load %swift.type*, %swift.type** [[ISA_ADDR]]
// CHECK-NEXT: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ([[BOUNDS]], [[BOUNDS]]* @"$S16class_resilience14ResilientChildCMo", i32 0, i32 0)
// CHECK-NEXT: [[METADATA_BYTES:%.*]] = bitcast %swift.type* [[ISA]] to i8*
// CHECK-NEXT: [[VTABLE_OFFSET_TMP:%.*]] = getelementptr inbounds i8, i8* [[METADATA_BYTES]], [[INT]] [[BASE]]
// CHECK-NEXT: [[VTABLE_OFFSET_ADDR:%.*]] = bitcast i8* [[VTABLE_OFFSET_TMP]] to i32 (%T16class_resilience14ResilientChildC*)**
// CHECK-NEXT: [[METHOD:%.*]] = load i32 (%T16class_resilience14ResilientChildC*)*, i32 (%T16class_resilience14ResilientChildC*)** [[VTABLE_OFFSET_ADDR]]
// CHECK-NEXT: [[RESULT:%.*]] = call swiftcc i32 [[METHOD]](%T16class_resilience14ResilientChildC* swiftself %0)
// CHECK-NEXT: ret i32 [[RESULT]]


// ClassWithResilientProperty metadata initialization function

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S16class_resilience26ClassWithResilientPropertyCMr"(%swift.type*, i8*, i8**)
// CHECK: entry:
// CHECK-NEXT: [[FIELDS:%.*]] = alloca [3 x i8**]
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* %0 to [[INT]]*
// CHECK-NEXT: [[FIELDS_DEST:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[METADATA_ADDR]], [[INT]] {{11|14}}
// CHECK-NEXT: [[FIELDS_ADDR:%.*]] = bitcast [3 x i8**]* [[FIELDS]] to i8*
// CHECK-NEXT: call void @llvm.lifetime.start.p0i8(i64 {{12|24}}, i8* [[FIELDS_ADDR]])
// CHECK-NEXT: [[FIELDS_PTR:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* [[FIELDS]], i32 0, i32 0

// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"([[INT]] 319)
// CHECK-NEXT: [[SIZE_METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT: [[RESULT:%.*]] = icmp ule [[INT]] [[STATUS]], 63
// CHECK-NEXT: br i1 [[RESULT]], label %dependency-satisfied, label %metadata-dependencies.cont

// CHECK: dependency-satisfied:

// -- ClassLayoutFlags = 0x100 (HasStaticVTable)
// CHECK:      void @swift_initClassMetadata(%swift.type* %0, %swift.type* null, [[INT]] 256, [[INT]] 3, i8*** [[FIELDS_PTR]], [[INT]]* [[FIELDS_DEST]])

// CHECK-native:      [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* {{.*}}
// CHECK-native-NEXT: store [[INT]] [[FIELD_OFFSET]], [[INT]]* @"$S16class_resilience26ClassWithResilientPropertyC1s16resilient_struct4SizeVvpWvd"

// CHECK-native:      [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* {{.*}}
// CHECK-native-NEXT: store [[INT]] [[FIELD_OFFSET]], [[INT]]* @"$S16class_resilience26ClassWithResilientPropertyC5colors5Int32VvpWvd"

// CHECK: br label %metadata-dependencies.cont

// CHECK: metadata-dependencies.cont:

// CHECK-NEXT: [[PENDING_METADATA:%.*]] = phi %swift.type* [ [[SIZE_METADATA]], %entry ], [ null, %dependency-satisfied ]
// CHECK-NEXT: [[NEW_STATUS:%.*]] = phi [[INT]] [ 63, %entry ], [ 0, %dependency-satisfied ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[PENDING_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]


// ClassWithResilientlySizedProperty metadata initialization function

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S16class_resilience33ClassWithResilientlySizedPropertyCMr"(%swift.type*, i8*, i8**)
// CHECK: entry:
// CHECK-NEXT: [[FIELDS:%.*]] = alloca [2 x i8**]
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* %0 to [[INT]]*
// CHECK-NEXT: [[FIELDS_DEST:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[METADATA_ADDR]], [[INT]] {{11|14}}
// CHECK-NEXT: [[FIELDS_ADDR:%.*]] = bitcast [2 x i8**]* [[FIELDS]] to i8*
// CHECK-NEXT: call void @llvm.lifetime.start.p0i8(i64 {{8|16}}, i8* [[FIELDS_ADDR]])
// CHECK-NEXT: [[FIELDS_PTR:%.*]] = getelementptr inbounds [2 x i8**], [2 x i8**]* [[FIELDS]], i32 0, i32 0

// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct9RectangleVMa"([[INT]] 319)
// CHECK-NEXT: [[SIZE_METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT: [[RESULT:%.*]] = icmp ule [[INT]] [[STATUS]], 63
// CHECK-NEXT: br i1 [[RESULT]], label %dependency-satisfied, label %metadata-dependencies.cont

// CHECK: dependency-satisfied:

// -- ClassLayoutFlags = 0x100 (HasStaticVTable)
// CHECK:      call void @swift_initClassMetadata(%swift.type* %0, %swift.type* null, [[INT]] 256, [[INT]] 2, i8*** [[FIELDS_PTR]], [[INT]]* [[FIELDS_DEST]])

// CHECK-native:      [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* {{.*}}
// CHECK-native-NEXT: store [[INT]] [[FIELD_OFFSET]], [[INT]]* @"$S16class_resilience33ClassWithResilientlySizedPropertyC1r16resilient_struct9RectangleVvpWvd"

// CHECK-native:      [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* {{.*}}
// CHECK-native-NEXT: store [[INT]] [[FIELD_OFFSET]], [[INT]]* @"$S16class_resilience33ClassWithResilientlySizedPropertyC5colors5Int32VvpWvd"

// CHECK: br label %metadata-dependencies.cont

// CHECK: metadata-dependencies.cont:

// CHECK-NEXT: [[PENDING_METADATA:%.*]] = phi %swift.type* [ [[SIZE_METADATA]], %entry ], [ null, %dependency-satisfied ]
// CHECK-NEXT: [[NEW_STATUS:%.*]] = phi [[INT]] [ 63, %entry ], [ 0, %dependency-satisfied ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[PENDING_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]


// ResilientChild metadata initialization function

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S16class_resilience14ResilientChildCMr"(%swift.type*, i8*, i8**)

// Initialize the superclass field...
// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15resilient_class22ResilientOutsideParentCMa"([[INT]] 257)
// CHECK:      [[SUPER:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT: [[RESULT:%.*]] = icmp ule [[INT]] [[STATUS]], 1
// CHECK-NEXT: br i1 [[RESULT]], label %dependency-satisfied, label %metadata-dependencies.cont

// CHECK: dependency-satisfied:

// Initialize field offset vector...
// CHECK:      call void @swift_initClassMetadata(%swift.type* %0, %swift.type* [[SUPER]], [[INT]] 0, [[INT]] 1, i8*** {{.*}}, [[INT]]* {{.*}})

// Initialize constructor vtable override...
// CHECK:      [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ([[BOUNDS]], [[BOUNDS]]* @"$S15resilient_class22ResilientOutsideParentCMo", i32 0, i32 0)
// CHECK:      [[OFFSET:%.*]] = add [[INT]] [[BASE]], {{16|32}}
// CHECK:      [[METADATA_BYTES:%.*]] = bitcast %swift.type* %0 to i8*
// CHECK:      [[VTABLE_ENTRY_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_BYTES]], [[INT]] [[OFFSET]]
// CHECK:      [[VTABLE_ENTRY_TMP:%.*]] = bitcast i8* [[VTABLE_ENTRY_ADDR]] to i8**
// CHECK:      store i8* bitcast (%T16class_resilience14ResilientChildC* (%T16class_resilience14ResilientChildC*)* @"$S16class_resilience14ResilientChildCACycfc" to i8*), i8** [[VTABLE_ENTRY_TMP]]

// Initialize getValue() vtable override...
// CHECK:      [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ([[BOUNDS]], [[BOUNDS]]* @"$S15resilient_class22ResilientOutsideParentCMo", i32 0, i32 0)
// CHECK:      [[OFFSET:%.*]] = add [[INT]] [[BASE]], {{28|56}}
// CHECK:      [[METADATA_BYTES:%.*]] = bitcast %swift.type* %0 to i8*
// CHECK:      [[VTABLE_ENTRY_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_BYTES]], [[INT]] [[OFFSET]]
// CHECK:      [[VTABLE_ENTRY_TMP:%.*]] = bitcast i8* [[VTABLE_ENTRY_ADDR]] to i8**
// CHECK:      store i8* bitcast ([[INT]] (%T16class_resilience14ResilientChildC*)* @"$S16class_resilience14ResilientChildC8getValueSiyF" to i8*), i8** [[VTABLE_ENTRY_TMP]]

// CHECK:      br label %metadata-dependencies.cont

// CHECK: metadata-dependencies.cont:

// CHECK-NEXT: [[PENDING_METADATA:%.*]] = phi %swift.type* [ [[SUPER]], %entry ], [ null, %dependency-satisfied ]
// CHECK-NEXT: [[NEW_STATUS:%.*]] = phi [[INT]] [ 1, %entry ], [ 0, %dependency-satisfied ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[PENDING_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]


// ResilientChild metadata relocation function

// CHECK-LABEL: define internal %swift.type* @"$S16class_resilience14ResilientChildCMi"(%swift.type_descriptor*, i8*)
// CHECK-NEXT: entry:
// CHECK-NEXT: [[METADATA:%.*]] = call %swift.type* @swift_relocateClassMetadata(%swift.type_descriptor* %0, i8* %1)
// CHECK-NEXT: ret %swift.type* [[METADATA]]


// ResilientChild.field setter dispatch thunk

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S16class_resilience14ResilientChildC5fields5Int32VvsTj"(i32, %T16class_resilience14ResilientChildC* swiftself)
// CHECK:      [[ISA_ADDR:%.*]] = getelementptr inbounds %T16class_resilience14ResilientChildC, %T16class_resilience14ResilientChildC* %1, i32 0, i32 0, i32 0
// CHECK-NEXT: [[ISA:%.*]] = load %swift.type*, %swift.type** [[ISA_ADDR]]
// CHECK-NEXT: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ([[BOUNDS]], [[BOUNDS]]* @"$S16class_resilience14ResilientChildCMo", i32 0, i32 0)
// CHECK-NEXT: [[METADATA_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}
// CHECK-NEXT: [[METADATA_BYTES:%.*]] = bitcast %swift.type* [[ISA]] to i8*
// CHECK-NEXT: [[VTABLE_OFFSET_TMP:%.*]] = getelementptr inbounds i8, i8* [[METADATA_BYTES]], [[INT]] [[METADATA_OFFSET]]
// CHECK-NEXT: [[VTABLE_OFFSET_ADDR:%.*]] = bitcast i8* [[VTABLE_OFFSET_TMP]] to void (i32, %T16class_resilience14ResilientChildC*)**
// CHECK-NEXT: [[METHOD:%.*]] = load void (i32, %T16class_resilience14ResilientChildC*)*, void (i32, %T16class_resilience14ResilientChildC*)** [[VTABLE_OFFSET_ADDR]]
// CHECK-NEXT: call swiftcc void [[METHOD]](i32 %0, %T16class_resilience14ResilientChildC* swiftself %1)
// CHECK-NEXT: ret void


// FixedLayoutChild metadata initialization function

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S16class_resilience16FixedLayoutChildCMr"(%swift.type*, i8*, i8**)

// Initialize the superclass field...
// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15resilient_class22ResilientOutsideParentCMa"([[INT]] 257)
// CHECK:      [[SUPER:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT: [[RESULT:%.*]] = icmp ule [[INT]] [[STATUS]], 1
// CHECK-NEXT: br i1 [[RESULT]], label %dependency-satisfied, label %metadata-dependencies.cont

// CHECK: dependency-satisfied:

// CHECK:      call void @swift_initClassMetadata(%swift.type* %0, %swift.type* [[SUPER]], [[INT]] 0, [[INT]] 1, i8*** {{%.*}}, [[INT]]* {{%.*}})

// CHECK:      br label %metadata-dependencies.cont

// CHECK: metadata-dependencies.cont:

// CHECK-NEXT: [[PENDING_METADATA:%.*]] = phi %swift.type* [ [[SUPER]], %entry ], [ null, %dependency-satisfied ]
// CHECK-NEXT: [[NEW_STATUS:%.*]] = phi [[INT]] [ 1, %entry ], [ 0, %dependency-satisfied ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[PENDING_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]


// FixedLayoutChild metadata relocation function

// CHECK-LABEL: define internal %swift.type* @"$S16class_resilience16FixedLayoutChildCMi"(%swift.type_descriptor*, i8*)
// CHECK-NEXT: entry:
// CHECK-NEXT: [[METADATA:%.*]] = call %swift.type* @swift_relocateClassMetadata(%swift.type_descriptor* %0, i8* %1)
// CHECK-NEXT: ret %swift.type* [[METADATA]]


// ResilientGenericChild metadata initialization function

// CHECK-LABEL: define internal %swift.type* @"$S16class_resilience21ResilientGenericChildCMi"(%swift.type_descriptor*, i8**, i8*)
// CHECK:              [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** %1, i8* %2)
// CHECK:              ret %swift.type* [[METADATA]]

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S16class_resilience21ResilientGenericChildCMr"
// CHECK-SAME:    (%swift.type* [[METADATA:%.*]], i8*, i8**)

// Initialize the superclass pointer...
// CHECK:              [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15resilient_class29ResilientGenericOutsideParentCMa"([[INT]] 257, %swift.type* %T)
// CHECK:              [[SUPER:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK:              [[SUPER_STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK:              [[SUPER_OK:%.*]] = icmp ule [[INT]] [[SUPER_STATUS]], 1
// CHECK:              br i1 [[SUPER_OK]],

// CHECK:              call void @swift_initClassMetadata(%swift.type* [[METADATA]], %swift.type* [[SUPER]], [[INT]] 0,
// CHECK:              [[DEP:%.*]] = phi %swift.type* [ [[SUPER]], {{.*}} ], [ null, {{.*}} ]
// CHECK:              [[DEP_REQ:%.*]] = phi [[INT]] [ 1, {{.*}} ], [ 0, {{.*}} ]
// CHECK:              [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[DEP]], 0
// CHECK:              [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[DEP_REQ]], 1
// CHECK:              ret %swift.metadata_response [[T1]]
