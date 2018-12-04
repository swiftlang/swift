// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefix=CHECK

// REQUIRES: CPU=armv7k

// CHECK: %swift.type = type { [[INT:i32|i64]] }

import Foundation

public class FixedLayoutObjCSubclass : NSObject {
  // This field could use constant direct access because NSObject has
  // fixed layout, but we don't allow that right now.
  public final var field: Int32 = 0
};

// CHECK-LABEL: define hidden swiftcc void @"$s28class_resilience_objc_armv7k29testConstantDirectFieldAccessyyAA23FixedLayoutObjCSubclassCF"(%T28class_resilience_objc_armv7k23FixedLayoutObjCSubclassC*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @"$s28class_resilience_objc_armv7k23FixedLayoutObjCSubclassC5fields5Int32VvpWvd"
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T28class_resilience_objc_armv7k23FixedLayoutObjCSubclassC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
// CHECK:      call void @swift_beginAccess
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: store i32 10, i32* [[PAYLOAD_ADDR]]

func testConstantDirectFieldAccess(_ o: FixedLayoutObjCSubclass) {
  o.field = 10
}

public class NonFixedLayoutObjCSubclass : NSCoder {
  // This field uses non-constant direct access because NSCoder has resilient
  // layout.
  public final var field: Int32 = 0
}

// CHECK-LABEL: define hidden swiftcc void @"$s28class_resilience_objc_armv7k32testNonConstantDirectFieldAccessyyAA0F23FixedLayoutObjCSubclassCF"(%T28class_resilience_objc_armv7k26NonFixedLayoutObjCSubclassC*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @"$s28class_resilience_objc_armv7k26NonFixedLayoutObjCSubclassC5fields5Int32VvpWvd"
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T28class_resilience_objc_armv7k26NonFixedLayoutObjCSubclassC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
// CHECK:      call void @swift_beginAccess
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: store i32 10, i32* [[PAYLOAD_ADDR]]

func testNonConstantDirectFieldAccess(_ o: NonFixedLayoutObjCSubclass) {
  o.field = 10
}

public class GenericObjCSubclass<T> : NSCoder {
  public final var content: T
  public final var field: Int32 = 0

  public init(content: T) {
    self.content = content
  }
}

// CHECK-LABEL: define hidden swiftcc void @"$s28class_resilience_objc_armv7k31testConstantIndirectFieldAccessyyAA19GenericObjCSubclassCyxGlF"(%T28class_resilience_objc_armv7k19GenericObjCSubclassC*)

// FIXME: we could eliminate the unnecessary isa load by lazily emitting
// metadata sources in EmitPolymorphicParameters

// CHECK:      call %objc_class* @object_getClass
// CHECK:      [[ADDR:%.*]] = bitcast %T28class_resilience_objc_armv7k19GenericObjCSubclassC* %0 to %objc_object*
// CHECK-NEXT: [[KLASS:%.*]] = call %objc_class* @object_getClass(%objc_object* [[ADDR]])
// CHECK-NEXT: [[ISA:%.*]] = bitcast %objc_class* [[KLASS]] to %swift.type*
// CHECK-NEXT: [[ISA_ADDR:%.*]] = bitcast %swift.type* [[ISA]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[ISA_ADDR]], [[INT]] 15
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_ADDR:%.*]]
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T28class_resilience_objc_armv7k19GenericObjCSubclassC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
// CHECK:      call void @swift_beginAccess
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: store i32 10, i32* [[PAYLOAD_ADDR]]

func testConstantIndirectFieldAccess<T>(_ o: GenericObjCSubclass<T>) {
  // This field uses constant indirect access because NSCoder has resilient
  // layout. Non-constant indirect is never needed for Objective-C classes
  // because the field offset vector only contains Swift field offsets.
  o.field = 10
}
