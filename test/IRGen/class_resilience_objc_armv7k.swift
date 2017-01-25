// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefix=CHECK

// REQUIRES: objc_interop
// REQUIRES: CPU=armv7k

// CHECK: %swift.type = type { [[INT:i32|i64]] }

import Foundation

public class FixedLayoutObjCSubclass : NSObject {
  // This field could use constant direct access because NSObject has
  // fixed layout, but we don't allow that right now.
  public final var field: Int32 = 0
};

// CHECK-LABEL: define hidden swiftcc void @_TF28class_resilience_objc_armv7k29testConstantDirectFieldAccessFCS_23FixedLayoutObjCSubclassT_(%C28class_resilience_objc_armv7k23FixedLayoutObjCSubclass*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC28class_resilience_objc_armv7k23FixedLayoutObjCSubclass5fieldVs5Int32
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %C28class_resilience_objc_armv7k23FixedLayoutObjCSubclass* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: store i32 10, i32* [[PAYLOAD_ADDR]]

func testConstantDirectFieldAccess(_ o: FixedLayoutObjCSubclass) {
  o.field = 10
}

public class NonFixedLayoutObjCSubclass : NSCoder {
  // This field uses non-constant direct access because NSCoder has resilient
  // layout.
  public final var field: Int32 = 0
}

// CHECK-LABEL: define hidden swiftcc void @_TF28class_resilience_objc_armv7k32testNonConstantDirectFieldAccessFCS_26NonFixedLayoutObjCSubclassT_(%C28class_resilience_objc_armv7k26NonFixedLayoutObjCSubclass*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_TWvdvC28class_resilience_objc_armv7k26NonFixedLayoutObjCSubclass5fieldVs5Int32
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %C28class_resilience_objc_armv7k26NonFixedLayoutObjCSubclass* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_ADDR]], i32 0, i32 0
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

// CHECK-LABEL: define hidden swiftcc void @_TF28class_resilience_objc_armv7k31testConstantIndirectFieldAccessurFGCS_19GenericObjCSubclassx_T_(%C28class_resilience_objc_armv7k19GenericObjCSubclass*)

// FIXME: we could eliminate the unnecessary isa load by lazily emitting
// metadata sources in EmitPolymorphicParameters

// CHECK:      call %objc_class* @object_getClass
// CHECK:      [[ADDR:%.*]] = bitcast %C28class_resilience_objc_armv7k19GenericObjCSubclass* %0 to %objc_object*
// CHECK-NEXT: [[KLASS:%.*]] = call %objc_class* @object_getClass(%objc_object* [[ADDR]])
// CHECK-NEXT: [[ISA:%.*]] = bitcast %objc_class* [[KLASS]] to %swift.type*
// CHECK-NEXT: [[ISA_ADDR:%.*]] = bitcast %swift.type* [[ISA]] to [[INT]]*
// CHECK-NEXT: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[ISA_ADDR]], [[INT]] 16
// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_ADDR:%.*]]
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %C28class_resilience_objc_armv7k19GenericObjCSubclass* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Vs5Int32*
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Vs5Int32, %Vs5Int32* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: store i32 10, i32* [[PAYLOAD_ADDR]]

func testConstantIndirectFieldAccess<T>(_ o: GenericObjCSubclass<T>) {
  // This field uses constant indirect access because NSCoder has resilient
  // layout. Non-constant indirect is never needed for Objective-C classes
  // because the field offset vector only contains Swift field offsets.
  o.field = 10
}
