// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -new-mangling-for-tests -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// XFAIL: CPU=armv7k

// CHECK: %swift.type = type { [[INT:i32|i64]] }

import Foundation

public class FixedLayoutObjCSubclass : NSObject {
  // This field could use constant direct access because NSObject has
  // fixed layout, but we don't allow that right now.
  public final var field: Int32 = 0
};

// CHECK-LABEL: define hidden swiftcc void @_T021class_resilience_objc29testConstantDirectFieldAccessyAA23FixedLayoutObjCSubclassCF(%T21class_resilience_objc23FixedLayoutObjCSubclassC*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_T021class_resilience_objc23FixedLayoutObjCSubclassC5fields5Int32VvWvd
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T21class_resilience_objc23FixedLayoutObjCSubclassC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
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

// CHECK-LABEL: define hidden swiftcc void @_T021class_resilience_objc32testNonConstantDirectFieldAccessyAA0E23FixedLayoutObjCSubclassCF(%T21class_resilience_objc26NonFixedLayoutObjCSubclassC*)
// CHECK:      [[OFFSET:%.*]] = load [[INT]], [[INT]]* @_T021class_resilience_objc26NonFixedLayoutObjCSubclassC5fields5Int32VvWvd
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T21class_resilience_objc26NonFixedLayoutObjCSubclassC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
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

// CHECK-LABEL: define hidden swiftcc void @_T021class_resilience_objc31testConstantIndirectFieldAccessyAA19GenericObjCSubclassCyxGlF(%T21class_resilience_objc19GenericObjCSubclassC*)

// FIXME: we could eliminate the unnecessary isa load by lazily emitting
// metadata sources in EmitPolymorphicParameters

// CHECK:         bitcast %T21class_resilience_objc19GenericObjCSubclassC* %0

// CHECK-32:      [[ADDR:%.*]] = bitcast %T21class_resilience_objc19GenericObjCSubclassC* %0 to %swift.type**
// CHECK-32-NEXT: [[ISA:%.*]] = load %swift.type*, %swift.type** [[ADDR]]

// CHECK-64:      [[ADDR:%.*]] = bitcast %T21class_resilience_objc19GenericObjCSubclassC* %0 to [[INT]]*
// CHECK-64-NEXT: [[ISA:%.*]] = load [[INT]], [[INT]]* [[ADDR]]
// CHECK-64-NEXT: [[ISA_MASK:%.*]] = load [[INT]], [[INT]]* @swift_isaMask
// CHECK-64-NEXT: [[ISA_VALUE:%.*]] = and [[INT]] [[ISA]], [[ISA_MASK]]
// CHECK-64-NEXT: [[ISA:%.*]] = inttoptr [[INT]] [[ISA_VALUE]] to %swift.type*

// CHECK-NEXT:    [[ISA_ADDR:%.*]] = bitcast %swift.type* [[ISA]] to [[INT]]*

// CHECK-32-NEXT: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[ISA_ADDR]], [[INT]] 16

// CHECK-64-NEXT: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds [[INT]], [[INT]]* [[ISA_ADDR]], [[INT]] 13

// CHECK-NEXT: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_ADDR:%.*]]
// CHECK-NEXT: [[OBJECT:%.*]] = bitcast %T21class_resilience_objc19GenericObjCSubclassC* %0 to i8*
// CHECK-NEXT: [[ADDR:%.*]] = getelementptr inbounds i8, i8* [[OBJECT]], [[INT]] [[FIELD_OFFSET]]
// CHECK-NEXT: [[FIELD_ADDR:%.*]] = bitcast i8* [[ADDR]] to %Ts5Int32V*
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: store i32 10, i32* [[PAYLOAD_ADDR]]

func testConstantIndirectFieldAccess<T>(_ o: GenericObjCSubclass<T>) {
  // This field uses constant indirect access because NSCoder has resilient
  // layout. Non-constant indirect is never needed for Objective-C classes
  // because the field offset vector only contains Swift field offsets.
  o.field = 10
}
