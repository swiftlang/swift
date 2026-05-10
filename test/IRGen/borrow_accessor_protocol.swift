// RUN: %target-swift-frontend -emit-irgen -enable-library-evolution %s | %FileCheck %s --check-prefixes=CHECK-EVO
// REQUIRES: PTRSIZE=64

public protocol Proto {
    associatedtype Body
    var body: Body { borrow mutate }
}

// CHECK-EVO-LABEL: define{{.*}} swiftcc ptr @"$s24borrow_accessor_protocol5ProtoP4body4BodyQzvbTj"
// CHECK-EVO:         [[WITNESS_ADDR:%.*]] = getelementptr inbounds ptr, ptr %2,
// CHECK-EVO:         [[WITNESS:%.*]] = load ptr, ptr [[WITNESS_ADDR]]
// CHECK-EVO:         [[RESULT:%.*]] = call swiftcc ptr [[WITNESS]](ptr noalias swiftself %0, ptr %1, ptr %2)
// CHECK-EVO:         ret ptr [[RESULT]]

// CHECK-EVO-LABEL: define{{.*}} swiftcc ptr @"$s24borrow_accessor_protocol5ProtoP4body4BodyQzvzTj"
// CHECK-EVO:         [[WITNESS_ADDR:%.*]] = getelementptr inbounds ptr, ptr %2,
// CHECK-EVO:         [[WITNESS:%.*]] = load ptr, ptr [[WITNESS_ADDR]]
// CHECK-EVO:         [[RESULT:%.*]] = call swiftcc ptr [[WITNESS]](ptr swiftself %0, ptr %1, ptr %2)
// CHECK-EVO:         ret ptr [[RESULT]]

public protocol ConcreteProto {
    var value: Int { borrow mutate }
}

// CHECK-EVO-LABEL: define{{.*}} swiftcc i64 @"$s24borrow_accessor_protocol13ConcreteProtoP5valueSivbTj"
// CHECK-EVO:         [[WITNESS_ADDR:%.*]] = getelementptr inbounds ptr, ptr %2,
// CHECK-EVO:         [[WITNESS:%.*]] = load ptr, ptr [[WITNESS_ADDR]]
// CHECK-EVO:         [[RESULT:%.*]] = call swiftcc i64 [[WITNESS]](ptr noalias swiftself %0, ptr %1, ptr %2)
// CHECK-EVO:         ret i64 [[RESULT]]

// CHECK-EVO-LABEL: define{{.*}} swiftcc ptr @"$s24borrow_accessor_protocol13ConcreteProtoP5valueSivzTj"
// CHECK-EVO:         [[WITNESS_ADDR:%.*]] = getelementptr inbounds ptr, ptr %2,
// CHECK-EVO:         [[WITNESS:%.*]] = load ptr, ptr [[WITNESS_ADDR]]
// CHECK-EVO:         [[RESULT:%.*]] = call swiftcc ptr [[WITNESS]](ptr swiftself %0, ptr %1, ptr %2)
// CHECK-EVO:         ret ptr [[RESULT]]

public protocol SubscriptProto {
    associatedtype Element
    subscript(index: Int) -> Element { borrow mutate }
}

// CHECK-EVO-LABEL: define{{.*}} swiftcc ptr @"$s24borrow_accessor_protocol14SubscriptProtoP{{[^"]*}}icibTj"
// CHECK-EVO:         [[WITNESS_ADDR:%.*]] = getelementptr inbounds ptr, ptr %3,
// CHECK-EVO:         [[WITNESS:%.*]] = load ptr, ptr [[WITNESS_ADDR]]
// CHECK-EVO:         [[RESULT:%.*]] = call swiftcc ptr [[WITNESS]](i64 %0, ptr noalias swiftself %1, ptr %2, ptr %3)
// CHECK-EVO:         ret ptr [[RESULT]]

// CHECK-EVO-LABEL: define{{.*}} swiftcc ptr @"$s24borrow_accessor_protocol14SubscriptProtoP{{[^"]*}}icizTj"
// CHECK-EVO:         [[WITNESS_ADDR:%.*]] = getelementptr inbounds ptr, ptr %3,
// CHECK-EVO:         [[WITNESS:%.*]] = load ptr, ptr [[WITNESS_ADDR]]
// CHECK-EVO:         [[RESULT:%.*]] = call swiftcc ptr [[WITNESS]](i64 %0, ptr swiftself %1, ptr %2, ptr %3)
// CHECK-EVO:         ret ptr [[RESULT]]
