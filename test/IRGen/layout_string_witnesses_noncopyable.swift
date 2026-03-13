// RUN: %target-swift-frontend -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation -emit-ir -module-name Foo %s | %FileCheck %s

// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

// CHECK-NOT: @"$s3Foo7GenericVWV" = {{.*}}ptr @swift_cvw{{.*$}}
struct Generic<T: ~Copyable>: ~Copyable {
    let x: T
    let y: Int
}

// CHECK-NOT: @"$s3Foo13SinglePayloadOWV" = {{.*}}ptr @swift_cvw{{.*$}}
enum SinglePayload: ~Copyable {
    case x(AnyObject)
    case y
}

// CHECK-NOT: @"$s3Foo12MultiPayloadOWV" = {{.*}}ptr @swift_cvw{{.*$}}
enum MultiPayload: ~Copyable {
    case x(AnyObject)
    case y(AnyObject)
}

// CHECK-NOT: @"$s3Foo20SinglePayloadGenericOWV" = {{.*}}ptr @swift_cvw{{.*$}}
enum SinglePayloadGeneric<T>: ~Copyable {
    case x(T)
    case y
}

// CHECK-NOT: @"$s3Foo19MultiPayloadGenericOWV" = {{.*}}ptr @swift_cvw{{.*$}}
enum MultiPayloadGeneric<T>: ~Copyable {
    case x(T)
    case y(T)
}
