// RUN: %target-swift-frontend -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation -emit-ir -module-name Foo %s | %FileCheck %s

// CHECK-NOT: @"$s3Foo7GenericVWV" = {{.*}}ptr @swift_generic{{.*$}}
struct Generic<T: ~Copyable>: ~Copyable {
    let x: T
    let y: Int
}

// CHECK-NOT: @"$s3Foo13SinglePayloadOWV" = {{.*}}ptr @swift_generic{{.*$}}
enum SinglePayload: ~Copyable {
    case x(AnyObject)
    case y
}

// CHECK-NOT: @"$s3Foo12MultiPayloadOWV" = {{.*}}ptr @swift_generic{{.*$}}
enum MultiPayload: ~Copyable {
    case x(AnyObject)
    case y(AnyObject)
}

// CHECK-NOT: @"$s3Foo20SinglePayloadGenericOWV" = {{.*}}ptr @swift_generic{{.*$}}
enum SinglePayloadGeneric<T>: ~Copyable {
    case x(T)
    case y
}

// CHECK-NOT: @"$s3Foo19MultiPayloadGenericOWV" = {{.*}}ptr @swift_generic{{.*$}}
enum MultiPayloadGeneric<T>: ~Copyable {
    case x(T)
    case y(T)
}
