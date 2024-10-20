// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple -emit-ir %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: objc_interop

import _Concurrency

// CHECK: @_METACLASS_DATA__TtC37actor_class_forbid_objc_assoc_objects5Actor = internal constant { {{.*}} } { i32 [[METAFLAGS:1153]],
// CHECK: @_DATA__TtC37actor_class_forbid_objc_assoc_objects5Actor = internal constant { {{.*}} } { i32 [[OBJECTFLAGS:1152|1216]],
final actor Actor {
}

// CHECK: @_METACLASS_DATA__TtC37actor_class_forbid_objc_assoc_objects6Actor2 = internal constant { {{.*}} } { i32 [[METAFLAGS]],
// CHECK: @_DATA__TtC37actor_class_forbid_objc_assoc_objects6Actor2 = internal constant { {{.*}} } { i32 [[OBJECTFLAGS]],
actor Actor2 {
}

actor GenericActor<T> {
    var state: T
    init(state: T) { self.state = state }
}
