// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// is_same_metatype builtin is no longer used due to rdar://145707064 (Builtin.is_same_metatype should support noncopyable/nonescapable types)
// XFAIL: rdar145707064

// Check type equality related optimizations.

// CHECK-LABEL: sil @{{.*}}areEqualTypes1{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areEqualTypes1{{.*}}'
public func areEqualTypes1<T1, T2>(_ t1: T1.Type, _ t2: T2.Type) -> Bool {
    return t1 == t2
}

// CHECK-LABEL: sil @{{.*}}areEqualTypes2{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areEqualTypes2{{.*}}'
public func areEqualTypes2<T1, T2>(_ t1: T1.Type, _ t2o: T2.Type?) -> Bool {
    return t1 == t2o
}

// CHECK-LABEL: sil @{{.*}}areEqualTypes3{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areEqualTypes3{{.*}}'
public func areEqualTypes3<T1, T2>(_ t1o: T1.Type?, _ t2: T2.Type) -> Bool {
    return t1o == t2
}

// CHECK-LABEL: sil @{{.*}}areEqualTypes4{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areEqualTypes4{{.*}}'
public func areEqualTypes4<T1, T2>(_ t1o: T1.Type?, _ t2o: T2.Type?) -> Bool {
    return t1o == t2o
}

// CHECK-LABEL: sil @{{.*}}areEqualTypes5{{.*}} : $@convention(thin)
// CHECK-NOT: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areEqualTypes5{{.*}}'
public func areEqualTypes5<T1>(_ t1o: T1.Type?) -> Bool {
    return t1o == nil
}

// CHECK-LABEL: sil @{{.*}}areEqualTypes6{{.*}} : $@convention(thin)
// CHECK-NOT: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areEqualTypes6{{.*}}'
public func areEqualTypes6<T1>(_ t1o: T1.Type?) -> Bool {
    return nil == t1o
}

// CHECK-LABEL: sil @{{.*}}areNotEqualTypes1{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areNotEqualTypes1{{.*}}'
public func areNotEqualTypes1<T1, T2>(_ t1: T1.Type, _ t2: T2.Type) -> Bool {
    return t1 != t2
}

// CHECK-LABEL: sil @{{.*}}areNotEqualTypes2{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areNotEqualTypes2{{.*}}'
public func areNotEqualTypes2<T1, T2>(_ t1: T1.Type, _ t2o: T2.Type?) -> Bool {
    return t1 != t2o
}

// CHECK-LABEL: sil @{{.*}}areNotEqualTypes3{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areNotEqualTypes3{{.*}}'
public func areNotEqualTypes3<T1, T2>(_ t1o: T1.Type?, _ t2: T2.Type) -> Bool {
    return t1o != t2
}

// CHECK-LABEL: sil @{{.*}}areNotEqualTypes4{{.*}} : $@convention(thin)
// CHECK: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areNotEqualTypes4{{.*}}'
public func areNotEqualTypes4<T1, T2>(_ t1o: T1.Type?, _ t2o: T2.Type?) -> Bool {
    return t1o != t2o
}

// CHECK-LABEL: sil @{{.*}}areNotEqualTypes5{{.*}} : $@convention(thin)
// CHECK-NOT: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areNotEqualTypes5{{.*}}'
public func areNotEqualTypes5<T1>(_ t1o: T1.Type?) -> Bool {
    return t1o != nil
}

// CHECK-LABEL: sil @{{.*}}areNotEqualTypes6{{.*}} : $@convention(thin)
// CHECK-NOT: builtin "is_same_metatype"
// CHECK: // end sil function '{{.*}}areNotEqualTypes6{{.*}}'
public func areNotEqualTypes6<T1>(_ t1o: T1.Type?) -> Bool {
    return nil != t1o
}
