// RUN: %target-swift-emit-silgen -enable-experimental-feature AddressableTypes -enable-experimental-feature LifetimeDependence %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence

@_addressableForDependencies
struct Foo { var x: String }

struct Bar { var foo: Foo }

struct Dep: ~Escapable {
    var x: Int = 0

    @lifetime(immortal)
    init() { }
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12dependencyOn3foo{{.*}} :
// CHECK-SAME:    (@in_guaranteed Foo) -> 
@lifetime(borrow foo)
func dependencyOn(foo: Foo) -> Dep {
    // CHECK-NOT: load_borrow
}
// CHECK-LABEL: sil {{.*}}@$s{{.*}}12dependencyOn3bar{{.*}} :
// CHECK-SAME:    (@in_guaranteed Bar) -> 
@lifetime(borrow bar)
func dependencyOn(bar: Bar) -> Dep {
    // CHECK-NOT: load_borrow
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12dependencyOn3foo6butNot{{.*}} :
// CHECK-SAME:    (@in_guaranteed Foo, @guaranteed Foo) -> 
@lifetime(borrow foo)
func dependencyOn(foo: Foo, butNot _: Foo) -> Dep {
    // CHECK: bb0(%0 : $*Foo,
    // CHECK:   apply {{.*}}(%0)
    return dependencyOn(foo: foo)
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12dependencyOn3bar6butNot{{.*}} :
// CHECK-SAME:    (@in_guaranteed Bar, @guaranteed Bar) -> 
@lifetime(borrow bar)
func dependencyOn(bar: Bar, butNot _: Bar) -> Dep {
    // CHECK: bb0(%0 : $*Bar,
    // CHECK:   apply {{.*}}(%0)
    return dependencyOn(bar: bar)
}

extension Foo {
    // CHECK-LABEL: sil {{.*}}@$s{{.*}}3FooV16dependencyOnSelf{{.*}} :
    // CHECK-SAME:    (@in_guaranteed Foo) -> 
    @lifetime(borrow self)
    func dependencyOnSelf() -> Dep {
        // CHECK-NOT: load_borrow
    }

    // CHECK-LABEL: sil {{.*}}@$s{{.*}}3FooV16dependencyOnSelf6butNot{{.*}} :
    // CHECK-SAME:    (@guaranteed Foo, @in_guaranteed Foo) -> 
    @lifetime(borrow self)
    func dependencyOnSelf(butNot _: Foo) -> Dep {
        // CHECK: bb0({{.*}}, %1 : $*Foo)
        // CHECK:   apply {{.*}}(%1)
        return dependencyOnSelf()
    }

    // CHECK-LABEL: sil {{.*}}@$s{{.*}}3FooV19dependencyNotOnSelf{{.*}} :
    // CHECK-SAME:    (@in_guaranteed Foo, @guaranteed Foo) -> 
    @lifetime(borrow foo)
    func dependencyNotOnSelf(butOn foo: Foo) -> Dep {
        // CHECK: bb0(%0 : $*Foo,
        // CHECK:   apply {{.*}}(%0)
        return foo.dependencyOnSelf()
    }
}

extension Bar {
    // CHECK-LABEL: sil {{.*}}@$s{{.*}}3BarV16dependencyOnSelf{{.*}} :
    // CHECK-SAME:    (@in_guaranteed Bar) -> 
    @lifetime(borrow self)
    func dependencyOnSelf() -> Dep {
    }

    // CHECK-LABEL: sil {{.*}}@$s{{.*}}3BarV16dependencyOnSelf6butNot{{.*}} :
    // CHECK-SAME:    (@guaranteed Bar, @in_guaranteed Bar) -> 
    @lifetime(borrow self)
    func dependencyOnSelf(butNot _: Bar) -> Dep {
        // CHECK: bb0({{.*}}, %1 : $*Bar)
        // CHECK:   apply {{.*}}(%1)
        return dependencyOnSelf()
    }

    // CHECK-LABEL: sil {{.*}}@$s{{.*}}3BarV19dependencyNotOnSelf{{.*}} :
    // CHECK-SAME:    (@in_guaranteed Bar, @guaranteed Bar) -> 
    @lifetime(borrow bar)
    func dependencyNotOnSelf(butOn bar: Bar) -> Dep {
        // CHECK: bb0(%0 : $*Bar,
        // CHECK:   apply {{.*}}(%0)
        return bar.dependencyOnSelf()
    }
}

// CHECK-LABEL: sil {{.*}}@$s28addressable_for_dependencies14defaulArgument1iAA3DepVSi_tFfA_ :
// CHECK-SAME: $@convention(thin) () -> Int {

// CHECK-LABEL: sil {{.*}}@$s28addressable_for_dependencies14defaulArgument1iAA3DepVSi_tF :
// CHECK-SAME: $@convention(thin) (Int) -> @lifetime(borrow 0) @owned Dep {
@lifetime(borrow i)
func defaulArgument(i: Int = 0) -> Dep {}
