// RUN: %target-swift-emit-silgen -enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableParameters -enable-experimental-feature AddressableTypes %s | %FileCheck %s

// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_AddressableTypes

struct Foo: ~Escapable { }

struct Butt {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test1{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 1) @owned Foo
    @lifetime(borrow self)
    func test1(other: Butt) -> Foo {
    }

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test2{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 0) @owned Foo
    @lifetime(borrow other)
    func test2(other: Butt) -> Foo {
    }

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test3{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 4) @owned Foo
    @lifetime(borrow self)
    func test3(other: Butt, tuple: (Butt, Butt), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test4{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 0) @owned Foo
    @lifetime(borrow other)
    func test4(other: Butt, tuple: (Butt, Butt), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test5{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 1, borrow 2) @owned Foo
    @lifetime(borrow tuple)
    func test5(other: Butt, tuple: (Butt, Butt), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test6{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 3) @owned Foo
    @lifetime(borrow another)
    func test6(other: Butt, tuple: (Butt, Butt), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test7{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 2) @owned Foo
    @lifetime(borrow self)
    func test7(other: Butt, nothing: (), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test8{{.*}} : $
    // CHECK-SAME: -> @lifetime(immortal) @owned Foo
    @lifetime(borrow nothing)
    func test8(other: Butt, nothing: (), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}5test9{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 0) @owned Foo
    @lifetime(borrow other)
    func test9(other: Butt, nothing: (), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test10{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 1) @owned Foo
    @lifetime(borrow another)
    func test10(other: Butt, nothing: (), another: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test11{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow address 1) @owned Foo
    @_addressableSelf
    @lifetime(borrow self)
    func test11(other: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test12{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 0) @owned Foo
    @_addressableSelf
    @lifetime(borrow other)
    func test12(other: Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test13{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow 1) @owned Foo
    @lifetime(borrow self)
    func test13(other: @_addressable Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test14{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow address 0) @owned Foo
    @lifetime(borrow other)
    func test14(other: @_addressable Butt) -> Foo {}

    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test15{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow address 0) @owned Foo
    @lifetime(borrow tuple)
    func test15(tuple: @_addressable (Butt, Butt)) -> Foo {}
}

@_addressableForDependencies
struct AddressableForDeps {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test16{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow address_for_deps 3) @owned Foo
    @lifetime(borrow self)
    func test16(tuple: (AddressableForDeps, AddressableForDeps),
                other: AddressableForDeps) -> Foo {}

    // The dependency makes the tuple pass as a single indirect argument.
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test17{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow address_for_deps 0) @owned Foo
    @lifetime(borrow tuple)
    func test17(tuple: (AddressableForDeps, AddressableForDeps),
                other: AddressableForDeps) -> Foo {}

    // The tuple destructures as usual, but `other` is passed indirectly.
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}6test18{{.*}} : $
    // CHECK-SAME: -> @lifetime(borrow address_for_deps 2) @owned Foo
    @lifetime(borrow other)
    func test18(tuple: (AddressableForDeps, AddressableForDeps),
                other: AddressableForDeps) -> Foo {}
}

