// RUN: %target-swift-emit-silgen -enable-experimental-feature AddressableParameters %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableParameters

protocol Gen {
    associatedtype Ass
    associatedtype Ind
}

struct Foo: Gen {
    typealias Ass = Bar
    typealias Ind = Int

    var bar: Bar

    var barAddressor: Bar {
        unsafeAddress { fatalError() }
    }

    var barSelfAddressor: Bar {
        @_addressableSelf
        unsafeAddress { fatalError() }
    }

    subscript(i: Int) -> Bar {
        unsafeAddress { fatalError() }
    }
}
struct Bar {
    var bas: Bas
}
struct Bas {
    var end: Int
}

extension Gen {
    var genAddressor: Ass {
        unsafeAddress { fatalError() }
    }

    var genSelfAddressor: Ass {
        @_addressableSelf
        unsafeAddress { fatalError() }
    }

    subscript(gen i: Ind) -> Ass {
        unsafeAddress { fatalError() }
    }
    subscript(genSelf i: Ind) -> Ass {
        @_addressableSelf
        unsafeAddress { fatalError() }
    }
}


func bas(_ bas: borrowing @_addressable Bas) { }

func bar(_ bar: borrowing @_addressable Bar) { }

// CHECK-LABEL: sil {{.*}} @${{.*}}19testAddressableBase
func testAddressableBase(_ foo: borrowing @_addressable Foo, i: Int) {
// CHECK: bb0(%0 : @noImplicitCopy $*Foo
    // CHECK: [[WRAP_BASE:%.*]] = copyable_to_moveonlywrapper_addr %0
    // CHECK: [[BASE:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[WRAP_BASE]]

    // CHECK: [[BAR:%.*]] = struct_element_addr [[BASE]], #Foo.bar
    // CHECK: [[UNWRAP_BAR:%.*]] = moveonlywrapper_to_copyable_addr [[BAR]]
    // CHECK: apply {{.*}}([[UNWRAP_BAR]])
    bar(foo.bar)

    // CHECK: [[BAR:%.*]] = struct_element_addr [[BASE]], #Foo.bar
    // CHECK: [[BAS:%.*]] = struct_element_addr [[BAR]], #Bar.bas
    // CHECK: [[UNWRAP_BAS:%.*]] = moveonlywrapper_to_copyable_addr [[BAS]]
    // CHECK: apply {{.*}}([[UNWRAP_BAS]])
    bas(foo.bar.bas)

    // non-addressable base:
    // CHECK: [[ADDRESSOR:%.*]] = function_ref @{{.*}}12barAddressor{{.*}}lu :
    // CHECK: [[ADDRESS:%.*]] = apply [[ADDRESSOR]](
    // CHECK: [[ADDRESS_RAW:%.*]] = struct_extract [[ADDRESS]], #UnsafePointer._rawValue
    // CHECK: [[ADDRESS_ADDR:%.*]] = pointer_to_address [[ADDRESS_RAW]]
    // CHECK: [[ADDRESS_DEP:%.*]] = mark_dependence {{.*}} [[ADDRESS_ADDR]]
    // CHECK: apply {{.*}}([[ADDRESS_DEP]])
    bar(foo.barAddressor)

    // addressable base:
    // CHECK: [[ADDRESSOR:%.*]] = function_ref @{{.*}}16barSelfAddressor{{.*}}lu :
    // CHECK: [[UNWRAP_FOO:%.*]] = moveonlywrapper_to_copyable_addr [[BASE]]
    // CHECK: [[ADDRESS:%.*]] = apply [[ADDRESSOR]]([[UNWRAP_FOO]])
    // CHECK: [[ADDRESS_RAW:%.*]] = struct_extract [[ADDRESS]], #UnsafePointer._rawValue
    // CHECK: [[ADDRESS_ADDR:%.*]] = pointer_to_address [[ADDRESS_RAW]]
    // CHECK: [[ADDRESS_DEP:%.*]] = mark_dependence {{.*}} [[ADDRESS_ADDR]]
    // CHECK: apply {{.*}}([[ADDRESS_DEP]])
    bar(foo.barSelfAddressor)

    // non-addressable base:
    // CHECK: [[ADDRESSOR:%.*]] = function_ref @$s19addressable_members3FooVyAA3BarVSicilu :
    // CHECK: [[ADDRESS:%.*]] = apply [[ADDRESSOR]](
    // CHECK: [[ADDRESS_RAW:%.*]] = struct_extract [[ADDRESS]], #UnsafePointer._rawValue
    // CHECK: [[ADDRESS_ADDR:%.*]] = pointer_to_address [[ADDRESS_RAW]]
    // CHECK: [[ADDRESS_DEP:%.*]] = mark_dependence {{.*}} [[ADDRESS_ADDR]]
    // CHECK: apply {{.*}}([[ADDRESS_DEP]])
    bar(foo[i])
    // CHECK: [[ADDRESSOR:%.*]] = function_ref @$s19addressable_members3FooVyAA3BarVSicilu :
    // CHECK: [[ADDRESS:%.*]] = apply [[ADDRESSOR]](
    // CHECK: [[ADDRESS_RAW:%.*]] = struct_extract [[ADDRESS]], #UnsafePointer._rawValue
    // CHECK: [[ADDRESS_ADDR:%.*]] = pointer_to_address [[ADDRESS_RAW]]
    // CHECK: [[ADDRESS_DEP:%.*]] = mark_dependence {{.*}} [[ADDRESS_ADDR]]
    // CHECK: [[BAS_ADDRESS:%.*]] = struct_element_addr [[ADDRESS_DEP]], #Bar.bas
    // CHECK: apply {{.*}}([[BAS_ADDRESS]])
    bas(foo[i].bas)

    // Temporary materialization outlives addressor call, even if it's not
    // expected to be addressable.
    // CHECK: [[TEMP:%.*]] = alloc_stack $Foo
    // CHECK: [[ADDRESS:%.*]] = apply {{.*}}<Foo>([[TEMP]])
    // CHECK: [[ADDRESS_RAW:%.*]] = struct_extract [[ADDRESS]], #UnsafePointer._rawValue
    // CHECK: [[ADDRESS_ADDR:%.*]] = pointer_to_address [[ADDRESS_RAW]]
    // CHECK: mark_dependence [unresolved] [[ADDRESS_ADDR]] on [[TEMP]]
    // CHECK: dealloc_stack [[TEMP]]
    bar(foo.genAddressor)
    bar(foo[gen: i])

    bar(foo.genSelfAddressor)
    bar(foo[genSelf: i])
}

func temporaryFoo() -> Foo { fatalError() }

func testTemporaryBase(_ foo: borrowing @_addressable Foo, i: Int) {
    bar(temporaryFoo().bar)
    bas(temporaryFoo().bar.bas)

    bar(temporaryFoo().barAddressor)

    bar(temporaryFoo().barSelfAddressor)

    bar(temporaryFoo()[i])
    bas(temporaryFoo()[i].bas)

    bar(temporaryFoo().genAddressor)
    bar(temporaryFoo()[gen: i])
}

func testInoutBase(_ foo: inout Foo) {
    bar(foo.bar)
    bas(foo.bar.bas)
}
