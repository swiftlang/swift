// RUN: %target-swift-frontend -enforce-exclusivity=unchecked -emit-sil %s | %FileCheck %s

struct Foo: ~Copyable {
    var x: Any
}

final class Bar {
    init() { fatalError() }

    // Ensure that noncopyable bindings still get [unsafe] exclusivity markers
    // and get checked properly by the move-only checker.

    // Bar.foo.setter:
    // CHECK-LABEL: sil {{.*}} @$s{{.*}}3BarC3foo{{.*}}vs
    // CHECK:         [[FIELD:%.*]] = ref_element_addr {{.*}}, #Bar.foo
    // CHECK:         [[FIELD_ACCESS:%.*]] = begin_access [modify] [unsafe] [[FIELD]]
    // CHECK-NEXT:    destroy_addr [[FIELD_ACCESS]]
    // CHECK-NEXT:    copy_addr [take] {{.*}} to [init] [[FIELD_ACCESS]]
    // CHECK-NEXT:    end_access [[FIELD_ACCESS]]
    // CHECK-NOT:     [[FIELD]]
    // CHECK-NOT:     [[FIELD_ACCESS]]
    // CHECK: } // end sil {{.*}} '$s{{.*}}3BarC3foo{{.*}}vs'

    var foo: Foo
}
