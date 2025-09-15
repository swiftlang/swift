// RUN: %target-swift-emit-silgen -module-name main %s -define-availability 'InlineArray 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999' | %FileCheck %s

@available(InlineArray 0.1, *)
struct S {
    var a: InlineArray<40, Int> = .init(repeating: 0)

    // CHECK-LABEL: sil {{.*}} @$s{{.*}}1SV1f
    mutating func f(x: inout Int, y: Int) {
        // CHECK: bb0({{.*}}, [[SELF:%.*]] : $*S):
        // CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[SELF]]
        // CHECK:   [[ADDRESSOR:%.*]] = function_ref @$s{{.*}}11InlineArrayV
        // CHECK:   [[PTR:%.*]] = apply [[ADDRESSOR]]
        // CHECK:   [[RAWPTR:%.*]] = struct_extract [[PTR]]
        // CHECK:   [[ADDR:%.*]] = pointer_to_address [[RAWPTR]]
        // CHECK:   [[DEP:%.*]] = mark_dependence [unresolved] [[ADDR]]
        // CHECK:   [[ADDR_ACCESS:%.*]] = begin_access [read] [unsafe] [[DEP]]
        // CHECK:   load [trivial] [[ADDR_ACCESS]]
        // CHECK:   end_access [[ADDR_ACCESS]]
        // CHECK:   end_access [[ACCESS]]
        x += a[y]
    }
}

@available(InlineArray 0.1, *)
final class C {
    final var a: InlineArray<40, Int> = .init(repeating: 0)

    // CHECK-LABEL: sil {{.*}} @$s{{.*}}1CC1f
    func f(x: inout Int, y: Int) {
        // CHECK: bb0({{.*}}, [[SELF:%.*]] : @guaranteed $C):
        // CHECK:   [[FIELD:%.*]] = ref_element_addr [[SELF]]
        // CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[FIELD]]
        // CHECK:   [[ADDRESSOR:%.*]] = function_ref @$s{{.*}}11InlineArrayV
        // CHECK:   [[PTR:%.*]] = apply [[ADDRESSOR]]
        // CHECK:   [[RAWPTR:%.*]] = struct_extract [[PTR]]
        // CHECK:   [[ADDR:%.*]] = pointer_to_address [[RAWPTR]]
        // CHECK:   [[DEP:%.*]] = mark_dependence [unresolved] [[ADDR]]
        // CHECK:   [[ADDR_ACCESS:%.*]] = begin_access [read] [unsafe] [[DEP]]
        // CHECK:   load [trivial] [[ADDR_ACCESS]]
        // CHECK:   end_access [[ADDR_ACCESS]]
        // CHECK:   end_access [[ACCESS]]
        x += a[y]
    }
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}6tupleF
@available(InlineArray 0.1, *)
func tupleF(tuple: inout (Int, InlineArray<40, Int>), x: inout Int, y: Int) {
    // CHECK: bb0([[TUPLE:%.*]] : $*(Int, InlineArray<40, Int>)
    // CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[TUPLE]]
    // CHECK:   [[ADDRESSOR:%.*]] = function_ref @$s{{.*}}11InlineArrayV
    // CHECK:   [[PTR:%.*]] = apply [[ADDRESSOR]]
    // CHECK:   [[RAWPTR:%.*]] = struct_extract [[PTR]]
    // CHECK:   [[ADDR:%.*]] = pointer_to_address [[RAWPTR]]
    // CHECK:   [[DEP:%.*]] = mark_dependence [unresolved] [[ADDR]]
    // CHECK:   [[ADDR_ACCESS:%.*]] = begin_access [read] [unsafe] [[DEP]]
    // CHECK:   load [trivial] [[ADDR_ACCESS]]
    // CHECK:   end_access [[ADDR_ACCESS]]
    // CHECK:   end_access [[ACCESS]]
    x += tuple.1[y]
}

@available(InlineArray 0.1, *)
protocol P {
    var a: InlineArray<40, Int> { get set }
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}12existentialF
@available(InlineArray 0.1, *)
func existentialF(e: inout P, x: inout Int, y: Int) {
    // CHECK:   [[TEMP:%.*]] = alloc_stack $InlineArray
    // CHECK:   [[ADDRESSOR:%.*]] = function_ref @$s{{.*}}11InlineArrayV
    // CHECK:   [[PTR:%.*]] = apply [[ADDRESSOR]]
    // CHECK:   [[RAWPTR:%.*]] = struct_extract [[PTR]]
    // CHECK:   [[ADDR:%.*]] = pointer_to_address [[RAWPTR]]
    // CHECK:   [[DEP:%.*]] = mark_dependence [unresolved] [[ADDR]]
    // CHECK:   [[ADDR_ACCESS:%.*]] = begin_access [read] [unsafe] [[DEP]]
    // CHECK:   load [trivial] [[ADDR_ACCESS]]
    // CHECK:   end_access [[ADDR_ACCESS]]
    // CHECK:   dealloc_stack [[TEMP]]
    x += e.a[y]
}
