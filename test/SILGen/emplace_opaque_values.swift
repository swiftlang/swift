// RUN: %target-swift-frontend -enable-experimental-feature BuiltinModule -emit-silgen -sil-verify-all -enable-sil-opaque-values %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

// With opaque values, a generic error is thrown as a direct value instead of
// through an indirect error result, so the error block of the `try_apply` that
// `Builtin.emplace` emits receives the error as an argument.

import Builtin

struct Loadable {
    var x: AnyObject
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_throws_generic
func emplace_throws_generic<E: Error>(_ body: (Builtin.RawPointer) throws(E) -> ()) throws(E) -> Loadable {
    // CHECK: [[TMP:%.*]] = alloc_stack $Loadable
    // CHECK-NEXT: builtin "prepareInitialization"([[TMP]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[TMP]]
    // CHECK-NEXT: try_apply {{.*}}([[PTR]]) {{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
    // CHECK: [[ERROR]]([[ERR:%.*]] : @owned $E):
    // CHECK-NEXT: end_lifetime [[TMP]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    // CHECK-NEXT: end_formal_scope
    // CHECK-NEXT: throw [[ERR]]
    // CHECK: [[NORMAL]]({{.*}}):
    // CHECK-NEXT: [[RESULT:%.*]] = load [take] [[TMP]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    // CHECK-NEXT: end_formal_scope
    // CHECK-NEXT: return [[RESULT]]
    return try Builtin.emplace(body)
}

// The error has to go to the enclosing `catch`, not out of the function.
// CHECK-LABEL: sil {{.*}} @$s{{.*}}_caught
func emplace_caught<E: Error>(_ body: (Builtin.RawPointer) throws(E) -> ()) throws(E) -> Bool {
    // CHECK: try_apply {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
    // CHECK: [[ERROR]]([[ERR:%.*]] : @owned $E):
    // CHECK-NEXT: end_lifetime
    // CHECK-NEXT: dealloc_stack
    // CHECK-NOT: throw
    // CHECK: integer_literal $Builtin.Int1, 0
    // CHECK: } // end sil function
    do throws(E) {
        let _: Loadable = try Builtin.emplace(body)
        return true
    } catch {
        return false
    }
}

// The enclosing function doesn't throw at all, so there is no error result for
// the closure to forward its error into.
// CHECK-LABEL: sil {{.*}} @$s{{.*}}_caught_nonthrowing
func emplace_caught_nonthrowing<E: Error>(_ body: (Builtin.RawPointer) throws(E) -> ()) -> Bool {
    // CHECK: try_apply {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
    // CHECK: [[ERROR]]([[ERR:%.*]] : @owned $E):
    // CHECK-NEXT: end_lifetime
    // CHECK-NEXT: dealloc_stack
    // CHECK-NOT: throw
    // CHECK: } // end sil function
    do throws(E) {
        let _: Loadable = try Builtin.emplace(body)
        return true
    } catch {
        return false
    }
}
