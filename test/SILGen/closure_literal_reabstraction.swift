// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func gen<T, U>(f: (T) throws -> U) {}

struct Butt {
    var x: Int
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractCaptureListExprArgument
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}U_
// CHECK:         [[CLOSURE:%.*]] = partial_apply {{.*}}[[CLOSURE_FN]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape {{.*}} [[CLOSURE]]
// CHECK:         apply {{.*}}<Int, Int>([[CLOSURE_NE]])
func reabstractCaptureListExprArgument() {
    gen(f: {[x = 42] y in x + y })
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractKeyPathFunctionArgument
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}U_
// CHECK:         [[KP_ARG:%.*]] = copy_value {{.*}} $KeyPath
// CHECK:         [[CLOSURE:%.*]] = partial_apply {{.*}}[[CLOSURE_FN]]([[KP_ARG]])
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape {{.*}} [[CLOSURE]]
// CHECK:         apply {{.*}}<Butt, Int>([[CLOSURE_NE]])
func reabstractKeyPathFunctionArgument() {
    gen(f: \Butt.x)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractStaticMemberRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE_FN]]
// CHECK:         apply {{.*}}<Int, Butt>([[CLOSURE]])
func reabstractStaticMemberRef() {
    gen(f: Butt.init)
}
