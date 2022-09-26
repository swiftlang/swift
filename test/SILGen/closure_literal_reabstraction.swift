// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func gen<T, U>(f: (T) throws -> U) {}

prefix operator !=<

struct Butt {
    var x: Int

    func getX(plus y: Int) -> Int { return x + y }

    static func create(x: Int) -> Butt { return Butt(x: x) }

    static prefix func !=< (a: Butt) -> Butt { return a }
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
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[KP_ARG:%.*]] = copy_value {{.*}} $KeyPath
// CHECK:         [[CLOSURE:%.*]] = partial_apply {{.*}}[[CLOSURE_FN]]([[KP_ARG]])
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape {{.*}} [[CLOSURE]]
// CHECK:         apply {{.*}}<Butt, Int>([[CLOSURE_NE]])
func reabstractKeyPathFunctionArgument() {
    gen(f: \Butt.x)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractInitializerRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE_FN]]
// CHECK:         apply {{.*}}<Int, Butt>([[CLOSURE]])
func reabstractInitializerRef() {
    gen(f: Butt.init)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractInitializerRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE_FN]]
// CHECK:         apply {{.*}}<Int, Butt>([[CLOSURE]])
func reabstractStaticMemberRef() {
    gen(f: Butt.create)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractMemberOperatorRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE_FN]]
// CHECK:         apply {{.*}}<Butt, Butt>([[CLOSURE]])
func reabstractMemberOperatorRef() {
    gen(f: !=<)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractCoercedMemberOperatorRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE_FN]]
// CHECK:         apply {{.*}}<Butt, Butt>([[CLOSURE]])
func reabstractCoercedMemberOperatorRef() {
    gen(f: (!=<) as (Butt) -> Butt)
}

// TODO
func reabstractInstanceMethodRef(instance: Butt) {
    gen(f: instance.getX)
}
