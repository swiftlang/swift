// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

func gen<T, U>(f: (T) throws -> U) {}

prefix operator !=<

struct Butt {
    var x: Int

    func getX(plus y: Int) -> Int { return x + y }

    static func create(x: Int) -> Butt { return Butt(x: x) }

    static prefix func !=< (a: Butt) -> Butt { return a }
}

class AbstractButt {
    required init(x: Int) {}

    class func create(x: Int) -> Self { return self.init(x: x) }
}

class AbstractGenericButt<T> {
    required init(c: Int) {}
    class func create(c: Int) -> Self { return self.init(c: c) }
}

func abstractButtFactory() -> AbstractButt.Type { return AbstractButt.self }

protocol Buttable {
    init(p: Int)
    static func create(p: Int) -> Self
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

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractDynamicMetatypeInitializerRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE]]
// CHECK:         apply {{.*}}<Int, AbstractButt>([[CLOSURE_NE]])
func reabstractDynamicMetatypeInitializerRef() {
    gen(f: abstractButtFactory().init)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractDynamicMetatypeStaticMemberRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE]]
// CHECK:         apply {{.*}}<Int, AbstractButt>([[CLOSURE_NE]])
func reabstractDynamicMetatypeStaticMemberRef() {
    gen(f: abstractButtFactory().create)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractGenericInitializerRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, T>([[CLOSURE_NE]])
func reabstractGenericInitializerRef<T: Buttable>(butt: T) {
    gen(f: T.init)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractGenericStaticMemberRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, T>([[CLOSURE_NE]])
func reabstractGenericStaticMemberRef<T: Buttable>(butt: T) {
    gen(f: T.create)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractDynamicGenericStaticMemberRef
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]<T>(%0)
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, T>([[CLOSURE_NE]])
func reabstractDynamicGenericStaticMemberRef<T: Buttable>(t: T.Type) {
    gen(f: t.create)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractExistentialInitializerRef
// CHECK:         [[MV:%.*]] = move_value [var_decl] %0 : $@thick any Buttable.Type
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[MV]])
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE]]
// CHECK:         apply {{.*}}<Int, any Buttable>([[CLOSURE_NE]])
func reabstractExistentialInitializerRef(butt: any Buttable.Type) {
    gen(f: butt.init)
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractExistentialStaticMemberRef
// CHECK:         [[MV:%.*]] = move_value [var_decl] %0 : $@thick any Buttable.Type
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[MV]])
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE]]
// CHECK:         apply {{.*}}<Int, any Buttable>([[CLOSURE_NE]])
func reabstractExistentialStaticMemberRef(butt: any Buttable.Type) {
    gen(f: butt.create)
}

// TODO: The move_value [var_decl]'s here are an unexpected consequence of how SILGen generates reabstractions.
//
// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractClassCompInitializerRefs
func reabstractClassCompInitializerRefs<T>(
    butt: any (AbstractGenericButt<T> & Buttable).Type
) {
// CHECK:         [[MV1:%.*]] = move_value [var_decl] %0
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]<T>([[MV1]])
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, any AbstractGenericButt<T> & Buttable>([[CLOSURE_NE]])
    gen(f: butt.init(c:))
// CHECK:         [[MV2:%.*]] = move_value [var_decl] %0
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u0_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]<T>([[MV2]])
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, any AbstractGenericButt<T> & Buttable>([[CLOSURE_NE]])
    gen(f: butt.init(p:))
}

// CHECK-LABEL: sil {{.*}} @{{.*}}reabstractClassCompStaticMemberRefs
func reabstractClassCompStaticMemberRefs<T>(
    butt: any (AbstractGenericButt<T> & Buttable).Type
) {
// CHECK:         [[MV1:%.*]] = move_value [var_decl] %0
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]<T>([[MV1]])
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, any AbstractGenericButt<T> & Buttable>([[CLOSURE_NE]])
    gen(f: butt.create(c:))
// CHECK:         [[MV2:%.*]] = move_value [var_decl] %0
// CHECK:         [[CLOSURE_FN:%.*]] = function_ref {{.*}}u0_
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]<T>([[MV2]])
// CHECK:         [[CLOSURE_C:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[CLOSURE_NE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_C]]
// CHECK:         apply {{.*}}<Int, any AbstractGenericButt<T> & Buttable>([[CLOSURE_NE]])
    gen(f: butt.create(p:))
}

// TODO

func reabstractInstanceMethodRef(instance: Butt) {
    gen(f: instance.getX)
}
