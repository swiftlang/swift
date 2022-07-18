// RUN: %target-swift-emit-silgen -import-objc-header %S/../Inputs/objc_direct.h -o - %s | %FileCheck %s

// REQUIRES: objc_interop

// NOTE: `[[BYTE01]]` should match the byte `0b01`.

func markUsed<T>(_ t: T) {}

protocol BarProtocol {
    func directProtocolMethod() -> String!
}

extension Bar: BarProtocol {}

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
let bar = Bar(value: 0)!
// CHECK: function_ref @$sSo3BarC5valueABSgs5Int32V_tcfC

let bar2 = Bar.init(value: 0)!
// CHECK: function_ref @$sSo3BarC5valueABSgs5Int32V_tcfC

bar.directProperty = 123
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directProperty!setter.foreign

markUsed(bar.directProperty)
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directProperty!getter.foreign

bar.directProperty2 = 456
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directProperty2!setter.foreign

markUsed(bar.directProperty2)
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directProperty2!getter.foreign

bar[0] = 789
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.subscript!setter.foreign

markUsed(bar[0])
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.subscript!getter.foreign

markUsed(bar.directMethod())
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directMethod!foreign

markUsed(bar.directMethod2())
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directMethod2!foreign

markUsed(Bar.directClassMethod())
// CHECK: objc_method [direct] %{{.*}} : $@objc_metatype Bar.Type, #Bar.directClassMethod!foreign

markUsed(Bar.directClassMethod2())
// CHECK: objc_method [direct] %{{.*}} : $@objc_metatype Bar.Type, #Bar.directClassMethod2!foreign

markUsed(bar.directProtocolMethod())
// CHECK: objc_method [direct] %{{.*}} : $Bar, #Bar.directProtocolMethod!foreign

// CHECK: } // end sil function 'main'

// CHECK: sil [clang Bar.directProtocolMethod] @[[BYTE01:.]]-[Bar directProtocolMethod] : $@convention(objc_method)

// CHECK: sil shared [serialized] [ossa] @$sSo3BarC5valueABSgs5Int32V_tcfC : $@convention(method) (Int32, @thick Bar.Type) -> @owned Optional<Bar> {
// CHECK: {{.*}} = function_ref @$sSo3BarC5valueABSgs5Int32V_tcfcTO : $@convention(method)
// CHECK: } // end sil function '$sSo3BarC5valueABSgs5Int32V_tcfC'

// CHECK-DAG: sil @[[BYTE01]]-[Bar setDirectProperty:] : $@convention(objc_method)
// CHECK-DAG: sil @[[BYTE01]]-[Bar directProperty] : $@convention(objc_method)
// CHECK-DAG: sil @[[BYTE01]]-[Bar setDirectProperty2:] : $@convention(objc_method)
// CHECK-DAG: sil @[[BYTE01]]-[Bar directProperty2] : $@convention(objc_method)
// CHECK-DAG: sil @[[BYTE01]]-[Bar objectAtIndexedSubscript:] : $@convention(objc_method)
// CHECK-DAG: sil @[[BYTE01]]-[Bar setObject:atIndexedSubscript:] : $@convention(objc_method)
// CHECK-DAG: sil [clang Bar.directMethod] @[[BYTE01]]-[Bar directMethod] : $@convention(objc_method)
// CHECK-DAG: sil [clang Bar.directMethod2] @[[BYTE01]]-[Bar directMethod2] : $@convention(objc_method)
// CHECK-DAG: sil [clang Bar.directClassMethod] @[[BYTE01]]+[Bar directClassMethod] : $@convention(objc_method)
// CHECK-DAG: sil [clang Bar.directClassMethod2] @[[BYTE01]]+[Bar directClassMethod2] : $@convention(objc_method)

// CHECK-LABEL: sil{{.*}}@$sSo3BarC5valueABSgs5Int32V_tcfcTO : $@convention(method) (Int32, @owned Bar) -> @owned Optional<Bar> {
// CHECK: {{.*}} = objc_method [direct] %1 : $Bar, #Bar.init!initializer.foreign
// CHECK: } // end sil function '$sSo3BarC5valueABSgs5Int32V_tcfcTO'
