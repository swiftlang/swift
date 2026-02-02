// RUN: %target-swift-emit-silgen -import-objc-header %S/../Inputs/objc_direct.h -o - %s | %FileCheck %s

// REQUIRES: objc_interop

// NOTE: `[[BYTE01]]` should match the byte `0b01`.

func markUsed<T>(_ t: T) {}

protocol BarProtocol {
    func directProtocolMethod() -> String!
}

extension Bar: BarProtocol {}

let bar = Bar()
markUsed(Bar(value: 0))
// CHECK: function_ref @$sSo3BarC5valueABSgs5Int32V_tcfC
markUsed(Bar.init(value: 0))
// CHECK: function_ref @$sSo3BarC5valueABSgs5Int32V_tcfC

bar.directProperty = 123
// CHECK: function_ref @$sSo3BarC14directPropertys5Int32VvsTo

markUsed(bar.directProperty)
// CHECK: function_ref @$sSo3BarC14directPropertys5Int32VvgTo

bar.directProperty2 = 456
// CHECK: function_ref @$sSo3BarC15directProperty2s5Int32VvsTo

markUsed(bar.directProperty2)
// CHECK: function_ref @$sSo3BarC15directProperty2s5Int32VvgTo

bar[0] = 789
// CHECK: function_ref @$sSo3BarCys5Int32VADcisTo

markUsed(bar[0])
// CHECK: function_ref @$sSo3BarCys5Int32VADcigTo

markUsed(bar.directMethod())
// CHECK: function_ref @$sSo3BarC12directMethodSSSgyFTo

markUsed(bar.directMethod2())
// CHECK: function_ref @$sSo3BarC13directMethod2SSSgyFTo

markUsed(Bar.directClassMethod())
// CHECK: function_ref @$sSo3BarC17directClassMethodSSSgyFZTo

markUsed(Bar.directClassMethod2())
// CHECK: function_ref @$sSo3BarC18directClassMethod2SSSgyFZTo

markUsed(bar.directProtocolMethod())
// CHECK: function_ref @$sSo3BarC20directProtocolMethodSSSgyFTo

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo3BarC5valueABSgs5Int32V_tcfC : $@convention(method) (Int32, @thick Bar.Type) -> @owned Optional<Bar> {
// CHECK: {{.*}} = function_ref @$sSo3BarC5valueABSgs5Int32V_tcfcTO : $@convention(method)
// CHECK: } // end sil function '$sSo3BarC5valueABSgs5Int32V_tcfC'

// CHECK: sil [asmname "[[BYTE01:.]]-[Bar setDirectProperty:]"] @$sSo3BarC14directPropertys5Int32VvsTo : $@convention(objc_method)
// CHECK: sil [asmname "[[BYTE01]]-[Bar directProperty]"] @$sSo3BarC14directPropertys5Int32VvgTo : $@convention(objc_method)
// CHECK: sil [asmname "[[BYTE01]]-[Bar setDirectProperty2:]"] @$sSo3BarC15directProperty2s5Int32VvsTo : $@convention(objc_method)
// CHECK: sil [asmname "[[BYTE01]]-[Bar directProperty2]"] @$sSo3BarC15directProperty2s5Int32VvgTo : $@convention(objc_method)
// CHECK: sil [asmname "[[BYTE01]]-[Bar directMethod]"] [clang Bar.directMethod] @$sSo3BarC12directMethodSSSgyFTo : $@convention(objc_method)
// CHECK: sil [asmname "[[BYTE01]]-[Bar directMethod2]"] [clang Bar.directMethod2] @$sSo3BarC13directMethod2SSSgyFTo : $@convention(objc_method)
// CHECK: sil [asmname "[[BYTE01]]+[Bar directClassMethod]"] [clang Bar.directClassMethod] @$sSo3BarC17directClassMethodSSSgyFZTo : $@convention(objc_method)
// CHECK-DAG: sil [asmname "[[BYTE01]]+[Bar directClassMethod2]"] [clang Bar.directClassMethod2] @$sSo3BarC18directClassMethod2SSSgyFZTo : $@convention(objc_method)

// CHECK: sil [asmname "[[BYTE01]]-[Bar directProtocolMethod]"] [clang Bar.directProtocolMethod] @$sSo3BarC20directProtocolMethodSSSgyFTo : $@convention(objc_method)

// CHECK-LABEL: sil{{.*}}@$sSo3BarC5valueABSgs5Int32V_tcfcTO : $@convention(method) (Int32, @owned Bar) -> @owned Optional<Bar> {
// CHECK: function_ref @$sSo3BarC5valueABSgs5Int32V_tcfcTo : $@convention(objc_method)
// CHECK: } // end sil function '$sSo3BarC5valueABSgs5Int32V_tcfcTO'
