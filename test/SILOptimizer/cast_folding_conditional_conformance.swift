// RUN: %target-swift-frontend %s -emit-sil -O -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -o /dev/null

// rdar://problem/38694450

protocol P {}
extension Array: P where Element: P {}
struct X {}
// CHECK-LABEL: sil @$S36cast_folding_conditional_conformance5arrayyyF : $@convention(thin) () -> () {
public func array() {
    // CHECK: unconditional_checked_cast_addr Array<X> in {{%[0-9]*}} : $*Array<X> to P in {{%[0-9]*}} : $*P
    var x = [X()] as! P
}

struct Y<T> {}
extension Y: P where T: P {}
// CHECK-LABEL: sil @$S36cast_folding_conditional_conformance3fooyyxmlF : $@convention(thin) <T> (@thick T.Type) -> () {
public func foo<T>(_: T.Type) {
    // CHECK: unconditional_checked_cast_addr Y<T> in {{%[0-9]*}} : $*Y<T> to P in {{%[0-9]*}} : $*P
    var x = Y<T>() as! P
}

