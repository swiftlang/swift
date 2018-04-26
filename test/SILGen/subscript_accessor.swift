// RUN: %target-swift-frontend -enable-sil-ownership -emit-silgen -primary-file %s | %FileCheck %s

// CHECK-LABEL: sil private [transparent] @$S18subscript_accessor1XVxSgycimytfU_
// CHECK: [[SETTER:%.*]] = function_ref @$S18subscript_accessor1XVxSgycis
// CHECK-NEXT: apply [[SETTER]]<T>
struct X<T> {
  subscript () -> T? {
    get {
      return nil
    }
    set { }
  }
}
