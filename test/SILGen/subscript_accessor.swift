// RUN: %target-swift-frontend -enable-sil-ownership -O -emit-sil -primary-file %s | %FileCheck %s

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

  // CHECK: sil{{.*}}S18subscript_accessor9testXRead1xxAA1XVyxG_tlF
@_specialize(where T == (Int, Int))
func testXRead<T>(x: X<T>) -> T {
  return x[]!
}
// CHECK: $S18subscript_accessor1XVxSgycisTf4dn_n
