
// RUN: %target-swift-frontend -module-name specialize_anyobject -O -sil-inline-threshold 0 -emit-sil -primary-file %s | %FileCheck %s

// rdar://problem/20338028
protocol PA: class { }
protocol PB { associatedtype B: PA }

class CA: PA { }
class CB: PB { typealias B = CA }

struct S<A: PB> {
  @_transparent
  func crash() -> Bool {
    let a: A.B? = nil
    return a === a
  }
}

// CHECK-LABEL: sil hidden @$S20specialize_anyobject6callit{{[_0-9a-zA-Z]*}}F
func callit(s: S<CB>) {
  // CHECK: function_ref @$Ss3eeeoiySbyXlSg_ABtF : $@convention(thin) (@guaranteed Optional<AnyObject>, @guaranteed Optional<AnyObject>) -> Bool
  s.crash()
}
