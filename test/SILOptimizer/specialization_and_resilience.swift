// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -enable-library-evolution -emit-sil | %FileCheck %s

public enum En {
  case A
  case B
}

@inlinable
@inline(never)
func genfunc<T>(_ t: T) -> T {
  return t
}

// CHECK-LABEL: sil @$s4test11callGenFuncyyF : $@convention(thin) () -> () {
// CHECK:  = function_ref @$s4test7genfuncyxxlFAA2EnO_Tg5 : $@convention(thin) (En) -> @out En
// CHECK: } // end sil function '$s4test11callGenFuncyyF'
public func callGenFunc() {
  _ = genfunc(En.A)
}
