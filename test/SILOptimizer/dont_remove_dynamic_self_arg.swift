// RUN: %target-swift-frontend -module-name test -Xllvm -sil-print-types -emit-sil -O %s | %FileCheck %s
// Make sure IRGen does not crash
// RUN: %target-swift-frontend -module-name test -emit-ir -O %s

public class X {}

@inline(never)
func transform<T>(_ a: Any, c: (Any) -> T?) -> T? {
  return c(a)
}

// CHECK-LABEL: sil {{.*}}@$s4test1XC6testityACSgypFACXDSgypXEfU_ :
// CHECK: bb0({{.*}}, {{%[0-9]+}} : $@thick @dynamic_self X.Type):

// The checked_cast_addr_br must have %1 as implicit type dependent operand.
// CHECK:   checked_cast_addr_br take_always Any in %{{[0-9]+}} : $*Any to @dynamic_self X in %{{[0-9]+}} : $*X, bb1, bb2
// CHECK: } // end sil function '$s4test1XC6testityACSgypFACXDSgypXEfU_'

extension X {
  public func testit(_ a: Any) -> X? {
    guard let xx = transform(a, c: { $0 as? Self }) else {
      return nil
    }
    return xx
  }
}

