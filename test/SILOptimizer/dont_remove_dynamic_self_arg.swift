// RUN: %target-swift-frontend -module-name test -emit-sil -O %s | %FileCheck %s
// Make sure IRGen does not crash
// RUN: %target-swift-frontend -module-name test -emit-ir -O %s

public class X {}

@inline(never)
func transform<T>(_ a: Any, c: (Any) -> T?) -> T? {
  return c(a)
}

// CHECK-LABEL: sil private @$s4test1XC6testityACSgypFACXDSgypXEfU_ : $@convention(thin) (@in_guaranteed Any, @thick @dynamic_self X.Type) -> @owned Optional<X> {
// CHECK: bb0(%0 : $*Any, %1 : $@thick @dynamic_self X.Type):

// The checked_cast_addr_br must have %1 as implicit type dependend operand.
// CHECK:   checked_cast_addr_br take_always Any in %{{[0-9]+}} : $*Any to @dynamic_self X in %{{[0-9]+}} : $*X, bb1, bb2 // type-defs: %1;
// CHECK: } // end sil function '$s4test1XC6testityACSgypFACXDSgypXEfU_'

extension X {
  public func testit(_ a: Any) -> X? {
    guard let xx = transform(a, c: { $0 as? Self }) else {
      return nil
    }
    return xx
  }
}

