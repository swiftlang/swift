// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

class Base {}
class Derived: Base {}

let promoted: Base = Derived()
// CHECK: @_hasInitialValue internal let promoted: Base = Derived()

func takesAnyHashable(_ value: AnyHashable) {}

func testAnyHashableErasure() {
  takesAnyHashable(10)
  // CHECK: internal func testAnyHashableErasure() {
  // CHECK-NEXT:   takesAnyHashable(10)
  // CHECK-NEXT: }
}
