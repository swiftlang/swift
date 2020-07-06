// RUN: %target-swift-frontend -O -module-name=test -emit-sil -primary-file %s | %FileCheck %s

protocol P { associatedtype A = Int }
protocol Q : P {}

protocol B { var x: Int { get } }
struct Y<T> {}
extension Y : B where T : Q { var x: Int { 0 }}

extension P {
  var z: Int? { (Y<Self>() as? B)?.x }
}

struct X : Q {

// Check that this getter can be folded to a simple "return 0"

// CHECK-LABEL: sil hidden @$s4test1XV0A2MeSiSgvg : $@convention(method) (X) -> Optional<Int> {
// CHECK:      bb0(%0 : $X):
// CHECK-NEXT:   integer_literal ${{.*}}, 0
// CHECK-NEXT:   struct $Int
// CHECK-NEXT:   enum $Optional<Int>, #Optional.some!enumelt
// CHECK-NEXT:   return %3 : $Optional<Int>
// CHECK-NEXT: } // end sil function '$s4test1XV0A2MeSiSgvg'
  var testMe: Int? { z }
}
