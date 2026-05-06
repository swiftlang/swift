// RUN:%target-swift-frontend -emit-silgen %s | %FileCheck %s

class Klass {
  init() {}
}

struct NonTrivial {
  var k = Klass()
}

class C {
  let _x: Int = 0
  var x: Int { borrow { _x } }
}

// CHECK-LABEL: sil hidden [ossa] @$s30borrow_accessor_class_dispatch1fySiAA1CCF
// CHECK:         [[METHOD:%.*]] = class_method %0, #C.x!borrow : (C) -> () -> Int
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]](%0)
// CHECK:         return [[RESULT]]
// CHECK:       } // end sil function '$s30borrow_accessor_class_dispatch1fySiAA1CCF'
func f(_ c: C) -> Int { c.x }

class D {
  let _nt: NonTrivial = NonTrivial()
  var nt: NonTrivial { borrow { _nt } }
}

// CHECK-LABEL: sil hidden [ossa] @$s30borrow_accessor_class_dispatch1gySiAA1DCF
// CHECK:         [[METHOD:%.*]] = class_method %0, #D.nt!borrow : (D) -> () -> NonTrivial
// CHECK:         [[RESULT:%.*]] = apply [[METHOD]](%0)
// CHECK:       } // end sil function '$s30borrow_accessor_class_dispatch1gySiAA1DCF'
func g(_ d: D) -> Int {
  let _ = d.nt
  return 0
}

// CHECK-LABEL: sil_vtable C {
// CHECK:         #C.x!borrow: {{.*}} : @$s30borrow_accessor_class_dispatch1CC1xSivb
// CHECK:       }

// CHECK-LABEL: sil_vtable D {
// CHECK:         #D.nt!borrow: {{.*}} : @$s30borrow_accessor_class_dispatch1DC2ntAA10NonTrivialVvb
// CHECK:       }
