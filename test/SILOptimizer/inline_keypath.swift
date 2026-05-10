// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

public protocol P {}

// CHECK-LABEL: sil @$s14inline_keypath6testitySbAA1P_pXp_s14PartialKeyPathCyxGtSlRzlF
// CHECK:         [[T:%[0-9]+]] = open_existential_metatype %0
// CHECK:         = keypath {{.*}}@opened{{.*}} type-defs: [[T]]
// CHECK:       } // end sil function '$s14inline_keypath6testitySbAA1P_pXp_s14PartialKeyPathCyxGtSlRzlF'
public func testit<C: Collection>(_ t: any P.Type, _ kp: PartialKeyPath<C>) -> Bool {
  return tyFunc(t, kp)
}

@inline(__always)
func tyFunc<C: Collection, H: P>(_ h: H.Type, _ kp: PartialKeyPath<C>) -> Bool {
  return kp == \Array<H>.count
}


