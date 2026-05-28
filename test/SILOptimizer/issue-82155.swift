// RUN: %target-swift-emit-sil -primary-file %s -O -module-name=test | %FileCheck %s

// Regression test for https://github.com/swiftlang/swift/issues/82155
//
// A non-escaping closure that captures a class reference should not introduce
// retain/release traffic at the call site after closure specialization, since
// the captured reference's lifetime is already covered by the caller's
// @guaranteed parameter.

public class Object { var count = 0 }

@inline(never)
func execute<T>(closure: () -> T) -> T {
  closure()
}

// CHECK-LABEL: sil @$s4test7compare9reference5countSbAA6ObjectC_SitF : $@convention(thin) (@guaranteed Object, Int) -> Bool
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
// CHECK: } // end sil function '$s4test7compare9reference5countSbAA6ObjectC_SitF'
public func compare(reference: Object, count: Int) -> Bool {
  execute { reference.count == count }
}

@inline(never)
func executeVoid(closure: () -> Void) {
  closure()
}

// Non-generic variant. The closure body must have an observable side effect so
// that dead-argument-removal does not strip the captured reference and erase
// the parameter we want to check.
//
// CHECK-LABEL: sil @$s4test11compareVoid9reference5countyAA6ObjectC_SitF : $@convention(thin) (@guaranteed Object, Int) -> ()
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
// CHECK: } // end sil function '$s4test11compareVoid9reference5countyAA6ObjectC_SitF'
public func compareVoid(reference: Object, count: Int) {
  executeVoid { reference.count = count }
}
