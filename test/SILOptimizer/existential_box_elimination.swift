// RUN: %target-build-swift -O -wmo %s -module-name=test -Xfrontend -sil-verify-all -emit-sil | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -wmo -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test


final class LifetimeTracking {
  static var numObjects = 0
  init() { LifetimeTracking.numObjects += 1 }
  deinit { LifetimeTracking.numObjects -= 1 }
}

public struct TestError: Error {
  var errno: Int
  let t = LifetimeTracking()

  init(errno: Int) { self.errno = errno }
}

@inline(never)
@_optimize(none)
internal func internalImplementation(somethingGood: Bool) -> Result<Int, any Error> {
  return somethingGood ? .success(27) : .failure(TestError(errno:123))
}

public func publicWrapper(somethingGood: Bool) throws -> Int {
  return try internalImplementation(somethingGood: somethingGood).get()
}

// CHECK-LABEL: sil [noinline] @$s4test0A13WithForceCast13somethingGoodSiSb_tF
// CHECK:       [[F:%[0-9]+]] = function_ref @$s4test22internalImplementation13somethingGoods6ResultOySis5Error_pGSb_tF
// CHECK:       apply [[F]]
// CHECK:       switch_enum
// CHECK:     bb1({{%.*}} : $Int):
// CHECK-NOT:   alloc_existential_box
// CHECK: } // end sil function '$s4test0A13WithForceCast13somethingGoodSiSb_tF'
@inline(never)
public func testWithForceCast(somethingGood: Bool) -> Int {
  do {
    return try publicWrapper(somethingGood: somethingGood)
  } catch let e {
    return (e as! TestError).errno
  }
}

// CHECK-LABEL: sil [noinline] @$s4test0A19WithMultipleCatches13somethingGoodSiSb_tF
// CHECK:       [[F:%[0-9]+]] = function_ref @$s4test22internalImplementation13somethingGoods6ResultOySis5Error_pGSb_tF
// CHECK:       apply [[F]]
// CHECK:       switch_enum
// CHECK:     bb1({{%.*}} : $Int):
// CHECK-NOT:   alloc_existential_box
// CHECK: } // end sil function '$s4test0A19WithMultipleCatches13somethingGoodSiSb_tF'
@inline(never)
public func testWithMultipleCatches(somethingGood: Bool) -> Int {
  do {
    return try publicWrapper(somethingGood: somethingGood)
  } catch let e as TestError {
    return e.errno
  } catch {
    fatalError()
  }
}

@inline(never)
func checkResult(_ result: Int, expected: Int) {
  if result != expected {
    fatalError("unexpected result: \(result) != \(expected)")
  }
  if LifetimeTracking.numObjects != 0 {
    fatalError("objects leaked!")
  }
}

checkResult(testWithForceCast(somethingGood: true), expected: 27)
checkResult(testWithMultipleCatches(somethingGood: true), expected: 27)

checkResult(testWithForceCast(somethingGood: false), expected: 123)
checkResult(testWithMultipleCatches(somethingGood: false), expected: 123)
