// RUN: %target-swift-frontend -O -primary-file %s -emit-sil | %FileCheck %s


// Test opened types

protocol P {
  func foo()
}

// CHECK-LABEL: sil {{.*}} @{{.*}}test_open_existential
// CHECK: [[E:%[0-9]+]] = open_existential_addr immutable_access %0
// CHECK: [[W:%[0-9]+]] = witness_method $@opened{{.*}} [[E]] {{.*}} // type-defs: [[E]]
// CHECK: apply [[W]]<@opened{{.*}} // type-defs: [[E]]
// CHECK: return
@inline(__always)
func test_open_existential(p: P) {
  p.foo()
}

// Check if after inlining (= cloning) everything is still okay.

// CHECK-LABEL: sil {{.*}} @{{.*}}call_open_existential
// CHECK: [[E:%[0-9]+]] = open_existential_addr immutable_access %0
// CHECK: [[W:%[0-9]+]] = witness_method $@opened{{.*}} [[E]] {{.*}} // type-defs: [[E]]
// CHECK: apply [[W]]<@opened{{.*}} // type-defs: [[E]]
// CHECK: return
func call_open_existential(p: P) {
  test_open_existential(p: p)
}


// Test dynamic self

func idfunc<T>(_ t: T) -> T {
  return t
}

final class X {
// CHECK-LABEL: sil {{.*}} @{{.*}}test_dynself
// CHECK: bb0(%0 : $@thick X.Type):
// CHECK: apply %{{[0-9]+}}<@dynamic_self X>({{.*}} // type-defs: %0
// CHECK: return
  @inline(__always)
  class func test_dynself() -> Self {
    return idfunc(self.init())
  }

// Check if after inlining (= cloning) everything is still okay.

// CHECK-LABEL: sil {{.*}} @{{.*}}call_dynself
// CHECK: bb0(%0 : $@thick X.Type):
// CHECK: apply %{{[0-9]+}}<@dynamic_self X>({{.*}} // type-defs: %0
// CHECK: return
  class func call_dynself() -> Self {
    return test_dynself()
  }

  required init() { }
}

