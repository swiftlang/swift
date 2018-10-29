// This test verifies that we add the function attributes used by ASan.

// RUN: %target-swift-frontend -emit-ir -disable-llvm-optzns -sanitize=address %s | %FileCheck %s -check-prefix=ASAN

// ASAN: define {{.*}} @"$s4main4testyyF"() [[DEFAULT_ATTRS:#[0-9]+]]
public func test() {
}

// ASAN: define {{.*}} @"$s4main1xSivr"({{.*}}) [[COROUTINE_ATTRS:#[0-9]+]]
public var x: Int {
  _read {
    yield 0
  }
}

// ASAN: attributes [[DEFAULT_ATTRS]] =
// ASAN-SAME: sanitize_address
// ASAN-SAME: }

// ASAN: attributes [[COROUTINE_ATTRS]] =
// ASAN-NOT: sanitize_address
// ASAN-SAME: }
