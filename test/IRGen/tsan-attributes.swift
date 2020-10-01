// REQUIRES: tsan_runtime
// This test verifies that we add the function attributes used by TSan.

// RUN: %target-swift-frontend -emit-ir -sanitize=thread %s | %FileCheck %s -check-prefix=TSAN

// TSan is only supported on 64 bit.
// REQUIRES: PTRSIZE=64

// TSAN: define {{.*}} @"$s4main4testyyF"() [[DEFAULT_ATTRS:#[0-9]+]]
public func test() {
}

// TSAN: define {{.*}} @"$s4main1xSivr"({{.*}}) [[COROUTINE_ATTRS:#[0-9]+]]
public var x: Int {
  _read {
    yield 0
  }
}

// TSAN: attributes [[DEFAULT_ATTRS]] =
// TSAN-SAME: sanitize_thread
// TSAN-SAME: }

// TSAN: attributes [[COROUTINE_ATTRS]] =
// TSAN-SAME: sanitize_thread
// TSAN-SAME: }
