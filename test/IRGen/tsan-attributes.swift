// This test verifies that we add the function attributes used by TSan.

// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -disable-llvm-optzns -sanitize=thread %s | %FileCheck %s -check-prefix=TSAN

// TSan is currently only supported on 64 bit mac and simulators.
// (We do not test the simulators here.)
// REQUIRES: CPU=x86_64, OS=macosx

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
// TSAN-NOT: sanitize_address
// TSAN-SAME: }
