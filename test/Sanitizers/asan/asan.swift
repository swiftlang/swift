// XFAIL: OS=windows-msvc

// This test hangs on aarch64 FreeBSD
// XFAIL will try to run the test and the verify that the test fails, which
// still hits the hang.
// UNSUPPORTED: OS=freebsd && CPU=aarch64

// RUN: %target-swiftc_driver %s -g -sanitize=address -o %t_asan-binary
// RUN: %target-codesign %t_asan-binary
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0 not %target-run %t_asan-binary 2>&1 | %FileCheck %s

// ODR Indicator variant
// RUN: %target-swiftc_driver %s -g -sanitize=address \
// RUN:   -sanitize-address-use-odr-indicator -o %t_asan-binary-odr-indicator
// RUN: %target-codesign %t_asan-binary-odr-indicator
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0 not %target-run \
// RUN:   %t_asan-binary-odr-indicator 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: asan_runtime

// Make sure we can handle swifterror. LLVM's address sanitizer pass needs to
// ignore swifterror addresses.

enum MyError : Error {
  case A
}
public func foobar(_ x: Int) throws {
  if x == 0 {
    throw MyError.A
  }

}

public func call_foobar() {
  do {
    try foobar(1)
  } catch(_) {
  }
}

// Test AddressSanitizer execution end-to-end.

var a = UnsafeMutablePointer<Int>.allocate(capacity: 1)
a.initialize(to: 5)
a.deinitialize(count: 1)
a.deallocate()
print(a.pointee)
a.deinitialize(count: 1)
a.deallocate()

// CHECK: AddressSanitizer: heap-use-after-free
