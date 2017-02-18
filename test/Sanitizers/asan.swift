// RUN: %target-swiftc_driver %s -g -sanitize=address -o %t_tsan-binary
// RUN: not env ASAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: asan_runtime
// XFAIL: linux

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
a.deallocate(capacity: 1)
print(a.pointee)
a.deinitialize(count: 1)
a.deallocate(capacity: 1)

// CHECK: AddressSanitizer: heap-use-after-free
