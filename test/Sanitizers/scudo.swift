// RUN: %target-swiftc_driver %s -g -sanitize=scudo -o %t_scudo-binary
// RUN: not --crash %target-run %t_scudo-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=linux-gnu
// REQUIRES: scudo_runtime

let allocated = UnsafeMutableRawPointer.allocate(byteCount: 128, alignment: 1)
allocated.deallocate()
allocated.deallocate()

// CHECK: {{ERROR: invalid chunk state|\*\*\* GWP-ASan detected a memory error \*\*\*}}
