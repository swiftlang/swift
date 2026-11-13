// RUN: %target-swiftc_driver %s -g -sanitize=address -o %t_asan-binary
// RUN: %target-codesign %t_asan-binary
// RUN: %target-run %t_asan-binary 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: asan_runtime
// REQUIRES: swift_stdlib_asserts
// UNSUPPORTED: use_os_stdlib, back_deployment_runtime

// ASan's malloc_good_size returns the requested size unchanged instead of
// rounding up to an allocation quantum, so String storage allocations end at
// arbitrary byte offsets. The breadcrumbs slot must still be placed within
// the allocation; __StringStorage._invariantCheck traps if it isn't.

let sizes = 200..<600

// Exact-capacity storage, then force allocated breadcrumbs.
for size in sizes {
  var s = ""
  s.reserveCapacity(size)
  s.append(contentsOf: String(repeating: "é", count: size / 2))
  precondition(s.utf16.count == size / 2)
  let i = s.utf16.index(s.utf16.startIndex, offsetBy: size / 4)
  precondition(s.utf16.distance(from: s.utf16.startIndex, to: i) == size / 4)
}

// Transcoding from UTF-16 stores a precalculated UTF-16 count in the slot.
for size in sizes {
  let utf16 = Array(repeating: UInt16(0xE9), count: size / 2)
  let s = String(decoding: utf16, as: UTF16.self)
  precondition(s.utf8.count == size / 2 * 2)
  precondition(s.utf16.count == size / 2)
}

// Appending one byte at a time grows through many capacities.
var appended = ""
for _ in sizes {
  appended.append("x")
}
precondition(appended.utf8.count == sizes.count)

// CHECK: done
print("done")
