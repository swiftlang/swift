// A C method taking 'const T *self' on a ~Copyable type imports as 'borrowing
// self', so it can be called through a borrow such as a stored property.

// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging %s

// RUN: %target-swift-frontend -emit-silgen -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging %s | %FileCheck %s

import NoncopyableMethod

// 'const Handle *self' must lower to borrowed (@in_guaranteed), not consumed (@in).
// CHECK: [clang Handle.read] {{.*}}@convention(c) (@in_guaranteed Handle, Int32) -> Int32
// Non-const 'Handle *self' must stay mutating (@inout).
// CHECK: [clang Handle.update] {{.*}}@convention(c) (@inout Handle, Int32) -> ()

struct Container: ~Copyable {
  var handle: Handle

  // Before the fix 'self' was consuming, so this failed: "noncopyable
  // 'self.handle' cannot be consumed when ... borrowed".
  borrowing func readThroughProperty(_ id: CInt) -> CInt {
    return handle.read(id: id)
  }

  // Non-const 'Handle *self' must remain mutating.
  mutating func updateThroughProperty(_ id: CInt) {
    handle.update(id: id)
  }
}

func callThroughBorrow(_ c: borrowing Container) -> CInt {
  return c.readThroughProperty(5)
}
