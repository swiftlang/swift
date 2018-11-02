// RUN: %target-swift-frontend -emit-sil %s -Onone -Xllvm -sil-print-debuginfo \
// RUN:   -o /dev/null -Xllvm -sil-print-after=raw-sil-inst-lowering \
// RUN:   -Xllvm -sil-print-only-functions=$S8patatino4BearC6before7before26during5afterACSb_S3btKcfc \
// RUN:   2>&1 | %FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest

func unwrap(_ b: Bool) throws -> Int {
  return 0
}
class Bear {
  let x: LifetimeTracked
  init(n: Int, before: Bool) throws {
    self.x = LifetimeTracked(0)
  }
  init(n: Int, after: Bool) throws {
    self.x = LifetimeTracked(0)
  }
  convenience init(before: Bool, before2: Bool, during: Bool, after: Bool) throws {
    try self.init(n: unwrap(before2), after: during)
  }
}

// CHECK: %7 = integer_literal $Builtin.Int1, 0, loc {{.*}}:20:15, scope 1
// CHECK-NEXT:  store %7 to [trivial] %5 : $*Builtin.Int1, loc {{.*}}:20:15, scope 1
