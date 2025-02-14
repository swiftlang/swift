// RUN: %target-swift-frontend -O %s -parse-as-library -Xllvm -sil-print-types -emit-sil -enforce-exclusivity=none -Xllvm -sil-disable-pass=function-signature-opts | %FileCheck %s

// REQUIRES: swift_in_compiler

// Test ARC optimizations on source level tests that have been
// miscompiled and crash (e.g. because of use-after-free).

// -----------------------------------------------------------------------------
// rdar://74469299 (ARC miscompile: EscapeAnalysis::mayReleaseContent;
// potential use-after-free)
// -----------------------------------------------------------------------------

public class Base {
  var i = 3
  init() {}
}
public class Node : Base {
  var node: Base

  @inline(never)
  init(node: Base) { self.node = node }
}
struct Queue {
  var node: Node
}

@inline(never)
func useQueue(q: __owned Queue) {}

@inline(never)
func useNode(n: Base) -> Int {
  return n.i
}

// CHECK-LABEL: sil [noinline] @$s9arc_crash14testMayReleaseAA4BaseCyF : $@convention(thin) () -> @owned Base {
// CHECK:   [[BASE:%.*]] = alloc_ref $Base
// CHECK:   [[EI:%.*]] = end_init_let_ref [[BASE]]
// CHECK:   strong_retain [[EI]] : $Base
// CHECK:   apply %{{.*}} : $@convention(thin) (@owned Queue) -> ()
// CHECK-LABEL: } // end sil function '$s9arc_crash14testMayReleaseAA4BaseCyF'
@inline(never)
public func testMayRelease() -> Base {
  let n2 = Base()
  let n1 = Node(node: n2)
  let q = Queue(node: n1)
  // n2 must not be release before useQueue.
  useQueue(q: q)
  return n2
}

// This crashes when testMayRelease releases the object too early.
// print("Object:")
// print(testMayRelease())
// -----------------------------------------------------------------------------
