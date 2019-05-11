// RUN: %target-swift-frontend -c -o /dev/null -O -Xllvm -sil-print-after=inline %s 2>&1 | %FileCheck %s --check-prefix NOTSKIPPING
// RUN: %target-swift-frontend -emit-sil -o /dev/null -O -Xllvm -sil-print-after=inline %s 2>&1 | %FileCheck %s --check-prefix NOTSKIPPING
// RUN: %target-swift-frontend -emit-module -o /dev/null -O -Xllvm -sil-print-after=inline %s 2>&1 | %FileCheck %s --check-prefix SKIPPING

// This test ensures that we don't run the Perf Inliner after serializing a
// module, if we're stopping optimizations after serializing. We want to also
// make sure we _do_ still run the Perf Inliner when we're doing a full
// compile or emitting SIL directly.

@inline(never)
func _blackHole(_ x: Int) {}

@inlinable
public func inlinableFunction(_ x: Int) -> Int {
  return x + 1
}

public func caller() {
  _blackHole(inlinableFunction(20))
}

// NOTSKIPPING: *** SIL function after {{.*}}, stage MidLevel, pass {{.*}}: PerfInliner (inline)
// SKIPPING-NOT: *** SIL function after {{.*}}, stage MidLevel, pass {{.*}}: PerfInliner (inline)
