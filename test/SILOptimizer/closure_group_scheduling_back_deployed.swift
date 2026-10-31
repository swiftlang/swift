// RUN: %target-swift-frontend -emit-sil %s -module-name E -Xllvm -sil-print-closure-groups -o /dev/null 2>&1 | %FileCheck %s --check-prefix=GROUPS

// REQUIRES: OS=macosx

// The back-deployment fallback is emitted only when the attribute's platform
// is the target.

@usableFromInline func modify(_ body: () -> ()) { body() }

// A @backDeployed function and its fallback share a closure, which joins them
// into one group.
// GROUPS: Closure group for access-enforcement-selection: $s1E12backDeployedSiyFTwB $s1E12backDeployedSiyFyyXEfU_ $s1E12backDeployedSiyF{{$}}
@available(macOS 10.15, *)
@backDeployed(before: macOS 99)
public func backDeployed() -> Int {
  var x = 0
  modify { x += 1 }
  return x
}
