// RUN: %target-swift-frontend %s -O -I %t -emit-ir -g -o - | %FileCheck %s

func use<T>(_ t: T) {}

@inline(never)
public func noinline(_ x: Int64) -> Int64 { return x }

@_transparent
public func transparent(_ y: Int64) -> Int64 {
  var local = y
  return noinline(local)
}

let z = transparent(0)
use(z)

// Check that a transparent function has no debug information.
// CHECK: define {{.*}}$S11transparentAA
// CHECK-SAME: !dbg ![[SP:[0-9]+]]
// CHECK-NEXT: entry:
// CHECK-NEXT: !dbg ![[ZERO:[0-9]+]]
// CHECK-NEXT: !dbg ![[ZERO]]
// CHECK-NEXT: }

// CHECK: ![[SP]] = {{.*}}name: "transparent"
// CHECK-SAME: file: ![[FILE:[0-9]+]]
// CHECK-NOT: line:
// CHECK: ![[FILE]] = {{.*}}"<compiler-generated>"
// CHECK: ![[ZERO]] = !DILocation(line: 0,
