// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -O0 | FileCheck %s

// FIXME: Should be a SIL test, but we can't parse same-type constraints
// <rdar://problem/16238241>

protocol Runcible {
  typealias Mince
  typealias Quince
}

struct Spoon<T: Runcible> {
  var t: T
}

// CHECK: define void @_T{{.*}}3foo{{.*}}(
func foo<T where T: Runcible, T == T.Mince> (t: T) -> Spoon<T> {
  return Spoon(t)
}
