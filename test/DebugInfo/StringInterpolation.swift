// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
func markUsed<T>(_ t: T) {}

// Note: If we ever want to go back to emitting debug info for $interpolation,
// it would also be acceptable for there to be a DILocalVariable, as long as it
// is marked with DIFlagArtificial.

public func f() {
  // CHECK-NOT: !DILocalVariable(name: "$interpolation",
  let world = "World"
  var s = "Hello \(world)!"
  markUsed(s)
}
f()
