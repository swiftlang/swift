// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
func markUsed<T>(_ t: T) {}

public func f() {
  // CHECK: !DILocalVariable(name: "$interpolation",
  // CHECK-SAME:             flags: DIFlagArtificial)
  let world = "World"
  var s = "Hello \(world)!"
  markUsed(s)
}
f()
