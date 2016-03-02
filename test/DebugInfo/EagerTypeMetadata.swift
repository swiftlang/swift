// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - | FileCheck %s

public class C<T>
{
  func c (i : T)
  {
    // Ensure that the type metadata for T is eagerly loaded at -Onone.
    // CHECK: define void @_TFC17EagerTypeMetadata1C1cfxT_
    // CHECK: %T = load %swift.type*, %swift.type**
    // CHECK-SAME: !dbg ![[LOC:[0-9]+]], !invariant.load
    // CHECK: ![[LOC]] = !DILocation(line: 0,
    var x = i
  }
}

