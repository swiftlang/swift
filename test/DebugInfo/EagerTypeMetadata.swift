// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - | %FileCheck %s

public class C<T>
{
  func c(_ i : T)
  {
    // Ensure that the type metadata for T is eagerly loaded at -Onone.
    // CHECK: define {{.*}} @"$s17EagerTypeMetadata1CC1cyyxF"
    // CHECK: %T = load %swift.type*, %swift.type**
    // CHECK-SAME: !dbg ![[LOC:[0-9]+]], !invariant.load
    // CHECK: ![[LOC]] = !DILocation(line: 0,
    var x = [i]
  }
}

