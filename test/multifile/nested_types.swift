// RUN: %target-build-swift -module-name test -wmo -O -emit-ir -Xfrontend -num-threads -Xfrontend 0 %s %S/Inputs/nested_types_defs.swift -o - | %FileCheck %s

// Make sure we generate the outer metadata.

// CHECK-DAG: @"$S4test5OuterVMf" = internal constant {{.*}} @"$SytWV"{{.*}}@"$S4test5OuterVMn"
// CHECK-DAG: @"$S4test6Outer2VMf" = internal constant {{.*}} @"$SytWV"{{.*}}@"$S4test6Outer2VMn"
// CHECK-DAG: @"$S4test6Outer3VMf" = internal constant {{.*}} @"$SytWV"{{.*}}@"$S4test6Outer3VMn"
// CHECK-DAG: @"$S4test6Outer4VMf" = internal constant {{.*}} @"$SytWV"{{.*}}@"$S4test6Outer4VMn"

class C<T> { }

struct Y {
  let x:C<Outer.Inner>
  let y:C<Outer2.InnerE>
  let z:C<Outer3.InnerC>
  let w:C<Outer4.InnerExtension>
}

public func test() {
  var c = Y(x: C<Outer.Inner>(), y: C<Outer2.InnerE>(), z: C<Outer3.InnerC>(),
            w: C<Outer4.InnerExtension>())

  print("a \(c)")
}
