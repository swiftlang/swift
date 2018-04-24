// RUN: %empty-directory(%t)
// RUN: %target-build-swift -wmo -O  %s %S/../Inputs/nested_types_defs.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

class C<T> { }

struct Y {
  let x:C<Outer.Inner>
  let y:C<Outer2.InnerE>
  let z:C<Outer3.InnerC>
  let w:C<Outer4.InnerExtension>
}

func test() {
  var c = Y(x: C<Outer.Inner>(), y: C<Outer2.InnerE>(), z: C<Outer3.InnerC>(), w: C<Outer4.InnerExtension>())

  print("a \(c)")
}

// CHECK: a Y(x: a.C<a.Outer.Inner>, y: a.C<a.Outer2.InnerE>, z: a.C<a.Outer3.InnerC>, w: a.C<a.Outer4.InnerExtension>)
test()
