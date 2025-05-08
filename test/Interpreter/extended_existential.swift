// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -target %target-cpu-apple-macos15.0 %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: executable_test

protocol A<B>: ~Copyable {
  associatedtype B
}

protocol B: ~Copyable {}

let a: Any = (any ~Copyable).self
// CHECK: any Any<Self: ~Swift.Copyable>
print(a)

let b: Any = (any ~Escapable).self
// CHECK: any Any<Self: ~Swift.Escapable>
print(b)

let c: Any = (any ~Copyable & ~Escapable).self
// CHECK: any Any<Self: ~Swift.Copyable, Self: ~Swift.Escapable>
print(c)

let d: Any = (any A).self
// CHECK: A
print(d)

let e: Any = (any B).self
// CHECK: B
print(e)

let f: Any = (any A & B).self
// CHECK: A & B
print(f)

let g: Any = (any A & ~Copyable).self
// CHECK: any A<Self: ~Swift.Copyable>
print(g)

let h: Any = (any B & ~Copyable).self
// CHECK: any B<Self: ~Swift.Copyable>
print(h)

let i: Any = (any A & B & ~Copyable).self
// CHECK: any A & B<Self: ~Swift.Copyable>
print(i)
