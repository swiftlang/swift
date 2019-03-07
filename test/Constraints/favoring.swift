// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

func f(_: Int) { }
func f(_: Double) { }

func g(i: Int) {
  // CHECK: favoring overloads for
  // CHECK-NEXT: bound to decl{{.*}} : (Int) -> ()
  f(i)

  // CHECK: favoring overloads for
  // CHECK-NEXT: bound to decl{{.*}} : (Int) -> ()
  f(0)

  // CHECK: favoring overloads for
  // CHECK-NEXT: bound to decl{{.*}} : (Double) -> ()
  // CHECK-NOT: bound
  // CHECK: Initial
  f(3.14159)
}
