// RUN: %target-typecheck-verify-swift -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// rdar://77942193 - adding async overload leads to expressions becoming "too complex"

struct Obj {
  func op<T>(_: T) {}
  func op(_: Int) {}
}

// Three overloads of `filter_async` to avoid generic overload optimization

func filter_async<T>(fn1: () -> T) -> T { fn1() }
func filter_async<T>(fn2: () async -> T) -> T { fatalError() }
func filter_async(_: String) -> Void {}

var a: String? = nil

// CHECK: attempting disjunction choice $T0 bound to decl async_overload_filtering.(file).filter_async(fn2:)
// CHECK-NEXT: overload set choice binding $T0 := {{.*}}
// CHECK-NEXT: increasing score due to sync-in-asynchronous
// CHECK-NEXT: solution is worse than the best solution
filter_async {
  Obj()
}.op("" + (a ?? "a"))
