// RUN: %target-swift-frontend -O -emit-sil -Xllvm -sil-print-generic-specialization-loops %s 2>&1 | %FileCheck --check-prefix=CHECK %s

// Check that the generic specializer does not hang a compiler by
// creating and infinite loop of generic specializations.

// Tests in this file should not result in any detected specialization loops at all.
// CHECK-NOT: generic specialization loop

// Check specializations of mutually recusrive functions, where
// there is no specialization loop.
// CHECK-LABEL: sil{{.*}}testFooBar1{{.*}}convention
@inline(never)
public func foo1<T>(_ x: [T]) {
  bar1([5,6])
}

public func bar1<T>(_ x: [T]) {
  foo1(x)
}

public func testFooBar1() {
  foo1([1,2,3])
}

// Another example, where there is no specialization loop.
func foo6<T>(_ t: T) {
  bar6(t)
}

func bar6<T>(_ t: T) {
  foo6(t)
}

public func testFooBar6() {
  foo6(1)
}

// Check specializations of mutually recusrive functions, where
// there is no specialization loop.
@inline(never)
public func foo2<T>(_ x: [T]) {
  foo2(x.reversed())
}

public func testFoo2() {
  foo2([1,2,3])
}

// Check specializations of mutually recursive functions where
// there is no specialization loop, because no new specializations
// are required after 3-4 rounds of specializations.
func foo5<T, S>(_ t: T, _ s: S) {
  bar5([UInt8(1)], [t])
}

func bar5<T, S>(_ t: T, _ s: S) {
  foo5([t], [s])
}

public func testFooBar5() {
  foo5(1, 2.0)
}

// Yet another example of creating a lot of generic specializations for
// deeply nested generics. But it should not produce any specialization loops.
protocol Pingable {}

struct Some1<T> {
  init() {}
  func foo(_ x: T) {}
}
struct Some0<T> {
  init() {}
  func foo(_ x: T) {}
}

@inline(never)
func flood<T>(_ x: T) {
  _ = Some1<Some1<Some1<Some1<T>>>>() is Pingable
  _ = Some1<Some1<Some1<Some0<T>>>>() is Pingable
  _ = Some1<Some1<Some0<Some1<T>>>>() is Pingable
  _ = Some1<Some1<Some0<Some0<T>>>>() is Pingable
  _ = Some1<Some0<Some1<Some1<T>>>>() is Pingable
  _ = Some1<Some0<Some1<Some0<T>>>>() is Pingable
  _ = Some1<Some0<Some0<Some1<T>>>>() is Pingable
  _ = Some1<Some0<Some0<Some0<T>>>>() is Pingable
  _ = Some0<Some1<Some1<Some1<T>>>>() is Pingable
  _ = Some0<Some1<Some1<Some0<T>>>>() is Pingable
  _ = Some0<Some1<Some0<Some1<T>>>>() is Pingable
  _ = Some0<Some1<Some0<Some0<T>>>>() is Pingable
  _ = Some0<Some0<Some1<Some1<T>>>>() is Pingable
  _ = Some0<Some0<Some1<Some0<T>>>>() is Pingable
  _ = Some0<Some0<Some0<Some1<T>>>>() is Pingable
  _ = Some0<Some0<Some0<Some0<T>>>>() is Pingable
}

@inline(never)
func flood3<T>(_ x: T) {
 flood(Some1<Some1<Some1<Some1<T>>>>())
 flood(Some1<Some1<Some1<Some0<T>>>>())
 flood(Some1<Some1<Some0<Some1<T>>>>())
 flood(Some1<Some1<Some0<Some0<T>>>>())
 flood(Some1<Some0<Some1<Some1<T>>>>())
 flood(Some1<Some0<Some1<Some0<T>>>>())
 flood(Some1<Some0<Some0<Some1<T>>>>())
 flood(Some1<Some0<Some0<Some0<T>>>>())
 flood(Some0<Some1<Some1<Some1<T>>>>())
 flood(Some0<Some1<Some1<Some0<T>>>>())
 flood(Some0<Some1<Some0<Some1<T>>>>())
 flood(Some0<Some1<Some0<Some0<T>>>>())
 flood(Some0<Some0<Some1<Some1<T>>>>())
 flood(Some0<Some0<Some1<Some0<T>>>>())
 flood(Some0<Some0<Some0<Some1<T>>>>())
 flood(Some0<Some0<Some0<Some0<T>>>>())
}

@inline(never)
func flood2<T>(_ x: T) {
 flood3(Some1<Some1<Some1<Some1<T>>>>())
 flood3(Some1<Some1<Some1<Some0<T>>>>())
 flood3(Some1<Some1<Some0<Some1<T>>>>())
 flood3(Some1<Some1<Some0<Some0<T>>>>())
 flood3(Some1<Some0<Some1<Some1<T>>>>())
 flood3(Some1<Some0<Some1<Some0<T>>>>())
 flood3(Some1<Some0<Some0<Some1<T>>>>())
 flood3(Some1<Some0<Some0<Some0<T>>>>())
 flood3(Some0<Some1<Some1<Some1<T>>>>())
 flood3(Some0<Some1<Some1<Some0<T>>>>())
 flood3(Some0<Some1<Some0<Some1<T>>>>())
 flood3(Some0<Some1<Some0<Some0<T>>>>())
 flood3(Some0<Some0<Some1<Some1<T>>>>())
 flood3(Some0<Some0<Some1<Some0<T>>>>())
 flood3(Some0<Some0<Some0<Some1<T>>>>())
 flood3(Some0<Some0<Some0<Some0<T>>>>())
}

@inline(never)
public func run_TypeFlood(_ N: Int) {
  for _ in 1...N {
    flood2(Some1<Some1<Some1<Int>>>())
    flood2(Some1<Some1<Some0<Int>>>())
    flood2(Some1<Some0<Some1<Int>>>())
    flood2(Some1<Some0<Some0<Int>>>())
    flood2(Some0<Some1<Some1<Int>>>())
    flood2(Some0<Some1<Some0<Int>>>())
    flood2(Some0<Some0<Some1<Int>>>())
    flood2(Some0<Some0<Some0<Int>>>())
  }
}
