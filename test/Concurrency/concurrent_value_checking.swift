// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -enable-experimental-concurrent-value-checking
// REQUIRES: concurrency

class NotConcurrent { }

// ----------------------------------------------------------------------
// ConcurrentValue restriction on actor operations
// ----------------------------------------------------------------------

actor class A1 {
  let localLet: NotConcurrent = NotConcurrent()
  func synchronous() -> NotConcurrent? { nil }
  func asynchronous(_: NotConcurrent?) async { }
}

extension A1 {
  func testIsolation(other: A1) async {
    // All within the same actor domain, so the ConcurrentValue restriction
    // does not apply.
    _ = localLet
    _ = synchronous()
    _ = await asynchronous(nil)
    _ = self.localLet
    _ = self.synchronous()
    _ = await self.asynchronous(nil)

    // Across to a different actor, so ConcurrentValue restriction is enforced.
    _ = other.localLet // expected-warning{{cannot use property 'localLet' with a non-concurrent-value type 'NotConcurrent' across actors}}
    _ = await other.synchronous() // expected-warning{{cannot call function returning non-concurrent-value type 'NotConcurrent?' across actors}}
    _ = await other.asynchronous(nil) // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
  }
}

// ----------------------------------------------------------------------
// ConcurrentValue restriction on global actor operations
// ----------------------------------------------------------------------
actor class TestActor {}

@globalActor
struct SomeGlobalActor {
  static var shared: TestActor { TestActor() }
}

@SomeGlobalActor
let globalValue: NotConcurrent? = nil

@SomeGlobalActor
func globalSync(_: NotConcurrent?) {
}

@SomeGlobalActor
func globalAsync(_: NotConcurrent?) async {
  await globalAsync(globalValue) // both okay because we're in the actor
  globalSync(nil)
}

func globalTest() async {
  let a = globalValue // expected-warning{{cannot use let 'globalValue' with a non-concurrent-value type 'NotConcurrent?' across actors}}
  await globalAsync(a) // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
  await globalSync(a)  // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
}

struct HasSubscript {
  @SomeGlobalActor
  subscript (i: Int) -> NotConcurrent? { nil }
}

@MainActor
func globalTestMain() async {
  let a = globalValue // expected-warning{{cannot use let 'globalValue' with a non-concurrent-value type 'NotConcurrent?' across actors}}
  await globalAsync(a) // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
  await globalSync(a)  // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
}

@SomeGlobalActor
func someGlobalTest() {
  let hs = HasSubscript()
  let _ = hs[0] // okay
}

// ----------------------------------------------------------------------
// ConcurrentValue restriction on captures.
// ----------------------------------------------------------------------
func acceptNonConcurrent(_: () -> Void) { }
func acceptConcurrent(_: @concurrent () -> Void) { }

func testConcurrency() {
  let x = NotConcurrent()
  var y = NotConcurrent()
  y = NotConcurrent()
  acceptNonConcurrent {
    print(x) // okay
    print(y) // okay
  }
  acceptConcurrent {
    print(x) // expected-warning{{cannot use let 'x' with a non-concurrent-value type 'NotConcurrent' from concurrently-executed code}}
    print(y) // expected-warning{{cannot use var 'y' with a non-concurrent-value type 'NotConcurrent' from concurrently-executed code}}
  }
}

// ----------------------------------------------------------------------
// ConcurrentValue restriction on conformances.
// ----------------------------------------------------------------------
protocol AsyncProto {
  func asyncMethod(_: NotConcurrent) async
}

extension A1: AsyncProto {
  // FIXME: Poor diagnostic.
  func asyncMethod(_: NotConcurrent) async { } // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent' across actors}}
}

protocol MainActorProto {
  func asyncMainMethod(_: NotConcurrent) async
}

class SomeClass: MainActorProto {
  @SomeGlobalActor
  func asyncMainMethod(_: NotConcurrent) async { } // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent' across actors}}
}

// ----------------------------------------------------------------------
// ConcurrentValue restriction on concurrent functions.
// ----------------------------------------------------------------------

// FIXME: poor diagnostic
@concurrent func concurrentFunc() -> NotConcurrent? { nil } // expected-warning{{cannot call function returning non-concurrent-value type 'NotConcurrent?' across actors}}

// ----------------------------------------------------------------------
// ConcurrentValue restriction on @concurrent types.
// ----------------------------------------------------------------------
typealias CF = @concurrent () -> NotConcurrent? // expected-warning{{`@concurrent` function type has non-concurrent-value result type 'NotConcurrent?'}}
typealias BadGenericCF<T> = @concurrent () -> T? // expected-warning{{`@concurrent` function type has non-concurrent-value result type 'T?'}}
typealias GoodGenericCF<T: ConcurrentValue> = @concurrent () -> T? // okay

var concurrentFuncVar: (@concurrent (NotConcurrent) -> Void)? = nil // expected-warning{{`@concurrent` function type has non-concurrent-value parameter type 'NotConcurrent'}}

// ----------------------------------------------------------------------
// ConcurrentValue restriction on @concurrent closures.
// ----------------------------------------------------------------------
func acceptConcurrentUnary<T>(_: @concurrent (T) -> T) { }

func concurrentClosures<T>(_: T) {
  acceptConcurrentUnary { (x: T) in // expected-warning{{`@concurrent` closure has non-concurrent-value parameter type 'T'}}
    x
  }
}
