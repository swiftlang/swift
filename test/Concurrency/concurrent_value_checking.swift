// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -enable-experimental-concurrent-value-checking
// REQUIRES: concurrency

class NotConcurrent { }

// ----------------------------------------------------------------------
// ConcurrentValue restriction on actor operations
// ----------------------------------------------------------------------

actor A1 {
  let localLet: NotConcurrent = NotConcurrent()
  func synchronous() -> NotConcurrent? { nil }
  func asynchronous(_: NotConcurrent?) async { }
}

actor A2 {
  var localVar: NotConcurrent

  init(value: NotConcurrent) {
    self.localVar = value
  }
}

func testActorCreation(value: NotConcurrent) {
  _ = A2(value: value) // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent' across actors}}
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
actor TestActor {}

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

class ClassWithGlobalActorInits {
  @SomeGlobalActor
  init(_: NotConcurrent) { }

  @SomeGlobalActor
  init() { }
}


@MainActor
func globalTestMain(nc: NotConcurrent) async {
  let a = globalValue // expected-warning{{cannot use let 'globalValue' with a non-concurrent-value type 'NotConcurrent?' across actors}}
  await globalAsync(a) // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
  await globalSync(a)  // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent?' across actors}}
  _ = await ClassWithGlobalActorInits(nc) // expected-warning{{cannot pass argument of non-concurrent-value type 'NotConcurrent' across actors}}
  _ = await ClassWithGlobalActorInits()
}

@SomeGlobalActor
func someGlobalTest(nc: NotConcurrent) {
  let hs = HasSubscript()
  let _ = hs[0] // okay
  _ = ClassWithGlobalActorInits(nc)
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
    print(y) // expected-error{{reference to captured var 'y' in concurrently-executing code}}
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

@concurrent func concurrentFunc() -> NotConcurrent? { nil }

// ----------------------------------------------------------------------
// No ConcurrentValue restriction on @concurrent function types.
// ----------------------------------------------------------------------
typealias CF = @concurrent () -> NotConcurrent?
typealias BadGenericCF<T> = @concurrent () -> T?
typealias GoodGenericCF<T: ConcurrentValue> = @concurrent () -> T? // okay

var concurrentFuncVar: (@concurrent (NotConcurrent) -> Void)? = nil

// ----------------------------------------------------------------------
// ConcurrentValue restriction on @concurrent closures.
// ----------------------------------------------------------------------
func acceptConcurrentUnary<T>(_: @concurrent (T) -> T) { }

func concurrentClosures<T>(_: T) {
  acceptConcurrentUnary { (x: T) in
    _ = x // ok
    acceptConcurrentUnary { _ in x } // expected-warning{{cannot use parameter 'x' with a non-concurrent-value type 'T' from concurrently-executed code}}
  }
}

// ----------------------------------------------------------------------
// ConcurrentValue checking
// ----------------------------------------------------------------------
struct S1: ConcurrentValue {
  var nc: NotConcurrent // expected-error{{stored property 'nc' of 'ConcurrentValue'-conforming struct 'S1' has non-concurrent-value type 'NotConcurrent'}}
}

struct S2<T>: ConcurrentValue {
  var nc: T // expected-error{{stored property 'nc' of 'ConcurrentValue'-conforming generic struct 'S2' has non-concurrent-value type 'T'}}
}

struct S3<T> {
  var c: T
  var array: [T]
}

extension S3: ConcurrentValue where T: ConcurrentValue { }

enum E1: ConcurrentValue {
  case payload(NotConcurrent) // expected-error{{associated value 'payload' of 'ConcurrentValue'-conforming enum 'E1' has non-concurrent-value type 'NotConcurrent'}}
}

enum E2<T> {
  case payload(T)
}

extension E2: ConcurrentValue where T: ConcurrentValue { }

class C1: ConcurrentValue {
  let nc: NotConcurrent? = nil // expected-error{{stored property 'nc' of 'ConcurrentValue'-conforming class 'C1' has non-concurrent-value type 'NotConcurrent?'}}
  var x: Int = 0 // expected-error{{stored property 'x' of 'ConcurrentValue'-conforming class 'C1' is mutable}}
  let i: Int = 0
}

class C2: ConcurrentValue {
  let x: Int = 0
}

class C3: C2 {
  var y: Int = 0 // expected-error{{stored property 'y' of 'ConcurrentValue'-conforming class 'C3' is mutable}}
}

class C4: C2, UnsafeConcurrentValue {
  var y: Int = 0 // okay
}

class C5: UnsafeConcurrentValue {
  var x: Int = 0 // okay
}

class C6: C5 {
  var y: Int = 0 // still okay, it's unsafe
}

class C7<T>: ConcurrentValue { }

class C8: C7<Int> { } // okay

open class C9: ConcurrentValue { } // expected-error{{open class 'C9' cannot conform to `ConcurrentValue`; use `UnsafeConcurrentValue`}}

public class C10: ConcurrentValue { }
// expected-note@-1{{superclass is declared here}}
open class C11: C10 { }
// expected-error@-1{{superclass 'C10' of open class must be open}}
// expected-error@-2{{open class 'C11' cannot conform to `ConcurrentValue`; use `UnsafeConcurrentValue`}}

// ----------------------------------------------------------------------
// UnsafeConcurrentValue disabling checking
// ----------------------------------------------------------------------
struct S11: UnsafeConcurrentValue {
  var nc: NotConcurrent // okay
}

struct S12<T>: UnsafeConcurrentValue {
  var nc: T // okay
}

enum E11<T>: UnsafeConcurrentValue {
  case payload(NotConcurrent) // okay
  case other(T) // okay
}
