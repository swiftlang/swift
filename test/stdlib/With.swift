// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

let tuple: (a: String?, b: String?) = (a: nil, b: nil)

_ = tuple.with {
  $0.a = "withA"
  $0.b = "withB"
}

_ = tuple.with { tuple in
  if tuple.a == nil {
    tuple.a = "a_default"
  }

  if tuple.b == nil {
    tuple.b = "b_default"
  }
}

_ = tuple.with {
  $0 = (a: $0.a ?? "default", b: $0.b ?? "default")
}

_ = tuple.with {
  (a: $0.a ?? "default", b: $0.b ?? "default") // expected-warning{{expression of type '(a: String, b: String)' is unused}}
}

_ = tuple.with {
  $0 = (a: $0.a ?? "default", b: $0.b ?? "default", c: "c") // expected-error{{cannot assign value of type '(a: String, b: String, c: String)' to type '(a: String?, b: String?)'}}
}

_ = tuple.a.with { $0 = "withA" }
_ = tuple.a?.with { $0 = "withA?" }
_ = tuple.b.with { $0 = "withB" }
_ = tuple.b?.with { $0 = "withB?" }

extension Int {
  func addingTwo() -> Int {
    with { $0 += 2 }
  }
}

struct Test {
  var foo: String?
  var bar: String?

  func with(foo: String) -> Test {
    with { $0.foo = foo }
  }

  mutating func modifyFoo(_ foo: String) {
    self.foo = foo
  }

  mutating func modifyFooThrowing(_ foo: String) throws {
    self.foo = foo
  }

  mutating func modifyFooAsync(_ foo: String) async {
    self.foo = foo
  }

  mutating func modifyFooAsyncThrowing(_ foo: String) async throws {
    self.foo = foo
  }
}

let explicitWithFunc = Test().with
let explicitWithFuncExplicitType: (String) -> Test = Test().with
let explicitWithFuncExplicitType2: (String) -> Test = explicitWithFunc
let with: ((inout Test) -> Void) -> Test = Test().with
let with_throws: ((inout Test) throws -> Void) throws -> Test = Test().with
let with_async: ((inout Test) async -> Void) async -> Test = Test().with
let with_async_throws: ((inout Test) async throws -> Void) async throws -> Test = Test().with

let explicitWithFunc_as = Test().with
let explicitWithFuncExplicitType_as = Test().with as (String) -> Test
let with_as = Test().with as ((inout Test) -> Void) -> Test
let with_throws_as = Test().with as ((inout Test) throws -> Void) throws -> Test
let with_async_as = Test().with as ((inout Test) async -> Void) async -> Test 
let with_async_throws_as = Test().with as ((inout Test) async throws -> Void) async throws -> Test

_ = try Test()
  .with { $0.foo = "foo" }
  .with { $0.bar = "bar" }
  .with { $0 = Test() }
  .with { try $0.modifyFooThrowing("throwing") }

_ = Test().with(foo: "withFoo1")
_ = explicitWithFunc("withFoo2")
_ = Test().with { $0.foo = "withFoo3" }
_ = Test().with { $0.modifyFoo("non-throwing") }
_ = try Test().with { try $0.modifyFooThrowing("throwing") }

Task {
  _ = await Test().with { await $0.modifyFooAsync("async") }
  _ = try await Test().with { try await $0.modifyFooAsyncThrowing("async/throwing") }
}

struct TestAlreadyHasWithFuncDefined {
  var foo: String?
  var bar: String?

  func with(_ modify: (inout Self) -> Void) -> Self {
    var copy = self
    modify(&copy)
    return copy
  }
}

_ = TestAlreadyHasWithFuncDefined.with
_ = TestAlreadyHasWithFuncDefined().with { $0.foo = "foo" }

// This type intentionally doesn't have a member named "with",
// because this exercises a different code path in name lookup.
struct TestWithImplicitSelf {
  var foo: String?

  func withFoo(_ foo: String) -> TestWithImplicitSelf {
    with { $0.modifyFoo(foo) }
  }

  func withFooThrowing(_ foo: String) throws -> TestWithImplicitSelf {
    try with { try $0.modifyFooThrowing(foo) }
  }

  func withFooAsync(_ foo: String) async -> TestWithImplicitSelf {
    await with { await $0.modifyFooAsync(foo) }
  }

  func withFooAsyncThrowing(_ foo: String) async throws -> TestWithImplicitSelf {
    try await with { try await $0.modifyFooAsyncThrowing(foo) }
  }

  mutating func modifyFoo(_ foo: String) {
    self.foo = foo
  }

  mutating func modifyFooThrowing(_ foo: String) throws {
    self.foo = foo
  }

  mutating func modifyFooAsync(_ foo: String) async {
    self.foo = foo
  }

  mutating func modifyFooAsyncThrowing(_ foo: String) async throws {
    self.foo = foo
  }
}

@dynamicMemberLookup
struct DynamicMemberLookupTest {
  var foo: String?
  
  subscript(dynamicMember member: String) -> String {
    "dymamic member \(member)"
  }
}

_ = DynamicMemberLookupTest().with.uppercased()
_ = DynamicMemberLookupTest().with { $0.foo = "withFoo" }

let dynamicMemberString = DynamicMemberLookupTest().with
_ = dynamicMemberString.uppercased()

var anyValue: Any = 123
_ = anyValue.with { $0 = "\($0)!" }
_ = anyValue.with { $0 += 10 } // expected-error{{type of expression is ambiguous without a type annotation}}
_ = anyValue.with { (int: inout Int) in int += 10 } // expected-error{{cannot convert value of type '(inout Int) -> ()' to expected argument type '(inout Any) -> Void'}}

var voidValue: Void = ()
_ = voidValue.with { $0 = () }

_ = anyValue.with { value in
  if let int = value as? Int {
    value = int + 1
  }
}

var existentialCollection: any MutableCollection & BidirectionalCollection = ["a", "b", "c"]

_ = existentialCollection.with { $0.reverse() }
_ = existentialCollection.with { $0 = ContiguousArray([1, 2]) }
_ = existentialCollection.with { $0.shuffle() } // expected-error{{referencing instance method 'shuffle()' on 'MutableCollection' requires that 'Self' conform to 'RandomAccessCollection'}}

extension Collection {
  func `func`() { }
  func throwingFunc() throws { }
  func asyncFunc() async { }
  func asyncThrowingFunc() async throws { }
}

_ = existentialCollection.with { $0.func() }
_ = try existentialCollection.with { try $0.throwingFunc() }

Task {
  _ = await existentialCollection.with { await $0.asyncFunc() }
  _ = try await existentialCollection.with { try await $0.asyncThrowingFunc() }
}

func genericFunc<T>(on t: T) -> T {
  t.with { $0 = t }
}

_ = genericFunc(on: anyValue)
_ = genericFunc(on: tuple)
_ = genericFunc(on: Test())
_ = genericFunc(on: existentialCollection)
