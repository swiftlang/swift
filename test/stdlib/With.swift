// RUN: %target-typecheck-verify-swift -disable-availability-checking

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

// Why doesn't this work? Does this need to be allowed in the first place?
// TODO: Fix and uncomment
// let any: Any = "string value"
// any.with { any in
  // unexpected error: cannot assign value of type 'Any' to type 'Any'
  // any = "new string value"
// }

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
let with: ((inout Test) -> Void) -> Test = Test().with
let with_throws: ((inout Test) throws -> Void) throws -> Test  = Test().with
let with_async: ((inout Test) async -> Void) async -> Test  = Test().with
let with_async_throws: ((inout Test) async throws -> Void) async throws -> Test = Test().with

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
