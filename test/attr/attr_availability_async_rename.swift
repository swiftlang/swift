// REQUIRES: concurrency
// REQUIRES: objc_interop

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs/custom-modules
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -parse-as-library -I %S/Inputs/custom-modules

import ObjcAsync

@available(*, renamed: "asyncFunc(_:)")
func goodFunc1(value: String, completionHandler: @escaping (Int) -> Void) {}

@available(*, renamed: "asyncFunc(_:)")
func goodFunc2(value: String, completionHandler: @escaping (Int) -> Void) {}

// Renamed decl doesn't match any function, so alternative shouldn't be found
@available(*, renamed: "asyncFunc()")
func badFunc(value: String, completionHandler: @escaping (Int) -> Void) {}

// expected-note@+1 4 {{'asyncFunc' declared here}}
func asyncFunc(_ text: String) async -> Int { }

struct SomeStruct {
  @available(*, renamed: "structFunc")
  func structFunc(continuation: @escaping () -> Void) { }

  // expected-note@+1{{'structFunc()' declared here}}
  func structFunc() async { }

  @available(*, renamed: "staticStructFunc")
  static func staticStructFunc(completionHandler: @escaping () -> Void) { }

  // expected-note@+1 2 {{'staticStructFunc()' declared here}}
  static func staticStructFunc() async { }
}

@available(*, renamed: "overloaded()")
func asyncOnlyOverload(completionHandler: @escaping () -> Void) { }
func overloaded() { }
// expected-note@+1 {{'overloaded()' declared here}}
func overloaded() async { }

// Renamed decl is ambiguous but the params only match a single case, so we
// should resolve the function
@available(*, renamed: "overloadedAsyncFunc(value:)")
func nonAmbiguousFunc(value: Int, handler: @escaping () -> Void) {}
// expected-note@+1 {{'overloadedAsyncFunc(value:)' declared here}}
func overloadedAsyncFunc(value: Int) async {}
func overloadedAsyncFunc(value: String) async {}

// Renamed decl is ambiguous and there's multiple matches, so can't resolve the
// function
@available(*, renamed: "asyncFuncDifferentParamNames")
func ambiguousFunc(value: Int, handler: @escaping () -> Void) {}
func asyncFuncDifferentParamNames(value: Int) async {}
func asyncFuncDifferentParamNames(value2: Int) async {}

// Renamed decl params don't match, so shouldn't resolved
@available(*, renamed: "noMatchingParamsIntFunc(value:)")
func noMatchingParamsFunc(value: Character, handler: @escaping () -> Void) {}
func noMatchingParamsIntFunc(value: Int) async {}

// Matching function isn't async, so shouldn't be resolved
@available(*, renamed: "noMatchingSyncFunc(value:)")
func noMatchingAsyncFunc(value: Int, handler: @escaping () -> Void) {}
func noMatchingSyncFunc(value: Int) {}

// Suggest using async alternative function in async context

func asyncContext(t: HandlerTest) async {
  // expected-warning@+1{{consider using asynchronous alternative function}}
  goodFunc1(value: "Hello") { _ in }

  let _ = {
    // No warning or error since we're in a sync context here
    goodFunc1(value: "Hello") { _ in }
  }

  let _ = { () async -> () in
    let _ = await asyncFunc("Hello World")
    // expected-warning@+1{{consider using asynchronous alternative function}}
    goodFunc1(value: "Hello") { _ in }
  }

  let _ = await asyncFunc("World")

  // expected-warning@+1{{consider using asynchronous alternative function}}
  asyncOnlyOverload() { }

  // expected-warning@+1{{consider using asynchronous alternative function}}
  nonAmbiguousFunc(value: 1) { }

  // These don't get the warning because we failed to resolve the name to a
  // single async decl
  badFunc(value: "Hello") { _ in }
  ambiguousFunc(value: 1) { }
  noMatchingParamsFunc(value: "c") { }
  noMatchingAsyncFunc(value: 1) { }

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.simple { _ in }
  _ = await t.simple()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.simpleArg(1) { _ in }
  _ = await t.simpleArg(1)

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.alias { _ in }
  _ = await t.alias()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.error { _, _ in }
  _ = try! await t.error()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  try! t.removedError { _, _ in }
  _ = try! await t.removedError()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.asyncImportSame(1, completionHandler: { _ in })
  _ = await t.asyncImportSame(1)

  // Marked with swift_async(none), so shouldn't have a warning about using it
  t.asyncImportSame(1, replyTo: { _ in })
}

func syncContext(t: HandlerTest) {
  goodFunc1(value: "Hello") { _ in }
  t.simple { _ in }
  t.simpleArg(1) { _ in }
  t.alias { _ in }
  t.error { _, _ in }
  try! t.removedError { _, _ in }
  t.asyncImportSame(1, completionHandler: { _ in })
  t.asyncImportSame(1, replyTo: { _ in })
}

let asyncGlobalClosure = { () async -> () in
  // expected-warning@+1{{consider using asynchronous alternative function}}
  goodFunc1(value: "neat") { _ in }
}

class ClassCallingAsyncStuff {
  struct NestedStruct {
    @available(*, renamed: "structFunc()")
    func structCompFunc(handler: @escaping () -> ()) { }

    // expected-note@+1{{'structFunc()' declared here}}
    func structFunc() async {}
  }

  // expected-note@+1 4 {{'asyncFunc()' declared here}}
  func asyncFunc() async {}

  @available(*, renamed: "asyncFunc()")
  func compHandlerFunc(handler: @escaping () -> ()) {}

  @available(*, renamed: "asyncFunc()")
  func compAsyncHandlerFunc(handler: @escaping () async -> ()) {}

  func async1() async {
    // expected-warning@+1{{consider using asynchronous alternative function}}
    goodFunc1(value: "hi") { _ in }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    compAsyncHandlerFunc() { [self] () async -> () in
      // expected-warning@+1{{consider using asynchronous alternative function}}
      compAsyncHandlerFunc() { [self] () async -> () in
        // expected-warning@+1{{consider using asynchronous alternative function}}
        compHandlerFunc() { print("foo") }
      }
    }
  }

  func instanceFunc(other: ClassCallingAsyncStuff) async {
    // expected-error@+1{{cannot find 'c' in scope}}
    c.compHandlerFunc() { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.compHandlerFunc() { }
  }

  func structFunc(other: NestedStruct) async {
    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.structCompFunc() { }
  }

  func structFunc(other: SomeStruct) async {
    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.structFunc() { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    SomeStruct.staticStructFunc { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    type(of: other).staticStructFunc { }
  }

  // no warning
  let funFunc = goodFunc1
}
