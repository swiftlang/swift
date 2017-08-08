// RUN: %target-swift-frontend -typecheck -verify %s

func asyncThing() async -> Int {}

func test1(asyncfp : () async -> Int, fp : () -> Int) async {
  _ = await asyncfp()
  _ = await asyncfp() + asyncfp()
  _ = await asyncfp() + fp()
  _ = await fp() + 42  // expected-warning {{no calls to 'async' functions occur within 'await' expression}}
  _ = asyncfp() // expected-error {{call is 'async' but is not marked with 'await'}}

  
  await test1(asyncfp: asyncThing, fp: {42})

  await test1(asyncfp: { () async in 42},
        fp: { () async in 42})  // expected-error {{invalid removal of 'async' when converting function of type '() async -> Int' to '() -> Int'}}
}

func test2(asyncfp : () async -> Int, fp : () -> Int) {
  _ = asyncfp()  // expected-error {{call is 'async' but is not marked with 'await'}}
  _ = await asyncfp()  // expected-error {{async expression is not allowed in a non-async function}}
  _ = fp()
}


struct S {
  var x = await asyncThing()  // expected-error {{async expression is not allowed in an instance variable initializer}}
  var y = asyncThing()  // expected-error {{call is 'async' but is not marked with 'await'}}
}

func testOverloads() {}        // expected-note {{'testOverloads()' previously declared here}}
func testOverloads() async {}  // expected-error {{invalid redeclaration of 'testOverloads()'}}

// Test overrides.
class Base {
  init() async {}
  func foo() async {}
  func bar() async {}   // expected-note {{overridden declaration is here}} expected-note {{potential overridden instance method 'bar()' here}}
  func baz() {}   // expected-note {{overridden declaration is here}} expected-note {{potential overridden instance method 'baz()' here}}
}
class Derived : Base {
  override init() async {
    await super.init()
  }
  override func foo() async {}  // ok
  override func bar() {}        // expected-error {{cannot override async method with non-async method}} expected-error {{method does not override any method from its superclass}}
  override func baz() async {}  // expected-error {{cannot override non-async method with async method}} expected-error {{method does not override any method from its superclass}}
}

// Protocol requirements.
protocol P {
  func f() async
  func g() async // expected-note {{protocol requires function 'g()' with type '() async -> ()'; do you want to add a stub?}}
  func h()       // expected-note {{protocol requires function 'h()' with type '() -> ()'; do you want to add a stub?}}
}

struct PI : P {  // expected-error {{type 'PI' does not conform to protocol 'P'}}
  func f() async {}
  func g() {}       // expected-note {{candidate and protocol requirement are not both 'async'}}
  func h() async {} // expected-note {{candidate and protocol requirement are not both 'async'}}
}

