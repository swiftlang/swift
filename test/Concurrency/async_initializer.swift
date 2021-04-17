// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency


////
// some functions to play with

func f() async {
  let _ = Person() // expected-error {{call is 'async' but is not marked with 'await'}} {{11-11=await }}
}

func g() {}

////
// test super.init interactions

class Person {
  init() async {
    f() // expected-error {{call is 'async' but is not marked with 'await'}} {{5-5=await }}
  }

  convenience init(_ s: String) async {
    await self.init()
  }
}

class Bertrand: Person {
  override init() {} // expected-error {{missing call to superclass's initializer; 'super.init' is 'async' and requires an explicit call}}
  init(_ x: Int) async {} // expected-error {{missing call to superclass's initializer; 'super.init' is 'async' and requires an explicit call}}
}

class Barbara: Person {
  // expected-note@+2 {{add 'async' to function 'init(_:)' to make it asynchronous}} {{20-20= async}}
  // expected-error@+1 {{missing call to superclass's initializer; 'super.init' is 'async' and requires an explicit call}}
  init(_ d: Double) {
    f() // expected-error{{'async' call in a function that does not support concurrency}}
  }

  init(x: Int, y: Int) async {
    await super.init()
  }

  convenience init(a: Double, b: Double) async {
    await self.init(x: 0, y: 0)
  }
}

class Fruit {
  init() async {}
  init(name: String) {}
}

class Banana: Fruit {
  override init() {
    super.init(name: "banana")
  }
}

class Cat {} // expected-note {{overridden declaration is here}}

class Calico: Cat {
  override init() async {} // expected-error {{cannot override non-async initializer with async initializer}}
}

func reconstruct(c: Cat) {
  c.init() // expected-error {{'init' is a member of the type}}
}

////
// test reasync initializers

class MyType {
  init(_ f: () async -> Void) reasync {
    await f()
  }
}

func beep() async {
  let _ = MyType(f) // expected-error{{call is 'async' but is not marked with 'await'}}
  let _ = await MyType(f)

  let _ = MyType(g)
}

////
// test other types with constructors

actor A {
  init() async {
    await f()
  }
}

// NOTE: actor inheritance is probably being removed soon, so just remove this def of B
actor B: A {
  init(x : String) async {} // expected-error {{missing call to superclass's initializer; 'super.init' is 'async' and requires an explicit call}}
}

enum E {
  init() async {
    await f()
  }
}

struct SomeStruct {
  @MainActor init(asyncMainActor: Int) async {} // expected-note{{calls to initializer 'init(asyncMainActor:)' from outside of its actor context are implicitly asynchronous}}
  @MainActor init(mainActor: Int) {} // expected-note {{calls to initializer 'init(mainActor:)' from outside of its actor context are implicitly asynchronous}}
  @MainActor(unsafe) init(asyncMainActorUnsafe: Int) async {}
  @MainActor(unsafe) init(mainActorUnsafe: Int) {}
}

// expected-note@+2 2{{add '@MainActor' to make global function 'globActorTest1()' part of global actor 'MainActor'}}
// expected-note@+1 2 {{add 'async' to function 'globActorTest1()' to make it asynchronous}}
func globActorTest1() {
  _ = SomeStruct(asyncMainActor: 0) // expected-error {{'async' call in a function that does not support concurrency}}
  // expected-error@-1{{call to main actor-isolated initializer 'init(asyncMainActor:)' in a synchronous nonisolated context}}

  _ = SomeStruct(mainActor: 0) // expected-error {{call to main actor-isolated initializer 'init(mainActor:)' in a synchronous nonisolated context}}

  _ = SomeStruct(asyncMainActorUnsafe: 0) // expected-error {{'async' call in a function that does not support concurrency}}

  _ = SomeStruct(mainActorUnsafe: 0)
}

func globActorTestAsyncEdition() async {
  _ = await SomeStruct(asyncMainActor: 0)
  _ = await SomeStruct(mainActor: 0)
  _ = await SomeStruct(asyncMainActorUnsafe: 0)
  _ = await SomeStruct(mainActorUnsafe: 0)
}

////
// check protocol conformance & inheritance

protocol AsyncDefaultConstructable {
  init() async
}

protocol DefaultConstructable {
  init() // expected-note {{protocol requires initializer 'init()' with type '()'; do you want to add a stub?}} {{43-43=\n    init() {\n        <#code#>\n    \}\n}}
}

struct Location {
  var x : Int
  var y : Int
  init() async { // expected-note {{candidate is 'async', but protocol requirement is not}}
    self.x = 0
    self.y = 0
  }
}

extension Location: DefaultConstructable {} // expected-error {{type 'Location' does not conform to protocol 'DefaultConstructable'}}

extension Location: AsyncDefaultConstructable {}

protocol Plain {
  // expected-note@+2 {{overridden declaration is here}}
  // expected-note@+1 {{attempt to override convenience initializer here}}
  init()
}

protocol Spicy: Plain {
  // expected-error@+2 {{cannot override non-async initializer with async initializer}}
  // expected-error@+1 {{initializer does not override a designated initializer from its parent protocol}}
  override init() async
}
