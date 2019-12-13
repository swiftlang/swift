// RUN: %target-typecheck-verify-swift -disable-availability-checking

@_functionBuilder
struct TupleBuilder { // expected-note 2{{struct 'TupleBuilder' declared here}}
  static func buildBlock() -> () { }
  
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }
  
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T>(_ value: T) -> T { return value }
  static func buildIf<T>(_ value: T?) -> T? { return value }
}

@_functionBuilder
struct TupleBuilderWithoutIf { // expected-note {{struct 'TupleBuilderWithoutIf' declared here}}
  static func buildBlock() -> () { }
  
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }
  
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T>(_ value: T) -> T { return value }
}

func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

func tuplifyWithoutIf<T>(_ cond: Bool, @TupleBuilderWithoutIf body: (Bool) -> T) {
  print(body(cond))
}

func testDiags() {
  // For loop
  tuplify(true) { _ in
    17
    for c in name { // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilder'}}
    // expected-error@-1 {{use of unresolved identifier 'name'}}
    }
  }

  // Declarations
  tuplify(true) { _ in
    17
    let x = 17 // expected-error{{closure containing a declaration cannot be used with function builder 'TupleBuilder'}}
    x + 25
  }

  // Statements unsupported by the particular builder.
  tuplifyWithoutIf(true) {
    if $0 {    // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilderWithoutIf'}}
      "hello"
    }
  }
}

struct A { }
struct B { }

func overloadedTuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) -> A { // expected-note {{found this candidate}}
  return A()
}

func overloadedTuplify<T>(_ cond: Bool, @TupleBuilderWithoutIf body: (Bool) -> T) -> B { // expected-note {{found this candidate}}
  return B()
}

func testOverloading(name: String) {
  let a1 = overloadedTuplify(true) { b in
    if b {
      "Hello, \(name)" 
    }
  }

  let _: A = a1

  _ = overloadedTuplify(true) { b in // expected-error {{ambiguous use of 'overloadedTuplify(_:body:)'}}
    b ? "Hello, \(name)" : "Goodbye"
    42
    overloadedTuplify(false) {
      $0 ? "Hello, \(name)" : "Goodbye"
      42
      if $0 {
        "Hello, \(name)" 
      }
    }
  }
}

protocol P {
  associatedtype T
}

struct AnyP : P {
  typealias T = Any
  init<T>(_: T) where T : P {}
}

struct TupleP<U> : P {
  typealias T = U
  init(_: U) {}
}

@_functionBuilder
struct Builder {
  static func buildBlock<S0, S1>(_ stmt1: S0, _ stmt2: S1) // expected-note {{required by static method 'buildBlock' where 'S1' = 'Label<_>.Type'}}
           -> TupleP<(S0, S1)> where S0: P, S1: P {
    return TupleP((stmt1, stmt2))
  }
}

struct G<C> : P where C : P {
  typealias T = C
  init(@Builder _: () -> C) {}
}

struct Text : P {
  typealias T = String
  init(_: T) {}
}

struct Label<L> : P where L : P { // expected-note {{'L' declared as parameter to type 'Label'}}
  typealias T = L
  init(@Builder _: () -> L) {}
}

func test_51167632() -> some P {
  AnyP(G { // expected-error {{type 'Label<_>.Type' cannot conform to 'P'; only struct/enum/class types can conform to protocols}}
    Text("hello")
    Label  // expected-error {{generic parameter 'L' could not be inferred}}
    // expected-note@-1 {{explicitly specify the generic arguments to fix this issue}} {{10-10=<<#L: P#>>}}
  })
}

struct SR11440 {
  typealias ReturnsTuple<T> = () -> (T, T)
  subscript<T, U>(@TupleBuilder x: ReturnsTuple<T>) -> (ReturnsTuple<U>) -> Void { //expected-note {{in call to 'subscript(_:)'}}
    return { _ in }
  }

  func foo() {
    // This is okay, we apply the function builder for the subscript arg.
    self[{
      5
      5
    }]({
      (5, 5)
    })

    // But we shouldn't perform the transform for the argument to the call
    // made on the function returned from the subscript.
    self[{ // expected-error {{generic parameter 'U' could not be inferred}}
      5
      5
    }]({
      5
      5
    })
  }
}

func acceptInt(_: Int, _: () -> Void) { }

// SR-11350 crash due to improper recontextualization.
func erroneousSR11350(x: Int) {
  tuplify(true) { b in
    17
    x + 25
    Optional(tuplify(false) { b in
      if b {
        acceptInt(0) { }
      }
    }).domap(0) // expected-error{{value of type 'Optional<()>' has no member 'domap'}}
  }
}

func extraArg() {
  tuplify(true) { _ in
    1
    2
    3
    4
    5
    6 // expected-error {{extra argument in call}}
  }
}

// rdar://problem/53209000 - use of #warning and #error
tuplify(true) { x in
  1
  #error("boom")    // expected-error{{boom}}
  "hello"
  #warning("oops")  // expected-warning{{oops}}
  3.14159
}
