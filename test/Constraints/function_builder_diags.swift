// RUN: %target-typecheck-verify-swift

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

func overloadedTuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) -> A {
  return A()
}

func overloadedTuplify<T>(_ cond: Bool, @TupleBuilderWithoutIf body: (Bool) -> T) -> B {
  return B()
}

func testOverloading(name: String) {
  let a1 = overloadedTuplify(true) { b in
    if b {
      "Hello, \(name)" 
    }
  }

  let _: A = a1

  _ = overloadedTuplify(true) { b in
    b ? "Hello, \(name)" : "Goodbye"
    42
  }
}
