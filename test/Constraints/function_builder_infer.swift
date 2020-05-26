// RUN: %target-typecheck-verify-swift -disable-availability-checking

enum Either<T,U> {
  case first(T)
  case second(U)
}

@_functionBuilder
struct TupleBuilder {
  static func buildBlock() -> () {
    return ()
  }

  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
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

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
}

protocol Tupled {
  associatedtype TupleType
  
  @TupleBuilder var tuple: TupleType { get }
}

struct TupleMe: Tupled {
  var condition: Bool

  // Okay: applies the function builder @TupleBuilder.
  var tuple: some Any {
    "hello"
    if condition {
      "nested"
    }
    3.14159
    "world"
  }
}

// Witness is separated from the context declaring conformance, so don't infer
// the function builder.
struct DoNotTupleMe {
  var condition: Bool

  var tuple: some Any { // expected-error{{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}
    "hello" // expected-warning{{string literal is unused}}
    "world" // expected-warning{{string literal is unused}}
  }
}

extension DoNotTupleMe: Tupled { }

@_functionBuilder
struct OtherTupleBuilder {
  static func buildBlock() -> () {
    return ()
  }

  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
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

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
}

protocol Tupled2 {
  associatedtype TupleType
  
  @TupleBuilder var tuple: TupleType { get }
}

struct TupleMe2: Tupled, Tupled2 {
  var condition: Bool

  // Okay: applies the function builder @TupleBuilder, even though it satisfies
  // two requirements. (They have the same function builder)
  var tuple: some Any {
    "hello"
    if condition {
      "nested"
    }
    3.14159
    "world"
  }
}

protocol OtherTupled {
  associatedtype OtherTupleType
  
  @OtherTupleBuilder var tuple: OtherTupleType { get }
}

struct AmbigTupleMe: Tupled, OtherTupled {
  var condition: Bool

  // Ambiguous
  internal
  var tuple: Void { // expected-error{{ambiguous function builder inferred for 'tuple': 'TupleBuilder' or 'OtherTupleBuilder'}}
    // expected-note@-1{{add an explicit 'return' statement to not use a function builder}}{{3-3=return <#expr#>\n}}
    // expected-note@-2{{apply function builder 'TupleBuilder' (inferred from protocol 'Tupled')}}{{3-3=@TupleBuilder }}
    // expected-note@-3{{apply function builder 'OtherTupleBuilder' (inferred from protocol 'OtherTupled')}}{{3-3=@OtherTupleBuilder }}
    "hello" // expected-warning{{string literal is unused}}
    "world" // expected-warning{{string literal is unused}}
  }
}

// Separating the conformances resolves the ambiguity.
struct TupleMeResolvedSeparate: Tupled {
  var condition: Bool

  var tuple: some Any {
    "hello"
    if condition {
      "nested"
    }
    3.14159
    "world"
  }
}

extension TupleMeResolvedSeparate: OtherTupled { }

struct TupleMeResolvedExplicit: Tupled, OtherTupled {
  var condition: Bool

  var tuple: some Any {
    return "hello"
  }
}
