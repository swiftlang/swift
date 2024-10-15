// RUN: %target-typecheck-verify-swift -disable-availability-checking

enum Either<T,U> {
  case first(T)
  case second(U)
}

@resultBuilder
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

  // Okay: applies the result builder @TupleBuilder.
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
// the result builder.
struct DoNotTupleMe {
  var condition: Bool

  var tuple: some Any { // expected-error{{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}
    "hello" // expected-warning{{string literal is unused}}
    "world" // expected-warning{{string literal is unused}}
    // expected-note@-1 {{did you mean to return the last expression?}} {{5-5=return }}
  }
}

extension DoNotTupleMe: Tupled { }

@resultBuilder
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

  // Okay: applies the result builder @TupleBuilder, even though it satisfies
  // two requirements. (They have the same result builder)
  var tuple: some Any {
    "hello"
    if condition {
      "nested"
    }
    3.14159
    "world"
  }
}

protocol Tupled3 {
  associatedtype TupleType

  var tuple: TupleType { @TupleBuilder get }
}

struct TupleMe3: Tupled3 {
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

protocol OtherTupled {
  associatedtype OtherTupleType
  
  @OtherTupleBuilder var tuple: OtherTupleType { get }
}

struct AmbigTupleMe: Tupled, OtherTupled {
  var condition: Bool

  // Ambiguous
  // expected-note@+4{{add an explicit 'return' statement to not use a result builder}}{{+3:3-3=return <#expr#>\n}}
  // expected-note@+3{{apply result builder 'TupleBuilder' (inferred from protocol 'Tupled')}}{{-1:3-3=@TupleBuilder }}
  // expected-note@+2{{apply result builder 'OtherTupleBuilder' (inferred from protocol 'OtherTupled')}}{{-1:3-3=@OtherTupleBuilder }}
  internal
  var tuple: Void { // expected-error{{ambiguous result builder inferred for 'tuple': 'TupleBuilder' or 'OtherTupleBuilder'}}
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

// Inference through dynamic replacement
struct DynamicTupled: Tupled {
  dynamic var tuple: some Any {
    return "hello"
  }
}

extension DynamicTupled {
  @_dynamicReplacement(for: tuple)
  var replacementTuple: some Any {
    1
    3.14159
    "hello"
  }
}

struct DynamicTupled2: Tupled, OtherTupled {
  dynamic var tuple: some Any {
    return "hello"
  }
}

extension DynamicTupled2 {
  @_dynamicReplacement(for: tuple)
  var replacementTuple: some Any { // expected-error{{ambiguous result builder inferred for 'replacementTuple': 'TupleBuilder' or 'OtherTupleBuilder'}}
    // expected-note@-1{{add an explicit 'return' statement to not use a result builder}}
    // expected-note@-2{{apply result builder 'TupleBuilder' (inferred from protocol 'Tupled')}}
    // expected-note@-3{{apply result builder 'OtherTupleBuilder' (inferred from protocol 'OtherTupled')}}
    1
  }
}

struct DynamicTupled3 {
  @TupleBuilder dynamic var dynamicTuple: some Any {
    0
  }
}

extension DynamicTupled3: OtherTupled {
  @_dynamicReplacement(for: dynamicTuple)
  var tuple: some Any { // expected-error{{ambiguous result builder inferred for 'tuple': 'OtherTupleBuilder' or 'TupleBuilder'}}
    // expected-note@-1{{add an explicit 'return' statement to not use a result builder}}
    // expected-note@-2{{apply result builder 'OtherTupleBuilder' (inferred from protocol 'OtherTupled')}}
    // expected-note@-3{{apply result builder 'TupleBuilder' (inferred from dynamic replacement of 'dynamicTuple')}}
    0
  }
}

do {
  @resultBuilder
  enum BuildBoolFrom<T> {
    static func buildBlock(_ t: T...) -> Bool {}
  }

  protocol P {
    @BuildBoolFrom<String>
    var property: Bool { get }
  }

  struct Conformer1: P {
    @BuildBoolFrom<Int>
    var property: Bool {
      // OK, explicit result builder disables inference through protocol.
      1
      2
    }
  }

  struct Conformer2: P {
    var property: Bool {
      @BuildBoolFrom<Int>
      get {
        // OK, explicit result builder disables inference through protocol.
        1
        2
      }
    }
  }
}
