// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx50

// REQUIRES: OS=macosx

@available(*, unavailable)
@resultBuilder
struct UnavailableBuilder {
// expected-note@-1 2 {{'UnavailableBuilder' has been explicitly marked unavailable here}}
  static func buildBlock() {}
}

@UnavailableBuilder public func usesUnavailableBuilder() {}
// expected-error@-1 {{'UnavailableBuilder' is unavailable}}

public func takesUnavailableBuilder(@UnavailableBuilder _ fn: () -> ()) {}
// expected-error@-1 {{'UnavailableBuilder' is unavailable}}

enum Either<T,U> {
  case first(T)
  case second(U)
}

@resultBuilder
struct TupleBuilder {
// expected-note@-1{{add 'buildLimitedAvailability(_:)' to the result builder 'TupleBuilder' to erase type information for less-available types}}{{22-22=\n    static func buildLimitedAvailability(_ component: <#Component#>) -> <#Component#> {\n      <#code#>\n    \}}}
// expected-note@-2{{add 'buildLimitedAvailability(_:)' to the result builder 'TupleBuilder' to erase type information for less-available types}}{{22-22=\n    static func buildLimitedAvailability(_ component: <#Component#>) -> <#Component#> {\n      <#code#>\n    \}}}
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

func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

@available(OSX, introduced: 10.9)
func globalFuncAvailableOn10_9() -> Int { return 9 }

@available(OSX, introduced: 51)
func globalFuncAvailableOn51() -> Int { return 10 }

@available(OSX, introduced: 52)
struct Only52 { }

@available(OSX, introduced: 52)
func globalFuncAvailableOn52() -> Only52 { .init() }

tuplify(true) { cond in
  globalFuncAvailableOn10_9()
  if #available(OSX 51, *) {
    globalFuncAvailableOn51()
    tuplify(false) { cond2 in
      if cond, #available(OSX 52, *) { 
        // expected-warning@-1{{result builder 'TupleBuilder' does not implement 'buildLimitedAvailability'; this code may crash on earlier versions of the OS}}
        cond2
        globalFuncAvailableOn52()
      } else if true {
        globalFuncAvailableOn52() // expected-error{{'globalFuncAvailableOn52()' is only available in macOS 52 or newer}}
        // expected-note@-1{{add 'if #available' version check}}
      } else if false {
        globalFuncAvailableOn52() // expected-error{{'globalFuncAvailableOn52()' is only available in macOS 52 or newer}}
        // expected-note@-1{{add 'if #available' version check}}
      } else {
        globalFuncAvailableOn52() // expected-error{{'globalFuncAvailableOn52()' is only available in macOS 52 or newer}}
        // expected-note@-1{{add 'if #available' version check}}
      }
      if cond, #unavailable(OSX 52) { 
        // expected-warning@-1{{result builder 'TupleBuilder' does not implement 'buildLimitedAvailability'; this code may crash on earlier versions of the OS}}
        cond2
        globalFuncAvailableOn52() // expected-error{{'globalFuncAvailableOn52()' is only available in macOS 52 or newer}}
        // expected-note@-1{{add 'if #available' version check}}
      } else if true {
        globalFuncAvailableOn52()
      } else if false {
        globalFuncAvailableOn52()
      }
    }
  }
}

// Function builder that can perform type erasure for #available.
@resultBuilder
struct TupleBuilderAvailability {
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

  static func buildLimitedAvailability<T>(_ value: T) -> Any {
    return value
  }
}

func tuplifyWithAvailabilityErasure<T>(_ cond: Bool, @TupleBuilderAvailability body: (Bool) -> T) {
  print(body(cond))
}

tuplifyWithAvailabilityErasure(true) { cond in
  if cond, #available(OSX 52, *) {
    globalFuncAvailableOn52()
  }

  if cond, #unavailable(OSX 52) {
    cond
  } else {
    globalFuncAvailableOn52()
  }

  // https://github.com/apple/swift/issues/63764
  if #unavailable(OSX 52) {
    cond // Ok
  }
}

// rdar://97533700 – Make sure we can prefer an unavailable buildPartialBlock if
// buildBlock also isn't available.

@resultBuilder
struct UnavailableBuildPartialBlock {
  static func buildPartialBlock(first: Int) -> Int { 0 }

  @available(*, unavailable)
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { 0 }

  static func buildBlock(_ x: Int...) -> Int { 0 }
}

@UnavailableBuildPartialBlock
func testUnavailableBuildPartialBlock() -> Int {
  // We can use buildBlock here.
  2
  3
}

@resultBuilder
struct UnavailableBuildPartialBlockAndBuildBlock {
  @available(*, unavailable)
  static func buildPartialBlock(first: Int) -> Int { 0 }
  // expected-note@-1 {{'buildPartialBlock(first:)' has been explicitly marked unavailable here}}

  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { 0 }

  @available(*, unavailable)
  static func buildBlock(_ x: Int...) -> Int { 0 }
}

// We can still use buildPartialBlock here as both are unavailable.
@UnavailableBuildPartialBlockAndBuildBlock
func testUnavailableBuildPartialBlockAndBuildBlock() -> Int {
  // expected-error@-1 {{'buildPartialBlock(first:)' is unavailable}}
  2
  3
}

@available(*, unavailable)
@resultBuilder
struct UnavailableBuilderWithPartialBlock { // expected-note {{'UnavailableBuilderWithPartialBlock' has been explicitly marked unavailable here}}
  @available(*, unavailable)
  static func buildPartialBlock(first: String) -> Int { 0 }
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { 0 }
  static func buildBlock(_ x: Int...) -> Int { 0 }
}

@UnavailableBuilderWithPartialBlock // expected-error {{'UnavailableBuilderWithPartialBlock' is unavailable}}
func testUnavailableBuilderWithPartialBlock() -> Int {
  // The builder itself is unavailable, so we can still opt for buildPartialBlock.
  2 // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
  3
}
