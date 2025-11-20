// RUN: %target-typecheck-verify-swift -disable-availability-checking

// REQUIRES: concurrency

typealias F = @concurrent () async -> Void

typealias E = @concurrent () -> Void
// expected-error@-1 {{cannot use '@concurrent' on non-async function type}}

func test1(_: nonisolated(nonsending) (Int...) async -> Void) {}
func test2(_: @concurrent (Int...) async -> Void) {}

func test_err1_concurrent(_: @concurrent @MainActor () async -> Void) {}
// expected-error@-1 {{cannot use '@concurrent' because function type is isolated to a global actor 'MainActor'}}

func test_err1_caller(_: nonisolated(nonsending) @MainActor () async -> Void) {}
// expected-error@-1 {{cannot use 'nonisolated(nonsending)' because function type is isolated to a global actor 'MainActor'}}

func test_err2_concurrent(_: @concurrent @isolated(any) () async -> Void) {}
// expected-error@-1 {{cannot use '@concurrent' together with '@isolated(any)'}}

func test_err2_caller(_: nonisolated(nonsending) @isolated(any) () async -> Void) {}
// expected-error@-1 {{cannot use 'nonisolated(nonsending)' together with '@isolated(any)'}}

func test_err3_concurrent(_: @concurrent (isolated (any Actor)?) async -> Void) {}
// expected-error@-1 {{cannot use '@concurrent' together with an isolated parameter}}

func test_err3_caller(_: nonisolated(nonsending) (isolated (any Actor)?) async -> Void) {}
// expected-error@-1 {{cannot use 'nonisolated(nonsending)' together with an isolated parameter}}

func test_err4(_: nonisolated (Int) -> Void) {}
// expected-error@-1 {{expected 'nonsending' in modifier}}
// expected-error@-2 {{expected '{' in body of function declaration}}
// expected-warning@-3 {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}}
// expected-error@-4 {{consecutive statements on a line must be separated by ';'}}
// expected-error@-5 {{expected expression}}

func test_err5(_: nonisolated( () async -> Void) {}
// expected-error@-1 {{expected 'nonsending' in modifier}}

func test_err6(_: nonisolated(hello) () async -> Void) {}
// expected-error@-1 {{expected 'nonsending' in modifier}}
// expected-error@-2 {{cannot have more than one parameter list}}
// expected-error@-3 {{cannot find type 'hello' in scope}}
// expected-error@-4 {{onsecutive statements on a line must be separated by ';'}}
// expected-error@-5 {{expected expression}}

func test_err7(_: nonisolated(hello () async -> Void) {}
// expected-error@-1 {{expected 'nonsending' in modifier}}
// expected-error@-2 {{cannot find type 'hello' in scope}}

func test_err8(_: @concurrent Int) {} // expected-error {{attribute does not apply to type}}

do {
  let _ = [nonisolated(nonsending) () async -> Void]()
  let _ = [nonisolated(nonsending) () -> Void]()
  // expected-error@-1 {{cannot use 'nonisolated(nonsending)' on non-async function type}}
}

protocol P {}

struct S : nonisolated
             P { // Ok
}

do {
  func nonisolated() {}

  // `nonisolated` is parsed as a function call
  nonisolated // expected-error {{function is unused}}
  (42) // expected-warning {{integer literal is unused}}

  let _: nonisolated // expected-error {{cannot find type 'nonisolated' in scope}}
    (Int) async -> Void // expected-error {{expected member name or initializer call after type name}}
  // expected-note@-1 {{use '.self' to reference the type object}}
  // expected-warning@-2 {{expression of type '((Int) async -> Void).Type' is unused}}

  _ = [nonisolated()]
}

do {
  func nonisolated(_: Int) -> Int { 42 }

  nonisolated(0) // expected-warning {{result of call to 'nonisolated' is unused}}
  print("hello")
}
