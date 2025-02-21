// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature ExecutionAttribute

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

typealias F = @execution(concurrent) () async -> Void

typealias E = @execution(concurrent) () -> Void
// expected-error@-1 {{cannot use '@execution' on non-async function type}}

func test1(_: @execution(caller) (Int...) async -> Void) {}
func test2(_: @execution(concurrent) (Int...) async -> Void) {}

func test_err1_concurrent(_: @execution(concurrent) @MainActor () async -> Void) {}
// expected-error@-1 {{cannot use '@execution' because function type is isolated to a global actor 'MainActor'}}

func test_err1_caller(_: @execution(caller) @MainActor () async -> Void) {}
// expected-error@-1 {{cannot use '@execution' because function type is isolated to a global actor 'MainActor'}}

func test_err2_concurrent(_: @execution(concurrent) @isolated(any) () async -> Void) {}
// expected-error@-1 {{cannot use '@execution' together with @isolated(any)}}

func test_err2_caller(_: @execution(caller) @isolated(any) () async -> Void) {}
// expected-error@-1 {{cannot use '@execution' together with @isolated(any)}}

func test_err3_concurrent(_: @execution(concurrent) (isolated (any Actor)?) async -> Void) {}
// expected-error@-1 {{cannot use '@execution' together with an isolated parameter}}

func test_err3_caller(_: @execution(caller) (isolated (any Actor)?) async -> Void) {}
// expected-error@-1 {{cannot use '@execution' together with an isolated parameter}}

func test_err4(_: @execution (Int) -> Void) {}
// expected-error@-1 {{expected 'concurrent' or 'caller' as the execution behavior}}
// expected-error@-2 {{expected parameter type following ':'}}

func test_err5(_: @execution( () async -> Void) {}
// expected-error@-1 {{expected 'concurrent' or 'caller' as the execution behavior}}
// expected-note@-2 {{to match this opening '('}}
// expected-error@-3 {{expected ')' after execution behavior}}

func test_err6(_: @execution(hello) () async -> Void) {}
// expected-error@-1 {{expected 'concurrent' or 'caller' as the execution behavior}}

func test_err7(_: @execution(hello () async -> Void) {}
// expected-error@-1 {{expected 'concurrent' or 'caller' as the execution behavior}}
// expected-note@-2 {{to match this opening '('}}
// expected-error@-3 {{expected ')' after execution behavior}}

func test_err8(_: @execution(concurrent) Int) {} // expected-error {{attribute does not apply to type}}

do {
  let _ = [@execution(caller) () async -> Void]()
  let _ = [@execution(caller) () -> Void]()
  // expected-error@-1 {{cannot use '@execution' on non-async function type}}
}

