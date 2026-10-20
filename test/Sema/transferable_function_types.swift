// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -language-mode 6 -enable-experimental-feature NontransferableTypes

// REQUIRES: concurrency
// REQUIRES: swift_feature_NontransferableTypes

struct NonTransferable: ~Transferable {
  func use() {}
}

func takesTransferable(_: () -> Void) {}
func takesNontransferable(_: @nontransferable () -> Void) {}

func requiresTransferable<T>(_: T.Type) {}
// expected-note@-1 {{required by global function 'requiresTransferable' where 'T' = '@nontransferable () -> Void'}}

// Contextual type
do {
  func fn() {}

  let _: @nontransferable () -> Void = fn // Ok
  let nontransferable: @nontransferable () -> Void = {} // Ok

  let _: () -> Void = nontransferable
  // expected-error@-1 {{invalid conversion from non-'Transferable' function of type '@nontransferable () -> Void' to 'Transferable' function type '() -> Void'}}

  /* FIXME: Optionals don't support ~Transferable yet
  let nontransferableOpt: (@nontransferable () -> Void)? = fn // Ok
  let _: (() -> Void)? = nontransferableOpt // Error
  */
}

func testArgumentConversions(fn: @escaping () -> Void,
                             ntFn: @escaping @nontransferable () -> Void) {
  takesNontransferable(fn) // Ok
  takesNontransferable(ntFn) // Ok
  takesTransferable(fn) // Ok
  takesTransferable(ntFn)
  // expected-error@-1 {{invalid conversion from non-'Transferable' function of type '@nontransferable () -> Void' to 'Transferable' function type '() -> Void'}}
}

func joins(cond: Bool, fn: @escaping () -> Void, ntFn: @escaping @nontransferable () -> Void) {
  let joined = cond ? fn : ntFn // Ok (joins into @nontransferable)
  let _: @nontransferable () -> Void = joined // Ok
  let _: () -> Void = joined
  // expected-error@-1 {{invalid conversion from non-'Transferable' function of type '@nontransferable () -> Void' to 'Transferable' function type '() -> Void'}}
}

struct Storage { // expected-note {{consider adding '~Transferable' to struct 'Storage'}}
  let nt: @nontransferable () -> Void
  // expected-error@-1 {{stored property 'nt' of 'Transferable'-conforming struct 'Storage' has non-'Transferable' type '@nontransferable () -> Void'}}
}

struct NTStorage: ~Transferable {
  let nt: @nontransferable () -> Void // Ok
}

func testConformanceRequirements() {
  requiresTransferable((() -> Void).self) // Ok
  requiresTransferable((@nontransferable () -> Void).self)
  // expected-error@-1 {{type '@nontransferable () -> Void' cannot conform to 'Transferable'}}
}

func testSendableImpliesTransferable(
  _: @Sendable @nontransferable () -> Void,
  // expected-error@-1 {{'@Sendable' attribute is not allowed on non-'Transferable' types}}
  _: @nontransferable @Sendable () -> Void
  // expected-error@-1 {{'@Sendable' attribute is not allowed on non-'Transferable' types}}
) {
  let _ = { @Sendable @nontransferable in // expected-error {{'@Sendable' attribute is not allowed on non-'Transferable' types}}
  }

  let _: @nontransferable () -> Void = { @Sendable in // Ok
  }
}

// Global actor isolated function types and closures are inferred to be
// Sendable, unless they are @nontransferable.
@MainActor func testGlobalActorIsolated(
  ntFn: @escaping @MainActor @nontransferable () -> Void, // Ok
  nt: NonTransferable
) {
  let _: any Sendable = ntFn
  // expected-error@-1 {{type '@MainActor @nontransferable () -> ()' does not conform to the 'Sendable' protocol}}

  let explicit = { @MainActor @nontransferable in nt.use() } // Ok
  let _: any Sendable = explicit
  // expected-error@-1 {{type '@MainActor @nontransferable () -> ()' does not conform to the 'Sendable' protocol}}

  // The contextual type wins over the inferred Sendable, but not over an
  // explicit one.
  let _: @MainActor @nontransferable () -> Void = { @MainActor in nt.use() } // Ok
  let _: @MainActor @nontransferable () -> Void = { nt.use() } // Ok
  let _: @MainActor @nontransferable () -> Void = { @Sendable @MainActor in } // Ok
}
