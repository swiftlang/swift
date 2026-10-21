// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -language-mode 6 -enable-experimental-feature NontransferableTypes

// REQUIRES: concurrency
// REQUIRES: swift_feature_NontransferableTypes

struct NonTransferable: ~Transferable {
  func use() {}
}

class NonTransferableClass: ~Transferable {
  func use() {}
}

func takesTransferable(_: () -> Void) {}
func takesEscapingTransferable(_: @escaping () -> Void) {}
func takesNontransferable(_: @nontransferable () -> Void) {}
func takesEscapingNontransferable(_: @escaping @nontransferable () -> Void) {}

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
  let _: @MainActor @nontransferable () -> Void = { @Sendable @MainActor in nt.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
}

func testCaptures(nt: NonTransferable, ntc: NonTransferableClass, value: Int) {
  takesTransferable { nt.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  takesTransferable { [nt] in nt.use() }
  // expected-error@-1 {{capture list entry 'nt' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
  takesTransferable { [x = nt] in x.use() }
  // expected-error@-1 {{capture list entry 'x' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}

  takesEscapingTransferable { ntc.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'ntc'}}
  takesEscapingTransferable { [ntc] in ntc.use() }
  // expected-error@-1 {{capture list entry 'ntc' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferableClass'}}

  // The closure owns a capture list entry even if it's never used.
  takesTransferable { [nt] in () }
  // expected-error@-1 {{capture list entry 'nt' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
  // expected-warning@-2 {{capture 'nt' was never used}}

  // A closure is @nontransferable only if its contextual type is.
  let _: () -> Void = { nt.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  let _: () -> Void = { [nt] in nt.use() }
  // expected-error@-1 {{capture list entry 'nt' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
  _ = { nt.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  _ = { [nt] in nt.use() }
  // expected-error@-1 {{capture list entry 'nt' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}

  takesNontransferable { nt.use() } // Ok
  takesNontransferable { [nt] in nt.use() } // Ok
  takesEscapingNontransferable { ntc.use() } // Ok
  takesEscapingNontransferable { [ntc] in ntc.use() } // Ok
  let _: @nontransferable () -> Void = { nt.use() } // Ok
  let _: @nontransferable () -> Void = { [nt] in nt.use() } // Ok

  takesTransferable { _ = value } // Ok
  takesTransferable { [value] in _ = value } // Ok
  takesNontransferable { _ = value } // Ok
  takesNontransferable { [value] in _ = value } // Ok
}

func testAutoclosureCaptures(nt: NonTransferable) {
  func takesAutoclosure(_: @autoclosure () -> Void) {}
  func takesNontransferableAutoclosure(_: @autoclosure @nontransferable () -> Void) {}

  takesAutoclosure(nt.use())
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  takesNontransferableAutoclosure(nt.use()) // Ok
}

func testInoutCaptures(nt: inout NonTransferable) {
  takesTransferable { nt.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  takesTransferable { [nt] in nt.use() }
  // expected-error@-1 {{capture list entry 'nt' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
  takesNontransferable { nt.use() } // Ok
  takesNontransferable { [nt] in nt.use() } // Ok
}

func testFunctionCaptures(fn: @escaping () -> Void,
                          ntFn: @escaping @nontransferable () -> Void) {
  takesTransferable { ntFn() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'ntFn'}}
  takesTransferable { [ntFn] in ntFn() }
  // expected-error@-1 {{capture list entry 'ntFn' of 'Transferable'-conforming closure has non-'Transferable' type '@nontransferable () -> Void'}}
  takesNontransferable { ntFn() } // Ok
  takesNontransferable { [ntFn] in ntFn() } // Ok

  takesTransferable { fn() } // Ok
  takesTransferable { [fn] in fn() } // Ok
}

func testGenericCaptures<T: ~Transferable, U>(t: T, u: U) {
  takesTransferable { _ = t }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 't'}}
  takesTransferable { [t] in _ = t }
  // expected-error@-1 {{capture list entry 't' of 'Transferable'-conforming closure has non-'Transferable' type 'T'}}
  takesNontransferable { _ = t } // Ok
  takesNontransferable { [t] in _ = t } // Ok

  takesTransferable { _ = u } // Ok
  takesTransferable { [u] in _ = u } // Ok
}

extension NonTransferable {
  func testSelfCaptures() {
    takesTransferable { self.use() }
    // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'self'}}
    takesTransferable { use() }
    // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'self'}}
    takesTransferable { [self] in use() }
    // expected-error@-1 {{capture list entry 'self' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
    takesNontransferable { use() } // Ok
    takesNontransferable { [self] in use() } // Ok
  }
}

extension NonTransferableClass {
  func testSelfCaptures() {
    takesEscapingTransferable { self.use() }
    // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'self'}}
    takesEscapingTransferable { [self] in use() }
    // expected-error@-1 {{capture list entry 'self' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferableClass'}}
    takesEscapingNontransferable { self.use() } // Ok
    takesEscapingNontransferable { [self] in use() } // Ok
  }
}

func testNestedCaptures(nt: NonTransferable) {
  takesTransferable { takesNontransferable { nt.use() } }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  takesTransferable { takesNontransferable { [nt] in nt.use() } }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  takesNontransferable { takesNontransferable { nt.use() } } // Ok
  takesNontransferable { takesNontransferable { [nt] in nt.use() } } // Ok

  takesNontransferable { takesTransferable { nt.use() } }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  takesNontransferable { takesTransferable { [nt] in nt.use() } }
  // expected-error@-1 {{capture list entry 'nt' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
}

func testNestedCaptureListEntries(nt: NonTransferable) {
  takesNontransferable { [x = nt] in takesEscapingTransferable { x.use() } }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'x'}}
  takesNontransferable { [x = nt] in
    takesEscapingNontransferable { takesTransferable { x.use() } }
    // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'x'}}
  }
  takesNontransferable { [x = nt] in takesNontransferable { x.use() } } // Ok

  // Only the entry itself is diagnosed in the closure that owns it.
  takesTransferable { [x = nt] in takesNontransferable { x.use() } }
  // expected-error@-1 {{capture list entry 'x' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferable'}}
}

// A `defer` body runs in place with the isolation of its enclosing context, so
// its captures never cross an isolation boundary.
func testDeferCaptures(nt: NonTransferable) {
  defer { nt.use() } // Ok
  defer { takesNontransferable { nt.use() } } // Ok
  defer { takesTransferable { nt.use() } }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  nt.use()
}

func asyncWork() async {}

func testAsyncDeferCaptures(nt: NonTransferable) async {
  defer { await asyncWork(); nt.use() } // Ok
  defer { takesTransferable { nt.use() } }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  nt.use()
}

actor DeferIsolation {
  func isolated(nt: NonTransferable) async {
    defer { await asyncWork(); nt.use() } // Ok
    nt.use()
  }
}

@MainActor func testMainActorAsyncDeferCaptures(nt: NonTransferable) async {
  defer { await asyncWork(); nt.use() } // Ok
  nt.use()
}

nonisolated(nonsending) func testCallerIsolatedAsyncDeferCaptures(nt: NonTransferable) async {
  defer { await asyncWork(); nt.use() } // Ok
  nt.use()
}

@concurrent func testConcurrentAsyncDeferCaptures(nt: NonTransferable) async {
  defer { await asyncWork(); nt.use() } // Ok
  nt.use()
}

func testAsyncLetAndTaskCaptures(nt: NonTransferable, ntc: NonTransferableClass) async {
  Task { ntc.use() }
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'ntc'}}
  Task { [ntc] in ntc.use() }
  // expected-error@-1 {{capture list entry 'ntc' of 'Transferable'-conforming closure has non-'Transferable' type 'NonTransferableClass'}}

  async let x: Void = nt.use()
  // expected-error@-1 {{'Transferable'-conforming closure cannot capture non-'Transferable' value 'nt'}}
  _ = await x
}
