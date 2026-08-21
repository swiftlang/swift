// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -sil-region-isolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency

// Swift-source coverage test for the
// `-sil-region-isolation-emit-isolation-history` flag, which makes the
// SendNonSendable pass emit additional notes describing the chain of merges
// that brought a sent value into an actor-isolated region. The originating
// note names the isolated source ("'y' is connected to 'x' which is
// accessible to <isolation>"); intermediate notes name each user-named local
// along the chain ("'z' is connected to 'y'").
//
// For exhaustive merge-source coverage at the SIL level see the companion
// test `transfernonsendable_isolationhistory.sil`.

@MainActor func transferToMain<T>(_ t: T) async {}

class NS {} // expected-note {{class 'NS' does not conform to the 'Sendable' protocol}}

struct Box1 { var ns: NS; init(_ ns: NS) { self.ns = ns } }
struct Box2 { var b: Box1; init(_ b: Box1) { self.b = b } }
struct Box3 { var b: Box2; init(_ b: Box2) { self.b = b } }

actor CustomActorInstance {}

@globalActor
struct CustomActor {
  static let shared = CustomActorInstance()
}

@CustomActor func getCustomNS() async -> NS { NS() }

////////////////////////////////////////////////////////////////////////////////
// Single-step chain — disconnected named local joins task-isolated region.
////////////////////////////////////////////////////////////////////////////////

func single_step_let(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func single_step_tuple(_ x: NS) async {
  let y = (x, 1) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func single_step_struct(_ x: NS) async {
  // expected-note@+1 {{'y' is connected to result of 'Box1.init(_:)'}}
  let y = Box1(x) // expected-note {{result of 'Box1.init(_:)' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Suppression — sending an actively-isolated value should NOT get a
// reachability note. `x` is task-isolated already; there is no chain.
////////////////////////////////////////////////////////////////////////////////

func suppress_direct(_ x: NS) async {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Multi-step chains — originating note plus an intermediate note per
// user-named local along the chain.
////////////////////////////////////////////////////////////////////////////////

func chain_two_step(_ x: NS) async {
  // expected-note@+1 {{result of 'Box1.init(_:)' is connected to 'x' which is accessible to code in the current isolation context}}
  let y = Box1(x) // expected-note {{'y' is connected to result of 'Box1.init(_:)'}}
  // expected-note@+1 {{'y' is connected to result of 'Box2.init(_:)'}}
  let z = Box2(y) // expected-note {{'z' is connected to result of 'Box2.init(_:)'}}
  await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func chain_three_step(_ x: NS) async {
  // expected-note@+1 {{result of 'Box1.init(_:)' is connected to 'x' which is accessible to code in the current isolation context}}
  let y = Box1(x) // expected-note {{'y' is connected to result of 'Box1.init(_:)'}}
  // expected-note@+1 {{'y' is connected to result of 'Box2.init(_:)'}}
  let z = Box2(y) // expected-note {{'z' is connected to result of 'Box2.init(_:)'}}
  // expected-note@+1 {{'z' is connected to result of 'Box3.init(_:)'}}
  let w = Box3(z) // expected-note {{'w' is connected to result of 'Box3.init(_:)'}}
  await transferToMain(w) // expected-warning {{sending 'w' risks causing data races}}
  // expected-note @-1 {{sending 'w' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Inline construction at the call site — no user-named intermediate, so we
// fall back to the location-only "value was merged into …" note.
////////////////////////////////////////////////////////////////////////////////

func inline_tuple(_ x: NS) async {
  await transferToMain((x, 1)) // expected-warning {{sending value of non-Sendable type '(NS, Int)' risks causing data races; this is an error in the Swift 6 language mode}}
  // expected-note @-1 {{value was merged into code in the current isolation context region here}}
  // expected-note @-2 {{sending value of non-Sendable type '(NS, Int)' to main actor-isolated global function 'transferToMain' risks causing races in between code in the current isolation context and main actor-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// Multiple isolated sources — both `x` and `a` are task-isolated parameters.
// The chain note picks one of them as the originating source and reports the
// shared task isolation.
////////////////////////////////////////////////////////////////////////////////

func combine(_ a: NS, _ b: NS) -> NS { a }

func multi_isolated_params(_ x: NS, _ a: NS) async {
  // y is in the same region as both x and a. The originating note picks one
  // task-isolated parameter (whichever the chain walk reaches first).
  // expected-note@+1 {{'y' is connected to result of 'combine()'}}
  let y = combine(x, a) // expected-note {{result of 'combine()' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Casts — `as`/`as?` on a non-Sendable still merges the casted result with
// the source's region.
////////////////////////////////////////////////////////////////////////////////

class NSDerived: NS {}

func cast_unconditional(_ x: NSDerived) async {
  let y = x as NS // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func cast_conditional(_ x: NS) async {
  if let y = x as? NSDerived { // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
    // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// Global-actor isolated source — originating note names the global actor's
// isolation kind ("global actor 'CustomActor'-isolated code").
////////////////////////////////////////////////////////////////////////////////

func global_actor_chain() async {
  let y = await Box1(getCustomNS()) // expected-note {{result of 'Box1.init(_:)' is connected to 'getCustomNS' which is accessible to global actor 'CustomActor'-isolated code}}
  // expected-note@-1 {{'y' is connected to result of 'Box1.init(_:)'}}
  // expected-warning @-2 {{non-Sendable 'NS'-typed result can not be returned from global actor 'CustomActor'-isolated global function 'getCustomNS()' to nonisolated context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending global actor 'CustomActor'-isolated 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and global actor 'CustomActor'-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// Class-field merge — assigning a task-isolated value into a class instance's
// field merges the class instance into the task-isolated region.
////////////////////////////////////////////////////////////////////////////////

final class NSContainer {
  var slot: NS?
}

func class_field_chain(_ x: NS) async {
  let bag = NSContainer()
  // expected-note@+1 {{'bag.slot' is connected to 'x'}}
  bag.slot = x // expected-note {{'bag' is connected to 'bag.slot'}}
  await transferToMain(bag) // expected-warning {{sending 'bag' risks causing data races}}
  // expected-note @-1 {{sending 'bag' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// `var` reassignment chain — `var y` is reassigned to a value that ties it
// into the task-isolated region.
////////////////////////////////////////////////////////////////////////////////

func var_reassign_chain(_ x: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Diamond — three variants exercising CFG joins where the merge happens on
// different sides of the diamond. The originating note must still locate the
// task-isolated source through the CFG join.
////////////////////////////////////////////////////////////////////////////////

func diamond_then_branch_only(_ x: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  } else {
    // No merge here — y stays disconnected along this path.
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_else_branch_only(_ x: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    // No merge here.
  } else {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_both_branches(_ x: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    y = x
  } else {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Diamond with mixed branches — one branch reassigns y to a fresh
// disconnected value, the other reassigns y to a task-isolated value. The
// chain walk's CFGHistoryJoin handling has to explore both branches; when
// the disconnected-only path doesn't surface an isolated source, we have to
// pop and try the other path.
////////////////////////////////////////////////////////////////////////////////

func diamond_mixed_then_isolated(_ x: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    y = NS() // disconnected — chain walk should not stop here
  } else {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_mixed_else_isolated(_ x: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  } else {
    y = NS()
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Forward-declared `let` diamond — `let y` is initialized in both branches
// of an if/else (so the binding is honored as a `let` despite the
// branch-time assignment). The chain note must still find its way through
// the join point.
////////////////////////////////////////////////////////////////////////////////

func diamond_let_then_isolated(_ x: NS, _ flag: Bool) async {
  let y: NS
  if flag {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  } else {
    y = NS()
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_let_else_isolated(_ x: NS, _ flag: Bool) async {
  let y: NS
  if flag {
    y = NS()
  } else {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_let_both_isolated(_ x: NS, _ flag: Bool) async {
  let y: NS
  if flag {
    y = x
  } else {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Loop — `var y` reassigned inside a loop body. The chain note must still
// resolve the originating merge through the loop's back-edge join.
////////////////////////////////////////////////////////////////////////////////

func loop_assign(_ x: NS, _ count: Int) async {
  var y = NS()
  for _ in 0..<count {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Actor-instance isolated source — `actor.field` is actor-instance isolated.
// Capturing it into a local then sending creates a chain rooted in the actor.
////////////////////////////////////////////////////////////////////////////////

actor MyActor {
  var slot: NS = NS()

  func makeChainAndSend() async {
    let y = self.slot
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// Global-actor MainActor source — chain rooted in a MainActor-isolated value.
////////////////////////////////////////////////////////////////////////////////

@MainActor func getMainNS() -> NS { NS() }
@CustomActor func transferToCustom<T>(_ t: T) async {}

@MainActor func main_actor_chain() async {
  let m = getMainNS() // expected-note {{'m' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
  // expected-note@+1 {{'m' is connected to result of 'Box1.init(_:)'}}
  let y = Box1(m) // expected-note {{'y' is connected to result of 'Box1.init(_:)'}}
  await transferToCustom(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'y' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// `consuming` and `borrowing` parameter conventions on a copyable
// non-Sendable type — same chain note machinery, different ownership
// conventions on the parameter.
////////////////////////////////////////////////////////////////////////////////

func consuming_param(_ x: consuming NS) async {
  // expected-note@+2{{'x' is connected to result of 'Box1.init(_:)'}}
  // expected-note@+1{{'y' is connected to result of 'Box1.init(_:)'}}
  let y = Box1(x)
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func borrowing_param(_ x: borrowing NS) async {
  // expected-note@+1 {{'y' is connected to result of 'Box1.init(_:)'}}
  let y = Box1(copy x) // expected-note {{result of 'Box1.init(_:)' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Explicit noncopyable type — `~Copyable` struct with a non-Sendable field.
// Tests that the chain note machinery handles `~Copyable` storage the same
// way as ordinary copyable values.
////////////////////////////////////////////////////////////////////////////////

struct NCBox: ~Copyable {
  var ref: NS
  init(_ ref: NS) { self.ref = ref }
}

@MainActor func transferNCToMain(_ t: consuming NCBox) async {}

func noncopyable_chain(_ x: NS) async {
  let y = NCBox(x) // expected-note {{value was merged into code in the current isolation context region here}}
  await transferNCToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferNCToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Indirect enum — `indirect case` payload is heap-allocated. Constructing
// the case still merges the payload's region with the enum value's.
////////////////////////////////////////////////////////////////////////////////

indirect enum IndirectChain {
  case leaf(NS)
  case node(IndirectChain, NS)
}

func indirect_enum_chain(_ x: NS) async {
  let y = IndirectChain.leaf(x) // expected-note {{value was merged into code in the current isolation context region here}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Address-only generic — value of generic type `T` is address-only at the SIL
// level. Region merges happen via address-level instructions
// (tuple_addr_constructor / store / etc.).
//
// Name inference can't recover a name for the generic-typed local along the
// chain (no `var_decl` SILValue to anchor a name to), so we fall back to the
// generic location-only note.
////////////////////////////////////////////////////////////////////////////////

struct AddrOnlyBox<T> {
  var content: T
  init(_ t: T) { self.content = t }
}

func addr_only_chain<T>(_ x: T) async {
  let y = AddrOnlyBox(x) // expected-note {{'y' is connected to 'x'}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Legitimate identifier literally named `unknown` — VariableNameInferrer's
// pre-fix sentinel for "could not recover a name" was the literal identifier
// `unknown`, which the chain walker filtered by string match. That collided
// with any user-named local actually called `unknown`, silently dropping
// the chain step. The fix moves to a `Flag::FailIfNoName` opt-in so the
// inferrer reports failure as `std::nullopt` instead of "unknown".
////////////////////////////////////////////////////////////////////////////////

func unknown_legit_name(_ x: NS) async {
  let unknown = x // expected-note {{value was merged into code in the current isolation context region here}}
  // expected-note@+1{{'y' is connected to result of 'Box1.init(_:)'}}
  let y = Box1(unknown)
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Loop with an inner diamond — exercises the chain walker's CFGHistoryJoin
// recursion AND its per-branch state reset. Pre-fix, the walker's
// pendingTargetMerge / isolatedFound flags lived outside the worklist loop,
// so a branch that didn't surface a SequenceBoundary leaked its hypothesis
// into the next branch and caused non-deterministic note placement
// across recompiles. The visited-set fix bounds the walk in the presence
// of a loop's CFG back-edge join. The note location must be deterministic.
////////////////////////////////////////////////////////////////////////////////

func loop_with_inner_diamond(_ x: NS, _ count: Int, _ flag: Bool) async {
  var y = NS()
  for _ in 0..<count {
    if flag {
      y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    } else {
      y = NS()
    }
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Deep chain (10 user-named locals via direct aliasing) — exercises
// iteration order of the walker's `tracked` set across the
// SmallSet/SmallSetVector inline cap. Pre-fix, llvm::SmallSet<Element, 8>
// silently switched from insertion-order (under 8 entries) to sorted order
// (past 8) at the cliff, breaking chain-step ordering for chains of 9+
// entries. The fix uses SmallSetVector for stable insertion-order
// iteration regardless of size.
////////////////////////////////////////////////////////////////////////////////

func deep_chain_10(_ x: NS) async {
  let a = x // expected-note {{'a' is connected to 'x' which is accessible to code in the current isolation context}}
  let b = a // expected-note {{'b' is connected to 'a'}}
  let c = b // expected-note {{'c' is connected to 'b'}}
  let d = c // expected-note {{'d' is connected to 'c'}}
  let e = d // expected-note {{'e' is connected to 'd'}}
  let f = e // expected-note {{'f' is connected to 'e'}}
  let g = f // expected-note {{'g' is connected to 'f'}}
  let h = g // expected-note {{'h' is connected to 'g'}}
  let i = h // expected-note {{'i' is connected to 'h'}}
  let j = i // expected-note {{'j' is connected to 'i'}}
  await transferToMain(j) // expected-warning {{sending 'j' risks causing data races}}
  // expected-note @-1 {{sending 'j' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// These tests document current (sometimes inadequate) behavior of the chain
// walker for patterns surfaced by an adversarial review. Each carries a comment
// explaining what the diagnostic SHOULD ideally say. If a future change
// improves the chain — e.g. the inner-shadow step is no longer dropped, or the
// property-wrapper backing storage stops leaking — these tests will fail and
// flag the improvement for an expectation update.
////////////////////////////////////////////////////////////////////////////////

// Same-name shadowing: SendNonSendable.cpp's chain-step degeneracy filter
// (`if (step.name == predecessor.name) continue;`) is meant to suppress
// "X is reachable from X" output that arises when the inferrer maps both
// ends of a link to the same identifier. It overshoots when the user
// legitimately has two distinct SIL values with the same source name in
// nested scopes — the inner shadow is silently dropped from the chain.
//
// Ideal output would emit BOTH steps, possibly disambiguated:
//   `let y = x`             — note: 'y' (outer) is reachable from 'x'
//   `let y = Box1(y).ns`    — note: 'y' (inner) is reachable from 'y' (outer)
// Current output emits only one step; the inner shadow is lost. The
// originating note anchors at the OUTER `let y = x`, not where the
// shadow is introduced.
func shadow_chain(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  do {
    let y = Box1(y).ns
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
    // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  }
}

// Property wrapper: the synthetic backing storage variable named `_y` for
// `@Wrap var y` flows through the chain walker's name inference and
// produces user-facing notes naming `_y`. The `_X` prefix is technically
// a valid Swift identifier so we cannot blanket-filter it without also
// dropping legitimate `_internal`-style names; a proper fix needs access
// to the VarDecl's isImplicit() flag. Pinning current behavior so any
// improvement is reviewed.
@propertyWrapper
struct Wrap {
  var wrappedValue: NS
  init(wrappedValue: NS) { self.wrappedValue = wrappedValue }
}

func wrap_chain(_ x: NS) async {
  @Wrap var y: NS = x // expected-note {{value was merged into code in the current isolation context region here}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{'y' is connected to '_y'}}
  // expected-note @-2 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// `try?` Optional unwrap: the chain emits an inverted intermediate note
// (`'y' is reachable from 'z'`) — observable wrong direction. The
// originating note also names the wrong end of the chain (says `'z' is
// reachable from 'x'` rather than `'y' is reachable from 'x'`). Likely
// caused by the Optional unwrap synthesizing multiple SIL temporaries
// that the chain-step ordering can't disambiguate.
//
// Ideal output would be just two notes —
//   note: 'y' is reachable from 'x' which is accessible to ...
//   note: 'z' is reachable from 'y'
//
// Current output is the inverted/duplicated mess pinned below: three
// notes, one of which inverts the chain direction.
func makeOrThrow(_ x: NS) throws -> NS { x }

func try_optional_chain(_ x: NS) async throws {
  let y = try? makeOrThrow(x)
  if let z = y { // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// Known-issue patterns where the chain-note diagnostic produces wrong,
// missing, or surprising output.
//
// Each function below has TWO sets of expectations:
//
//   • The active `expected-*` annotations (no leading comment) match
//     the CURRENT (incorrect or incomplete) compiler output, so the
//     test passes -verify today. A FIXME comment above each describes
//     what is wrong with the diagnostic.
//
//   • Lines starting with `// IDEAL diagnostic:` describe what the
//     chain walker SHOULD emit if the bug were fixed. They are written
//     in plain English (NOT `expected-` directive syntax — that would
//     trigger -verify) so they sit alongside the active expectations
//     as a record of the right behavior.
//
// To validate a future fix: swap each FIXME's active expectations for
// a `expected-*` rendering of the IDEAL block, and re-run -verify.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Computed property setter — `bag.slot = x` invokes a setter call. The
// chain walker should surface 'bag.slot' as an intermediate step (mirroring
// how a stored-property assignment does), giving the user two notes.
////////////////////////////////////////////////////////////////////////////////

final class CPC {
  private var _slot: NS = NS()
  var slot: NS {
    get { _slot }
    set { _slot = newValue }
  }
}

func computed_prop_chain(_ x: NS) async {
  let bag = CPC()
  // FIXME: the 'bag.slot' intermediate step is dropped — name inference
  // doesn't recover the field-access through the synthesized setter call.
  bag.slot = x // expected-note {{'bag' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(bag) // expected-warning {{sending 'bag' risks causing data races}}
  // expected-note @-1 {{sending 'bag' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — what the chain walker should emit:
  //   bag.slot = x:                   note: 'bag.slot' is reachable from 'bag'
  //   await transferToMain(bag):      warning: sending 'bag' risks causing data races
  //                                   note:    'bag' is reachable from 'x' which is accessible to code in the current isolation context
  //                                   note:    sending 'bag' to main actor-isolated global function 'transferToMain' ...
}

////////////////////////////////////////////////////////////////////////////////
// `async let` — `async let y = boxed(x)` sends x into the child task,
// and `let z = await y` brings the result back. The user-visible chain
// is x → y → z and should be surfaced regardless of the send/unsend
// dance under the hood.
////////////////////////////////////////////////////////////////////////////////

func boxedAL(_ x: NS) -> NS { x }

func asynclet_chain(_ x: NS) async {
  // FIXME: no chain note ties 'x' or 'y' to 'z' on the eventual send.
  // The async-let send/unsend tear the chain — the unsend doesn't push
  // history in a form the walker can traverse, so the eventual
  // `transferToMain(z)` falls back to the generic location-only note
  // about "local nonisolated code".
  async let y = boxedAL(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' into async let risks causing data races between nonisolated code and code in the current isolation context}}
  let z = await y
  await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
} // expected-note {{access can happen concurrently}}

  // IDEAL diagnostic — chain walker should emit:
  //   async let y = boxedAL(x):       warning: sending 'x' risks causing data races (kept)
  //                                   note:    sending 'x' into async let ... (kept)
  //                                   note:    'y' is reachable from 'x' which is accessible to code in the current isolation context
  //   let z = await y:                note:    'z' is reachable from 'y'
  //   await transferToMain(z):        warning: sending 'z' risks causing data races
  //                                   note:    sending 'z' to main actor-isolated global function 'transferToMain' ...
  //                                            (text changes from "local nonisolated code" to "code in the current isolation context")

////////////////////////////////////////////////////////////////////////////////
// Task closure capture — `Task { @MainActor in _ = y }` captures y into
// a main-actor-isolated closure. The user-visible reachability is x → y;
// the chain note should appear at the binding line.
////////////////////////////////////////////////////////////////////////////////

func task_capture_chain(_ x: NS) async {
  // FIXME: no chain note ties 'y' back to 'x'. The
  // closure-capture-into-Task path bypasses the chain walker because
  // the merge happens at a closure argument rather than a
  // `transferToMain`-style apply.
  // expected-note@+1{{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let y = x
  Task { @MainActor in _ = y } // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{'y' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against code in the current isolation context}}

  // IDEAL diagnostic — chain walker should additionally emit:
  //   let y = x:                      note: 'y' is reachable from 'x' which is accessible to code in the current isolation context
}

////////////////////////////////////////////////////////////////////////////////
// inout subscript / append — `b.contents.append(x)` mutates a struct
// field through an inout setter chain. The chain should mirror the
// stored-property case: 'b.contents' is reachable from 'b', then 'b'
// is reachable from 'x'.
////////////////////////////////////////////////////////////////////////////////

struct Bag {
  var contents: [NS] = []
}

func inout_subscript_chain(_ x: NS) async {
  var b = Bag()
  // FIXME: only the `'b' is reachable from 'x'` step is emitted. The
  // 'b.contents' step is dropped — same root cause as
  // computed_prop_chain: the inout-setter form of mutation isn't
  // credited as a chain step.
  b.contents.append(x) // expected-note {{'b' is connected to 'x'}}
  await transferToMain(b) // expected-warning {{sending 'b' risks causing data races}}
  // expected-note @-1 {{sending 'b' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — chain walker should additionally emit:
  //   b.contents.append(x):           note: 'b.contents' is reachable from 'b'
}

////////////////////////////////////////////////////////////////////////////////
// Actor field captured into Task.detached — the chain should surface
// 'local' as reachable from 'self.field' (the actor-instance source),
// matching how the existing `MyActor.makeChainAndSend` test surfaces
// `self.slot` for a direct `transferToMain` call.
////////////////////////////////////////////////////////////////////////////////

actor Holder {
  var field: NS = NS()
  func method() async {
    // FIXME: no chain note when the send happens via Task closure
    // capture. Same root cause as task_capture_chain.
    let local = field
    await Task.detached { @MainActor in _ = local }.value
    // expected-warning @-1 {{sending 'local' risks causing data races}}
    // expected-note @-2 {{'self'-isolated 'local' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later actor-isolated uses}}
    // expected-warning @-3 {{passing closure as a 'sending' parameter risks causing data races between main actor-isolated code and concurrent execution of the closure}}
    // expected-note @-4 {{closure captures 'local' which is accessible to main actor-isolated code}}

    // IDEAL diagnostic — chain walker should additionally emit:
    //   let local = field:            note: 'local' is reachable from 'self.field' which is accessible to 'self'-isolated code
  }
}

////////////////////////////////////////////////////////////////////////////////
// Free function call leaks the callee's identifier as a chain step. When
// the chain walker encounters a SIL function_ref for a top-level (free)
// function called inline as part of building a chain step's value, it
// resolves the function_ref through VariableNameInferrer and treats the
// function's basename as a chain "variable". Real user code routinely
// has nested free-function calls (`Box(make(x))`, `convert(decode(input))`,
// builder pipelines) — today's diagnostic claims a function name is
// "reachable from" a local, which a reader cannot act on.
//
// Class/struct initializers, static methods, and closure-typed locals
// are filtered correctly; the bug is specific to free-function applies.
////////////////////////////////////////////////////////////////////////////////

func makeShared(_ x: NS) -> NS { x }
func wrap(_ a: NS) -> NS { a }

func freefunc_call_leak(_ x: NS) async {
  // FIXME: 'makeShared' is the callee, not a value. The chain walker
  // surfaces it as if it were an intermediate local.
  let y = wrap(makeShared(x)) // expected-note {{'makeShared' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@-1 {{'y' is connected to result of 'wrap()'}}
  // expected-note @-2 {{'makeShared' is connected to result of 'wrap()'}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — chain walker should emit:
  //   let y = wrap(makeShared(x)):    note: 'y' is reachable from 'x' which is accessible to ...
  //   await transferToMain(y):        warning + the standard sending note
  // The 'makeShared' note must be filtered (callee, not a value).
}

////////////////////////////////////////////////////////////////////////////////
// Stdlib internal symbol leaks into chain notes for collection-literal
// initialization. `var d: [String: NS] = [:]` lowers through
// `_allocateUninitializedArray`, whose function_ref is surfaced as a
// chain step. Same root cause as freefunc_call_leak — a free function
// reference being treated as if it were a value-binding identifier —
// but here the identifier is a stdlib intrinsic the user can't even
// reference.
////////////////////////////////////////////////////////////////////////////////

func dict_literal_chain(_ x: NS) async {
  // FIXME: '_allocateUninitializedArray' is a stdlib intrinsic, not a
  // user-visible identifier. It should never appear in a chain note.
  var d: [String: NS] = [:]
  d["k"] = x // expected-note {{'d' is connected to 'x'}}
  await transferToMain(d) // expected-warning {{sending 'd' risks causing data races}}
  // expected-note @-1 {{sending 'd' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — chain walker should emit:
  //   d["k"] = x:                     note: 'd' is reachable from 'x' which is accessible to ...
  //   await transferToMain(d):        warning + standard sending note.
  // The '_allocateUninitializedArray' note must be filtered.
}

////////////////////////////////////////////////////////////////////////////////
// `if let` Optional unwrap emits a REVERSED intermediate note. The line
// `let y: NS? = x` produces a note `'y' is reachable from 'z'` where `z`
// has not been declared yet — the chain walker swaps source-and-
// destination on the prior line. Same recurring shape as the existing
// try_optional_chain pin.
////////////////////////////////////////////////////////////////////////////////

func iflet_reversed_chain(_ x: NS) async {
  // FIXME: this line is annotated with `'y' is reachable from 'z'`, but
  // z is declared on the NEXT line and the data flow is z ← y, not y ← z.
  let y: NS? = x
  if let z = y { // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  }

  // IDEAL diagnostic — chain walker should emit:
  //   let y: NS? = x:                 note: 'y' is reachable from 'x' which is accessible to ...
  //   if let z = y { ... }:           note: 'z' is reachable from 'y'
  //   await transferToMain(z):        warning + standard sending note.
}

////////////////////////////////////////////////////////////////////////////////
// `Result` `case let` pattern — same reversed-chain bug as
// iflet_reversed_chain. The `let r: Result<NS, Error> = .success(x)`
// line emits `'r' is reachable from 'y'` where y is bound on the next
// line.
////////////////////////////////////////////////////////////////////////////////

func result_case_let_reversed_chain(_ x: NS) async {
  // FIXME: `'r' is reachable from 'y'` is reversed — r is built from x
  // and exists before y is destructured out.
  let r: Result<NS, Error> = .success(x)
  if case let .success(y) = r { // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
    // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  }

  // IDEAL diagnostic — chain walker should emit:
  //   let r: ... = .success(x):       note: 'r' is reachable from 'x' which is accessible to ...
  //   if case let .success(y) = r:    note: 'y' is reachable from 'r'
  //   await transferToMain(y):        warning + standard sending note.
}

////////////////////////////////////////////////////////////////////////////////
// KeyPath access — `let kp = \Bag.field; let y = bag[keyPath: kp]`
// surfaces `kp` as a chain step linking `bag` and `y`. KeyPath literals
// have no value-flow relationship to instances; `kp` is a static type-
// level construct, not a local in the data-flow chain.
////////////////////////////////////////////////////////////////////////////////

class KPBag { var field: NS = NS() }

func keypath_chain(_ x: NS) async {
  let bag = KPBag()
  // FIXME: kp is a literal `\Bag.field` keypath; it has no value-flow
  // relationship to bag.
  bag.field = x // expected-note {{'bag' is connected to 'x' which is accessible to code in the current isolation context}}
  let kp = \KPBag.field
  // expected-note@+1 {{'y' is connected to 'swift_readAtKeyPath'}}
  let y = bag[keyPath: kp] // expected-note {{'swift_readAtKeyPath' is connected to 'bag'}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — chain walker should emit:
  //   bag.field = x:                  note: 'bag' is reachable from 'x' which is accessible to ...
  //   let kp = ...:                   no chain note (KeyPath literal is type-level)
  //   let y = bag[keyPath: kp]:       note: 'y' is reachable from 'bag'
  //   await transferToMain(y):        warning + standard sending note.
}

////////////////////////////////////////////////////////////////////////////////
// withTaskGroup — the OUTER closure-sending warning at `group.addTask`
// emits no chain note for outer-frame named locals, even though the
// equivalent Task.detached form does. A/B-observable in one compiler
// invocation: identical chain x → y → z, captured into either
// Task.detached or group.addTask, produces different chain output.
////////////////////////////////////////////////////////////////////////////////

func taskgroup_chain_dropped(_ x: NS) async {
  let y = x
  let z = y
  // FIXME: outer-frame chain notes ('y' reachable from 'x', 'z' reachable
  // from 'y') don't fire for the addTask closure-send warning; same shape
  // via Task.detached does emit them.
  await withTaskGroup(of: Void.self) { group in
    // expected-note@+1 {{value was merged into code in the current isolation context region here}}
    group.addTask { // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current isolation context and concurrent execution of the closure}}
      let w = z // expected-note {{closure captures 'z' which is accessible to code in the current isolation context}}
      // expected-note @-1 {{'w' is connected to 'z' which is accessible to code in the current isolation context}}
      await transferToMain(w) // expected-warning {{sending 'w' risks causing data races}}
      // expected-note @-1 {{sending 'w' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
    }
  }

  // IDEAL diagnostic — chain walker should additionally emit at the
  // closure-sending warning's chain:
  //   let y = x:                      note: 'y' is reachable from 'x' which is accessible to ...
  //   let z = y:                      note: 'z' is reachable from 'y'
  // matching what Task.detached produces for the identical shape.
}

////////////////////////////////////////////////////////////////////////////////
// withCheckedContinuation — the warning fires at `cont.resume(returning:)`
// rather than at the eventual `transferToMain(z)`, and the chain step
// uses anonymous "value was merged into …" wording rather than the
// standard `'NAME' is reachable from 'NAME'` form. The user has no
// signal at the actual racy call site.
////////////////////////////////////////////////////////////////////////////////

func continuation_chain(_ x: NS) async {
  let mid = x
  // FIXME: warning fires at the wrong site (resume call, not the eventual
  // transferToMain). Chain step uses anonymous "value" wording.
  let y: NS = await withCheckedContinuation { cont in
    cont.resume(returning: mid) // expected-warning {{sending 'mid' risks causing data races}}
    // expected-note @-1 {{'mid' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
  }
  let z = y
  await transferToMain(z)

  // IDEAL diagnostic — chain walker should:
  //   • Suppress the resume-site warning (the value escapes correctly via
  //     continuation), OR at minimum name the merged value:
  //         note: 'y' is reachable from 'mid' (not anonymous "value")
  //   • Warn at the actual racy site:
  //         await transferToMain(z):  warning: sending 'z' risks causing data races
  //                                   note: 'z' is reachable from 'y'
  //                                   note: 'y' is reachable from 'mid'
  //                                   note: 'mid' is reachable from 'x' which is accessible to ...
}

////////////////////////////////////////////////////////////////////////////////
// Per-argument SILLocation: verifies that when SILGen attaches per-argument
// locations to an apply (gated by isolation-history), the originating-merge
// note anchors at the AST argument's source position rather than at the
// apply's anchor (the call expression's start). Each variant exercises a
// different apply opcode so the per-arg loc array survives ApplyInst,
// TryApplyInst, and BeginApplyInst construction.
////////////////////////////////////////////////////////////////////////////////

// First-argument case: the offending value is in slot 0. Confirms that the
// per-arg loc array is not silently shifted by the indirect-result / error
// prefix when the apply has neither.
func per_arg_loc(_ x: NS) async {
  // expected-note@+1{{'y' is connected to result of 'combine()'}}
  let y = combine(
    x, // expected-note {{result of 'combine()' is connected to 'x' which is accessible to code in the current isolation context}}
    NS())
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Second-argument case: the offending value is in slot 1. Confirms the
// per-arg loc array is indexed by argument position, not just the first
// slot. A bug that always wrote the first slot would anchor the note at
// the `NS()` line above instead of the `x,` line below.
func per_arg_loc_second(_ x: NS) async {
  // expected-note@+1{{'y' is connected to result of 'combine()'}}
  let y = combine(
    NS(),
    x) // expected-note {{value was merged into code in the current isolation context region here}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Throwing call (try_apply) case: confirms that createTryApply forwards
// the argLocs array to the constructed TryApplyInst. The PartitionOp
// derived from this call site reads the per-argument loc; a regression in
// TryApply argLoc threading would surface here as the note anchoring at
// the call line instead of the argument line.
func combineThrows(_ a: NS, _ b: NS) throws -> NS { a }

func per_arg_loc_throws(_ x: NS) async throws {
  // expected-note@+1{{'y' is connected to result of 'combineThrows()'}}
  let y = try combineThrows(
    x, // expected-note {{result of 'combineThrows()' is connected to 'x' which is accessible to code in the current isolation context}}
    NS())
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Coroutine (begin_apply) case: a read accessor lowers to begin_apply.
// Reading `bag.slot` through the coroutine accessor and then folding the
// borrowed value into a region exercises createBeginApply's argLoc path.
final class BagWithRead {
  private var stored: NS = NS()
  var slot: NS {
    _read { yield stored }
  }
}

func per_arg_loc_coroutine(_ x: NS) async {
  let bag = BagWithRead()
  // Use combine() with a multi-line arg list so the per-arg loc for `x` is
  // on a different line than the call. The merge of `x` into `bag`'s
  // region happens inside combine; the SIL apply receives `x` at slot 0
  // and `bag.slot` (read coroutine) at slot 1.
  // FIXME: chain-walker emits a noisy 3-step chain here when the apply's
  // singleRegion records N-1 single-peer merges; the second note is
  // direction-reversed and 'bag'/'bag.slot' name the same region. Should
  // collapse to one chain step "y is connected to x" (revisit alongside
  // walker dedup / direction fix).
  let y = combine( // expected-note {{'y' is connected to result of 'combine()'}}
    bag.slot, // expected-note {{'bag.slot' is connected to result of 'combine()'}}
    x, // expected-note {{'bag.slot' is connected to 'x' which is accessible to code in the current isolation context}}
  )
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Additional control-flow coverage: switches, nested/ternary diamonds, while/
// repeat/nested loops, guard, do/catch, break, and multiple isolated sources.
////////////////////////////////////////////////////////////////////////////////

func switch_one_isolated(_ x: NS, _ n: Int) async {
  var y = NS()
  switch n {
  case 0:
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  case 1:
    y = NS()
  default:
    break
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func switch_two_isolated(_ x: NS, _ n: Int) async {
  var y = NS()
  switch n {
  case 0:
    y = x
  case 1:
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  default:
    y = NS()
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func nested_diamond(_ x: NS, _ a: Bool, _ b: Bool) async {
  var y = NS()
  if a {
    if b {
      y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    } else {
      y = NS()
    }
  } else {
    y = NS()
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func ternary_chain(_ x: NS, _ flag: Bool) async {
  let y = flag ? x : NS() // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func while_loop_chain(_ x: NS, _ flag: Bool) async {
  var y = NS()
  var i = 0
  while i < 10 {
    if flag {
      y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    }
    i += 1
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func repeat_while_chain(_ x: NS, _ flag: Bool) async {
  var y = NS()
  var i = 0
  repeat {
    if flag {
      y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    }
    i += 1
  } while i < 10
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func nested_loop_chain(_ x: NS, _ n: Int) async {
  var y = NS()
  for _ in 0..<n {
    for _ in 0..<n {
      y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    }
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Two distinct isolated sources, both branches isolate y. Neither branch resets
// the isolation, so the discriminating-element rule cannot prefer one; the walk
// falls back to first-explored (names 'a', anchored on the else arm).
func two_isolated_sources(_ a: NS, _ b: NS, _ flag: Bool) async {
  var y = NS()
  if flag {
    y = a
  } else {
    y = b // expected-note {{'y' is connected to 'a' which is accessible to code in the current isolation context}}
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func sequential_diamonds(_ x: NS, _ f1: Bool, _ f2: Bool) async {
  var y = NS()
  if f1 {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  } else {
    y = NS()
  }
  if f2 {
    _ = y
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func do_catch_chain(_ x: NS) async throws {
  var y = NS()
  do {
    y = try makeOrThrow(x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  } catch {
    y = NS()
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func break_in_loop_chain(_ x: NS, _ n: Int) async {
  var y = NS()
  for _ in 0..<n {
    y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    break
  }
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// KNOWN-BAD DIAGNOSTICS (characterization). These pin current behavior where the
// chain walker loses information: an internal/runtime name leaks into the chain,
// a projection collapses to its base, the note names something other than the
// value actually sent, or we fall back to a nameless generic note. They should
// be *fixed* later; each FIXME says what the note ought to say.
////////////////////////////////////////////////////////////////////////////////

// FIXME: The sent value is 'box.ns', but the chain note names the container
// 'box'. It should say 'box.ns' (or at least agree with the sending note).
func send_projection_of_isolated(_ x: NS) async {
  var box = Box1(NS())
  box.ns = x // expected-note {{'box' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(box.ns) // expected-warning {{sending 'box.ns' risks causing data races}}
  // expected-note@-1 {{'box.ns' is connected to 'box'}}
// expected-note @-2 {{sending 'box.ns' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// FIXME: The write is 'box.b.b.ns = x', but the note collapses the whole access
// path to the base 'box'. It loses which field flowed the isolated value in.
func deep_projection_store(_ x: NS) async {
  var box = Box3(Box2(Box1(NS())))
  box.b.b.ns = x // expected-note {{'box' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(box) // expected-warning {{sending 'box' risks causing data races}}
// expected-note @-1 {{sending 'box' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// FIXME: 'y' is a copy of the actor's stored property, yet we emit only the
// nameless generic "value was merged … here" note. It should name the chain,
// e.g. "'y' is connected to 'slot'".
actor SlotActor {
  var slot = NS()
  func probe() async {
    let y = slot
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
// expected-note @-1 {{sending 'self'-isolated 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }
}

// FIXME: The array-literal lowering leaks the compiler-internal
// '_allocateUninitializedArray' as a chain step. An internal runtime name
// should never appear in a user-facing note.
func array_append_leak(_ x: NS) async {
  var arr = [NS()]
  arr.append(x) // expected-note {{'arr' is connected to 'x'}}
  await transferToMain(arr) // expected-warning {{sending 'arr' risks causing data races}}
  // expected-note @-1 {{sending 'arr' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// SEND/ISOLATION COMBINATION MATRIX.
//
// The sections above vary control flow. This one holds it fixed and varies the
// two things that actually drive the note: *which* member of a region is sent,
// and *how/when* the isolated value entered that region. Cases are grouped by
// scenario rather than by correct-vs-wrong, so a group can mix both; each
// individually-wrong case carries its own FIXME.
//
// Vocabulary used below: `b` is the task-isolated value, `a` is the value we
// send, `c` is a carrier that `b` was merged into.
////////////////////////////////////////////////////////////////////////////////

struct KlassPair {
  var first: NS
  var second: NS
  init(_ f: NS, _ s: NS) { first = f; second = s }
}

// Merges the regions of both arguments without producing a value: the merge is
// a side effect of the argument list, and there is no destination to name.
func mergeFn(_ a: NS, _ b: NS) {}

////////////////////////////////////////////////////////////////////////////////
// Merge via discarded apply arguments. Every other test in this file merges via
// an assignment or an assigned apply result; here there is no dest value at all,
// so the only nameable thing is the argument operand.
////////////////////////////////////////////////////////////////////////////////

func argmerge_two(_ x: NS) async {
  let y = NS()
  mergeFn(x, y) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func argmerge_transitive(_ x: NS) async {
  let y = NS()
  let z = NS()
  mergeFn(x, y) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  mergeFn(z, y)
  await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Argument order. `PartitionOp::Merge` records dest=representative and
// src=operand, so which argument is the isolated one decides which value gets
// stamped on the history node. The note should not depend on that ordering.
////////////////////////////////////////////////////////////////////////////////

func argmerge_isolated_first(_ x: NS) async {
  let y = NS()
  mergeFn(x, y) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func argmerge_isolated_second(_ x: NS) async {
  let y = NS()
  mergeFn(y, x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Two distinct projections of one base. `k.first` and `k.second` are different
// *values* that collapse to the same *element*, so a chain through them has two
// steps that share one element -- the sharpest value-vs-element case.
////////////////////////////////////////////////////////////////////////////////

// FIXME: The isolated value enters through 'k.first' and leaves through
// 'k.second', but neither field is named and the chain is inverted: 'y' is
// reported as connected directly to 'x' (they never merged) and the 'k' step
// anchors on the `var k` declaration rather than either store. Ideal:
//   k.first = x    note: 'k.first' is connected to 'x' which is accessible to ...
//   k.second = y   note: 'y' is connected to 'k.second'
func two_projections_one_base(_ x: NS) async {
  let y = NS()
  var k = KlassPair(NS(), NS())
  k.first = x // expected-note {{'k' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'y' is connected to 'k'}}
  k.second = y
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// The chain enters `k` through `.second` and exits through `.first`.
// FIXME: As above, neither projection is named and the 'k' step anchors on the
// declaration instead of the store that actually connected it.
func projection_enter_exit(_ x: NS) async {
  let y = NS()
  let z = NS()
  var k = KlassPair(NS(), NS())
  // expected-note@+1{{'k' is connected to 'y'}}
  k.second = y
  mergeFn(x, y) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'k'}}
  mergeFn(z, k.first)
  await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Fixed setup, vary only which region member is sent. The region is
// {x isolated, k, y} in all four; only the argument to transferToMain changes.
//
// FIXME: The first three emit byte-identical chain notes even though a
// different value is sent each time. The chain should be anchored on the value
// actually being sent.
////////////////////////////////////////////////////////////////////////////////

func send_container(_ x: NS) async {
  let y = NS()
  var k = KlassPair(NS(), NS())
  k.first = x // expected-note {{'k' is connected to 'x' which is accessible to code in the current isolation context}}
  k.second = y
  await transferToMain(k) // expected-warning {{sending 'k' risks causing data races}}
  // expected-note @-1 {{sending 'k' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func send_sibling_local(_ x: NS) async {
  let y = NS()
  var k = KlassPair(NS(), NS())
  k.first = x // expected-note {{'k' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'y' is connected to 'k'}}
  k.second = y
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func send_projection_member(_ x: NS) async {
  let y = NS()
  var k = KlassPair(NS(), NS())
  k.first = x // expected-note {{'k' is connected to 'x' which is accessible to code in the current isolation context}}
  k.second = y
  await transferToMain(k.second) // expected-warning {{sending 'k.second' risks causing data races}}
  // expected-note@-1 {{'k.second' is connected to 'k'}}
  // expected-note @-2 {{sending 'k.second' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Sending the isolated value itself out of a *populated* region. Correctly
// suppressed: 'x' is not disconnected, so there is no chain to explain.
// (`suppress_direct` above covers the same rule with a singleton region.)
func send_isolated_from_populated_region(_ x: NS) async {
  let y = NS()
  var k = KlassPair(NS(), NS())
  k.first = x
  k.second = y
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Bystander send: the sent value `a` was never party to the merge that brought
// the isolated `b` into the region. `b` was merged into a carrier `c`; `a`
// joined `c` by a separate merge. Crossed with the order of those two merges,
// which reaches the same final partition by different histories:
//   - isolation_after:  a joined c while the region was still disconnected
//   - isolation_before: a joined a region that was already isolated
//
// FIXME (whole group): two defects, visible in every case below.
//  1. The originating note claims "'a' is connected to 'b'" -- a connection
//     that never existed in the history. 'a' and 'b' were never merged; the
//     note should route through the carrier ('a' -> 'c' -> 'b').
//  2. Merge order is not distinguished at all: the ..._after and ..._before
//     variants produce byte-identical notes, as do all three threehop
//     variants. The walk is reporting a closure over "elements ever connected"
//     rather than the merge that actually introduced the isolation, so the
//     originating location is order-independent when it should not be.
////////////////////////////////////////////////////////////////////////////////

func bystander_argmerge_isolation_after(_ b: NS) async {
  let a = NS()
  let c = NS()
  // expected-note@+1{{'a' is connected to 'c'}}
  mergeFn(a, c)
  mergeFn(c, b) // expected-note {{'c' is connected to 'b' which is accessible to code in the current isolation context}}
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func bystander_argmerge_isolation_before(_ b: NS) async {
  let a = NS()
  let c = NS()
  mergeFn(c, b) // expected-note {{'c' is connected to 'b' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'a' is connected to 'c'}}
  mergeFn(a, c)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func bystander_projection_isolation_after(_ b: NS) async {
  let a = NS()
  var c = KlassPair(NS(), NS())
  // expected-note@+1{{'a' is connected to 'c'}}
  c.first = a
  c.second = b // expected-note {{'c' is connected to 'b' which is accessible to code in the current isolation context}}
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func bystander_projection_isolation_before(_ b: NS) async {
  let a = NS()
  var c = KlassPair(NS(), NS())
  c.second = b // expected-note {{'c' is connected to 'b' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'a' is connected to 'c'}}
  c.first = a
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Three hops: a -- c -- d, with b merged into d. The isolated value is now two
// hops away from the sent value. Vary where in the sequence isolation attaches.
func bystander_threehop_isolation_last(_ b: NS) async {
  let a = NS()
  let c = NS()
  let d = NS()
  // expected-note@+1{{'a' is connected to 'c'}}
  mergeFn(a, c)
  // expected-note@+1{{'c' is connected to 'd'}}
  mergeFn(c, d)
  mergeFn(d, b) // expected-note {{'d' is connected to 'b' which is accessible to code in the current isolation context}}
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func bystander_threehop_isolation_first(_ b: NS) async {
  let a = NS()
  let c = NS()
  let d = NS()
  mergeFn(d, b) // expected-note {{'d' is connected to 'b' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'c' is connected to 'd'}}
  mergeFn(c, d)
  // expected-note@+1{{'a' is connected to 'c'}}
  mergeFn(a, c)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func bystander_threehop_isolation_middle(_ b: NS) async {
  let a = NS()
  let c = NS()
  let d = NS()
  // expected-note@+1{{'c' is connected to 'd'}}
  mergeFn(c, d)
  mergeFn(d, b) // expected-note {{'d' is connected to 'b' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'a' is connected to 'c'}}
  mergeFn(a, c)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// OVERWRITE / SEVERED CHAINS.
//
// Negative tests: the history still *contains* a merge with the isolated value,
// but the connection was later severed, so nothing should be reported. These
// are the cases that catch a chain walk which closes over "elements ever
// connected" instead of consulting region membership at each step -- there is
// no diagnostic to match, so -verify fails if any note appears.
////////////////////////////////////////////////////////////////////////////////

func overwrite_severs(_ b: NS) async {
  var c = NS()
  mergeFn(c, b)
  c = NS()
  let a = NS()
  mergeFn(a, c)
  await transferToMain(a)
}

func carrier_reassigned_between(_ b: NS) async {
  let a = NS()
  var c = NS()
  mergeFn(a, c)
  c = NS()
  mergeFn(c, b)
  await transferToMain(a)
}

// Contrast with the two above: overwriting a *field* does not sever, because the
// field collapses to its base's element, so the isolated value stays in the
// region. This pairing pins whole-variable-overwrite vs field-overwrite.
func field_overwritten_twice(_ x: NS) async {
  let y = NS()
  var k = KlassPair(NS(), NS())
  k.first = x // expected-note {{'k' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'y' is connected to 'k'}}
  k.first = y
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// FURTHER CROSSINGS — bystander x CFG join, inout operand, unnameable source.
////////////////////////////////////////////////////////////////////////////////

func mergeInout(_ a: inout NS, _ b: NS) {}

func bystander_diamond(_ b: NS, _ flag: Bool) async {
  let a = NS()
  let c = NS()
  mergeFn(a, c)
  if flag {
    mergeFn(c, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  }
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func inout_merge(_ b: NS) async {
  var a = NS()
  mergeInout(&a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

actor BystanderProbe {
  var slot = NS()
  func run() async {
    let a = NS()
    let c = NS()
    // expected-note@+1{{'a' is connected to 'c'}}
    mergeFn(a, c)
    // expected-note@+1{{'c' is connected to 'self.slot' which is accessible to 'self'-isolated code}}
    mergeFn(c, slot)
    await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// MERGE VIA CLOSURE CAPTURE.
//
// A closure that captures two non-Sendable values merges their regions, so the
// closure itself acts as a carrier -- the captured values are never merged with
// each other directly. Distinct from the Task/addTask cases above, where the
// closure is *sent*; here the closure is merely formed and called locally.
//
// FIXME (whole group): three defects visible below.
//  1. The closure local ('f', 'g') is surfaced as a chain step, but the user
//     never wrote a connection between the closure and its captures -- the
//     closure is the implementation of the capture, not a link the reader can
//     act on. Worst case is 'f' is connected to 'g', a claim about two closures
//     that means nothing at the source level.
//  2. As in the other bystander groups, the originating note asserts
//     "'a' is connected to 'b'" though 'a' and 'b' only ever met through the
//     closure's capture list.
//  3. Notes pile onto the closure-literal line rather than distributing across
//     the captures that caused each merge.
////////////////////////////////////////////////////////////////////////////////

func useClosure(_ f: () -> ()) {}

func closure_captures_both(_ b: NS) async {
  let a = NS()
  let f = { mergeFn(a, b) } // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  useClosure(f)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func closure_carrier_bystander(_ b: NS) async {
  let a = NS()
  let c = NS()
  // expected-note@+1{{'a' is connected to 'c'}}
  let f = { mergeFn(a, c) }
  let g = { mergeFn(c, b) } // expected-note {{'c' is connected to 'b' which is accessible to code in the current isolation context}}
  useClosure(f)
  useClosure(g)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func closure_carrier_bystander_reversed(_ b: NS) async {
  let a = NS()
  let c = NS()
  let g = { mergeFn(c, b) } // expected-note {{'c' is connected to 'b' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'a' is connected to 'c'}}
  let f = { mergeFn(a, c) }
  useClosure(g)
  useClosure(f)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func closure_call_merge(_ b: NS) async {
  let a = NS()
  let f = { (n: NS) in mergeFn(n, b) }
  f(a)
  await transferToMain(a)
}

func send_closure_capturing_isolated(_ b: NS) async {
  let f = { mergeFn(b, b) } // expected-note {{'f' is connected to 'b' which is accessible to code in the current isolation context}}
  await transferToMain(f) // expected-warning {{sending 'f' risks causing data races}}
  // expected-note @-1 {{sending 'f' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func closure_captures_var_byref(_ b: NS) async {
  var a = NS()
  let f = { a = b } // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  useClosure(f)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func closure_escaping_stored(_ b: NS) async {
  let a = NS()
  var stored: (() -> ())? = nil
  stored = { mergeFn(a, b) } // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  _ = stored
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func closure_nested(_ b: NS) async {
  let a = NS()
  let f = { { mergeFn(a, b) }() } // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  useClosure(f)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// WHAT MAKES THE CLOSURE ISOLATED.
//
// `closure_captures_both` above is the task-isolated-by-capture case: the
// closure picks up isolation from a captured task-isolated value. These vary
// that source -- an explicit global actor on the closure, and actor isolation
// picked up by capturing actor state.
//
// NOTE: the explicitly-isolated closures below take a different diagnostic path
// (the closure-capture error, not the sending error), and emit no chain notes at
// all. They are here to pin that boundary.
////////////////////////////////////////////////////////////////////////////////

func useEscaping(_ f: @escaping () -> ()) {}

func closure_global_actor_explicit() async {
  let a = NS()
  let f = { @MainActor in _ = a } // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{'a' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  useEscaping(f) // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  await transferToMain(a) // expected-note {{access can happen concurrently}}
}

func closure_custom_global_actor_explicit() async {
  let a = NS()
  let f = { @CustomActor in _ = a } // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{'a' is captured by a global actor 'CustomActor'-isolated closure. global actor 'CustomActor'-isolated uses in closure may race against later nonisolated uses}}
  useEscaping(f) // expected-warning {{converting function value of type '@CustomActor () -> ()' to '() -> ()' loses global actor 'CustomActor'}}
  await transferToMain(a) // expected-note {{access can happen concurrently}}
}

actor StateHolder {
  var slot = NS()

  // Closure becomes actor-isolated by capturing actor-isolated state.
  func viaCapturedState() async {
    let a = NS()
    let f = { mergeFn(a, self.slot) } // expected-warning {{sending 'a' risks causing data races}}
    // expected-note @-1 {{'a' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later actor-isolated uses}}
    useClosure(f)
    await transferToMain(a) // expected-note {{access can happen concurrently}}
  }

  // Closure becomes actor-isolated by capturing self alongside the bystander.
  func viaCapturedSelf() async {
    let a = NS()
    let f = { mergeFn(a, self.slot); _ = self } // expected-warning {{sending 'a' risks causing data races}}
    // expected-note @-1 {{'a' is captured by a actor-isolated closure. actor-isolated uses in closure may race against later actor-isolated uses}}
    useClosure(f)
    await transferToMain(a) // expected-note {{access can happen concurrently}}
  }
}

////////////////////////////////////////////////////////////////////////////////
// SEND DESTINATION ISOLATION.
//
// The sections above vary where the isolation came *from*. This one varies where
// the value is sent *to*, which selects the descriptive-kind string in both the
// sending note and the originating chain note. Every other test in this file
// sends to the same @MainActor global function.
////////////////////////////////////////////////////////////////////////////////

@CustomActor func toCustomFunc<T>(_ t: T) async {}

struct MainMethodStruct { @MainActor func method<T>(_ t: T) async {} }
final class MainMethodClass { @MainActor func method<T>(_ t: T) async {} }
@MainActor struct IsolatedStruct { func method<T>(_ t: T) async {} }
@MainActor final class IsolatedClass { func method<T>(_ t: T) async {} }
actor Receiver { func take<T>(_ t: T) async {} }

func dest_custom_global_actor_func(_ b: NS) async {
  let a = NS()
  mergeFn(a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await toCustomFunc(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to global actor 'CustomActor'-isolated global function 'toCustomFunc' risks causing data races between global actor 'CustomActor'-isolated code and code in the current isolation context}}
}

func dest_global_actor_method_on_struct(_ b: NS, _ s: MainMethodStruct) async {
  let a = NS()
  mergeFn(a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await s.method(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated instance method 'method' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Two sends from one call: the bystander 'a' and the nonisolated receiver 'c'.
func dest_global_actor_method_on_class(_ b: NS, _ c: MainMethodClass) async {
  let a = NS()
  mergeFn(a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await c.method(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated instance method 'method' risks causing data races between main actor-isolated code and code in the current isolation context}}
  // expected-warning @-2 {{sending 'c' risks causing data races}}
  // expected-note @-3 {{sending 'c' to main actor-isolated instance method 'method' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func dest_method_on_isolated_struct(_ b: NS, _ s: IsolatedStruct) async {
  let a = NS()
  mergeFn(a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await s.method(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated instance method 'method' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func dest_method_on_isolated_class(_ b: NS, _ c: IsolatedClass) async {
  let a = NS()
  mergeFn(a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await c.method(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated instance method 'method' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func dest_actor_method(_ b: NS, _ r: Receiver) async {
  let a = NS()
  mergeFn(a, b) // expected-note {{'a' is connected to 'b' which is accessible to code in the current isolation context}}
  await r.take(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to actor-isolated instance method 'take' risks causing data races between actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// GLOBAL-ACTOR SOURCE REACHED BY MERGING.
//
// The right way to get a global-actor-isolated *source*: be inside the global
// actor's context so the isolated value can be obtained legitimately, merge a
// disconnected local into it, then send to a different isolation domain. (Doing
// this from a nonisolated context instead fails earlier -- a non-Sendable result
// cannot leave the actor -- so the value never becomes a usable source.)
//
// This also covers @concurrent as a send destination, which no other test uses.
////////////////////////////////////////////////////////////////////////////////

@concurrent func toConcurrent<T>(_ t: T) async {}

@MainActor func ga_merge_send_other_actor() async {
  let a = NS()
  // expected-note@+1{{'m' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
  let m = getMainNS()
  mergeFn(a, m) // expected-note {{'a' is connected to 'm'}}
  await transferToCustom(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'a' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}

@MainActor func ga_merge_send_concurrent() async {
  let a = NS()
  // expected-note@+1{{'m' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
  let m = getMainNS()
  mergeFn(a, m) // expected-note {{'a' is connected to 'm'}}
  await toConcurrent(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'a' to @concurrent global function 'toConcurrent' risks causing data races between @concurrent and main actor-isolated uses}}
}

// FIXME: The bystander form of the two above emits NO chain note at all -- not
// even a wrong one. 'a' joined carrier 'c', and 'c' is what merged with the
// main-actor-isolated 'm', so the chain should read a -> c -> m. Contrast with
// the task-isolated bystander cases above, which at least emit a (wrong) note;
// with a global-actor source the explanation is dropped entirely.
@MainActor func ga_bystander_send_concurrent() async {
  let a = NS()
  let c = NS()
  // expected-note@+1{{'a' is connected to 'c'}}
  mergeFn(a, c)
  // expected-note@+1{{'m' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
  let m = getMainNS()
  // expected-note@+1{{'c' is connected to 'm'}}
  mergeFn(c, m)
  await toConcurrent(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'a' to @concurrent global function 'toConcurrent' risks causing data races between @concurrent and main actor-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// ALREADY-SENT REGION.
//
// From inside a global-actor-isolated function, pass a disconnected local to a
// *different* global actor's function -- which sends its whole region -- then
// pass a sibling from that same region somewhere else. This is a use-after-send,
// a different diagnostic path that the isolation-history walker does not
// participate in. Pinned so the boundary is explicit.
////////////////////////////////////////////////////////////////////////////////

@CustomActor func sent_region_then_sibling_concurrent() async {
  let a = NS()
  let b = NS()
  mergeFn(a, b)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local global actor 'CustomActor'-isolated uses}}
  await toConcurrent(b) // expected-note {{access can happen concurrently}}
}

@CustomActor func sent_region_then_sibling_other_actor() async {
  let a = NS()
  let b = NS()
  mergeFn(a, b)
  await transferToMain(a) // expected-warning {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local global actor 'CustomActor'-isolated uses}}
  await transferToMain(b) // expected-note {{access can happen concurrently}}
}

////////////////////////////////////////////////////////////////////////////////
// SENT-NEVER-SENDABLE SUB-DIAGNOSTICS OTHER THAN AN ISOLATION CROSSING.
//
// Everything above reaches the SentNeverSendable emitter through one exit
// path: an apply whose isolation crossing gives the value a name
// (`emitNamedIsolation`). That emitter has several other exits, and each one
// pairs a *different* primary diagnostic with the same history notes:
//
//   • a 'sending' parameter          -> emitNamedSendingNeverSendableToSendingParam
//   • a 'sending' closure literal    -> initForSendingPartialApply
//   • a 'sending' result             -> emitNamedSendingReturn
//   • the typed (unnamed) fallbacks  -> emitTypedSendingNeverSendableToSendingParam
//                                       emitPassToApply
//
// The notes are emitted from a SWIFT_DEFER in SentNeverSendableDiagnosticEmitter
// ::emit(), so they attach regardless of which exit ran. These tests pin that:
// they hold the history-note wording fixed while the primary diagnostic varies.
////////////////////////////////////////////////////////////////////////////////

func takeSending(_ x: sending NS) {}
func takeSendingGeneric<T>(_ x: sending T) {}
func takeSendingClosure(_ f: sending () -> ()) {}

////////////////////////////////////////////////////////////////////////////////
// 'sending' parameter destination. The primary note says "is passed as a
// 'sending' parameter" instead of naming a callee isolation, but the chain
// leading up to it is the ordinary one.
////////////////////////////////////////////////////////////////////////////////

func sending_param_chain(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let z = y // expected-note {{'z' is connected to 'y'}}
  takeSendingGeneric(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{'z' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
  _ = x
}

////////////////////////////////////////////////////////////////////////////////
// 'sending' closure literal. Here the primary diagnostic anchors on the
// *capture* inside the closure while the history notes anchor on the captured
// locals' declarations — two independent anchor sources in one diagnostic.
////////////////////////////////////////////////////////////////////////////////

func sending_closure_literal_chain(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let z = y // expected-note {{'z' is connected to 'y'}}
  takeSendingClosure { _ = z } // expected-warning {{passing closure as a 'sending' parameter risks causing data races between code in the current isolation context and concurrent execution of the closure}}
  // expected-note @-1 {{closure captures 'z' which is accessible to code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// 'sending' result. The send is a `return`, not an apply, so this exits
// emit() through the ReturnInst branch rather than the ApplyExpr branch.
// Covered in sync, async, and throwing form since that branch asserts every
// SILResultInfo carries IsSending.
////////////////////////////////////////////////////////////////////////////////

func sending_result_chain(_ x: NS) -> sending NS {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  let z = y
  return z // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{'z' cannot be a 'sending' result. Code in the current task may race with caller uses}}
}

func sending_result_async_chain(_ x: NS) async -> sending NS {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  let z = y
  return z // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{'z' cannot be a 'sending' result. Code in the current task may race with caller uses}}
}

func sending_result_throws_chain(_ x: NS) throws -> sending NS {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  let z = y
  return z // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{'z' cannot be a 'sending' result. Code in the current task may race with caller uses}}
}

////////////////////////////////////////////////////////////////////////////////
// CONCRETE VS GENERIC CALLEE PARAMETER.
//
// Whether a chain link survives depends on the *callee's* parameter type, not
// on anything the user wrote at the send site. A generic parameter forces a
// copy into a temporary, which shows up in the history as an extra merge and
// gives the walk an intermediate to report; a concrete parameter merges the
// sent element directly with the isolated source, so the walk decides the
// chain is trivial and suppresses a step.
//
// The pairs below are identical Swift apart from the callee they send to.
////////////////////////////////////////////////////////////////////////////////

@MainActor func transferToMainConcrete(_ t: NS) async {}

// Generic callee, one hop: the note fires.
func concrete_vs_generic_generic_one_hop(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// FIXME: concrete callee, one hop: NO history note at all, though the source
// is identical to the generic case above apart from the callee. The walk
// rewinds `let y = x` as a merge of the sent element straight into 'x''s
// region, never sets intermediateSeen, and suppresses the chain as trivial —
// the suppression that exists for `transferToMain(x)`, where the user really
// did send the isolated value itself. Here the user did write an intermediate
// ('y'), so the note should fire.
func concrete_vs_generic_concrete_one_hop(_ x: NS) async {
  // expected-note@+1{{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let y = x
  await transferToMainConcrete(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMainConcrete' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — chain walker should emit:
  //   let y = x:                          note: 'y' is connected to 'x' which is accessible to code in the current isolation context
  //   await transferToMainConcrete(y):    warning: sending 'y' risks causing data races
  //                                       note:    sending 'y' to main actor-isolated global function 'transferToMainConcrete' ...
}

// Generic callee, two hops: both links are reported.
func concrete_vs_generic_generic_two_hop(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let z = y // expected-note {{'z' is connected to 'y'}}
  await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// FIXME: concrete callee, two hops: the innermost link is dropped. 'y' is
// connected to 'x' is reported, but 'z' is connected to 'y' is not, so the
// chain silently starts one step away from the value actually named in the
// warning. Same root cause as the one-hop case: the last merge before the
// send is folded away when the parameter is concrete.
func concrete_vs_generic_concrete_two_hop(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  let z = y
  await transferToMainConcrete(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMainConcrete' risks causing data races between main actor-isolated code and code in the current isolation context}}

  // IDEAL diagnostic — chain walker should additionally emit:
  //   let z = y:                          note: 'z' is connected to 'y'
}

// The same asymmetry on the 'sending' parameter path, to show it is a property
// of the callee's parameter type rather than of the isolation-crossing exit.
// FIXME: 'z' is connected to 'y' is dropped; compare sending_param_chain
// above, which is the same code against a generic 'sending' parameter.
func sending_param_concrete_chain(_ x: NS) async {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  let z = y
  takeSending(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{'z' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
  _ = x

  // IDEAL diagnostic — chain walker should additionally emit:
  //   let z = y:                          note: 'z' is connected to 'y'
}

// FIXME: the 'sending' result path folds the last link away too — 'z' is
// connected to 'y' is missing from every sending_result_* test above. Pinned
// once here rather than repeated on each.
func sending_result_dropped_link(_ x: NS) -> sending NS {
  let y = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note@+1{{'z' is connected to 'y'}}
  let z = y
  return z // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{'z' cannot be a 'sending' result. Code in the current task may race with caller uses}}

  // IDEAL diagnostic — chain walker should additionally emit:
  //   let z = y:                          note: 'z' is connected to 'y'
}

////////////////////////////////////////////////////////////////////////////////
// MERGE TOPOLOGY x ORDERING MATRIX.
//
// Each group below fixes a merge topology -- which values get merged, ignoring
// time -- and varies the order the merges happen in. The topology fixes how many
// notes are correct; the ordering must not change that count. A group whose
// count is not uniform is reporting a closure over "values ever connected"
// rather than the merges the program actually performed.
//
// One note per merge the user wrote, anchored at that merge. A merge between two
// spellings of a single value (the stack temporary SILGen creates for a call
// argument) is an artifact of lowering and gets no note.
//
// The hard case is a merge that separates the two values being connected while
// naming NEITHER of them -- both ride along as passengers on opposite sides of
// the split. See the passenger_split group: in a balanced merge tree over N
// carriers, only the two merges adjacent to the endpoints name an endpoint, so
// recognising a merge by its operands misses almost all of them.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Topology: direct. y -- x. 1 edge, 1 ordering.
// Expect 1 note: the terminal one. There is no intermediate to report, but the
// sent value is still disconnected and had to be told where its isolation came
// from. (Contrast suppress_direct above, where the SENT value is itself the
// isolated one and nothing is emitted.)
////////////////////////////////////////////////////////////////////////////////

func direct(_ x: NS) async {
    let y = NS()
    mergeFn(y, x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: path, 1 carrier. y -- z -- x. 2 edges, 2 orderings.
// Expect 2 notes each: 1 link + 1 terminal.
//
//   L = mergeFn(y, z)    I = mergeFn(z, x)
////////////////////////////////////////////////////////////////////////////////

func path1_link_isolate(_ x: NS) async {
    let y = NS()
    let z = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func path1_isolate_link(_ x: NS) async {
    let y = NS()
    let z = NS()

    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}

    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: path, 2 carriers. y -- w -- z -- x. 3 edges, all 3! = 6 orderings.
// Expect 3 notes each: 2 links + 1 terminal.
//
//   L = mergeFn(y, w)    C = mergeFn(z, w)    I = mergeFn(z, x)
//
// This is the section that matters most: same shape six ways. v1 is known to
// drop a hop on at least one ordering (see bystander_threehop_isolation_last
// in transfernonsendable_isolationhistory.swift, which emits 2 where its
// siblings emit 3), so a uniform count here is not a given.
////////////////////////////////////////////////////////////////////////////////

func path2_link_chain_isolate(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()

    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}

    mergeFn(z, w) // expected-note {{'w' is connected to 'z'}}

    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}

    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func path2_link_isolate_chain(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(z, w) // expected-note {{'w' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func path2_chain_link_isolate(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, w) // expected-note {{'w' is connected to 'z'}}
    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func path2_chain_isolate_link(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, w) // expected-note {{'w' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func path2_isolate_link_chain(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}
    mergeFn(z, w) // expected-note {{'w' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func path2_isolate_chain_link(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(z, w) // expected-note {{'w' is connected to 'z'}}
    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: fan -- INTENDED as one carrier reaching two separate isolated
// values, y -- z, z -- x1, z -- x2. It is not actually that shape.
//
// ANSWERED: one terminal note, and the reason is that the second merge does not
// exist. All non-Sendable function arguments are put in ONE region at entry
// (RegionAnalysis.cpp: initialEntryBlockPartition = Partition::singleRegion of
// the joined argument indices), so x1 and x2 are already region-mates before
// the body runs. By the time `mergeFn(z, x2)` executes, z and x2 are in the same
// region, and Partition::merge early-returns without pushing a history node.
// The log confirms it: the boundaries for that line are present with no
// MergeElementRegions between them, and the oldest node in the history is
// `into region of %%0 merged [%%1]` -- the two parameters being joined at entry.
//
// So there is only one isolated region here, reached one way, and one terminal
// note is right. This section does NOT exercise multiple isolated sources.
//
// The same applies to every group below that takes more than one NS parameter
// (fanchain, fandepth, fanthree, splitdiamond, unevendiamond,
// passenger_split_two_isolated, mixed_direct): the parameters are one region
// from entry, so the second and later "isolate" merges push no history node and
// the intended fan collapses to a path. Those groups are still useful as path
// orderings, but they do NOT exercise multiple isolated sources. Two genuinely
// independent isolated sources need two isolation DOMAINS, not two parameters.
////////////////////////////////////////////////////////////////////////////////

func fan_link_isolate_isolate(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(z, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fan_isolate_isolate_link(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(z, x2)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fan_isolate_link_isolate(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: fan over a 2-carrier chain, both isolated values at the far end.
// y -- z -- w, then w -- x1 and w -- x2.
// 3 disconnected (y, z, w), 2 isolated (x1, x2).
//
// The chain to the isolated pair is unambiguous -- y, z, w in that order -- so
// the 2 link notes are not in question. What is in question is what happens
// when the walk arrives at w and finds TWO isolated values there.
//
// This is the shape that stresses "split out all of the isolated elements
// first": there are two to split out, and whichever is split first is the one
// the focus meets first. If that decides which terminal note is emitted, the
// answer depends on split order rather than on the program.
////////////////////////////////////////////////////////////////////////////////

func fanchain_links_then_isolates(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(w, x1) // expected-note {{'w' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fanchain_isolates_then_links(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(w, x1) // expected-note {{'w' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fanchain_interleaved(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(w, x1) // expected-note {{'w' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(w, x2)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: two isolated values attaching at DIFFERENT depths along the chain.
// y -- z -- w, with z -- x1 (one hop from y) and w -- x2 (two hops).
// 3 disconnected, 2 isolated.
//
// Unlike the section above, the two isolated values are not interchangeable:
// x1 is reached after 1 link and x2 after 2. A focus that walks one route to
// completion reports whichever it meets first and never learns about the
// other. If only one chain is reported, is the nearer isolated value the right
// one to report, or the farther?
////////////////////////////////////////////////////////////////////////////////

func fandepth_links_then_isolates(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w)
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fandepth_isolates_then_links(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(z, w)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fandepth_near_isolate_last(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(w, x2) // expected-note {{'w' is connected to 'x2' which is accessible to code in the current isolation context}}
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(z, x1)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: three isolated values over a 3-carrier chain.
// y -- z -- w -- v, with z -- x1, w -- x2, v -- x3.
// 4 disconnected, 3 isolated, one attaching at each depth.
//
// Pushes past two: if the answer for two isolated values is "report them all",
// this says whether the structure generalises or whether two was a special
// case that a pair of fields happened to cover.
////////////////////////////////////////////////////////////////////////////////

func fanthree_links_then_isolates(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w)
    mergeFn(w, v)
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(v, x3)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func fanthree_isolates_then_links(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(v, x3)
    mergeFn(w, v)
    mergeFn(z, w)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Orderings of the fanthree topology. Same 6 edges every time, 6! = 720
// orderings; these are the ones that isolate a distinct structural property.
//
//   L  = mergeFn(y, z)     link: sent value attaches to the chain
//   C1 = mergeFn(z, w)     chain, near
//   C2 = mergeFn(w, v)     chain, far
//   I1 = mergeFn(z, x1)    isolate at depth 1
//   I2 = mergeFn(w, x2)    isolate at depth 2
//   I3 = mergeFn(v, x3)    isolate at depth 3
//
// The two extremes already exist above: fanthree_isolates_then_links is
// I1 I2 I3 C2 C1 L, and fanthree_links_then_isolates is L C1 C2 I1 I2 I3.
//
// The counts noted per case are derived from the rewind mechanism -- history
// pops newest-first, a merge is only recognised when one operand is already
// tracked, and tracking only grows by passing merges. They are PREDICTIONS,
// not observations: nothing calls the v2 emitter yet. They are worth writing
// down anyway because the spread is the point -- if the design is right, the
// count should be the same across this whole section, and the mechanism says
// it currently ranges from 0 to 6.
////////////////////////////////////////////////////////////////////////////////

// I1 I2 I3 L C1 C2 -- isolates still oldest, but the links run near-to-far
// instead of far-to-near. Rewind meets C2 and C1 before anything is tracked,
// so only z is ever reached: x2 and x3 drop out even though every isolating
// merge is older than every link. Link ORDER alone decides how far the chain
// is walked.
func fanthree_links_near_to_far(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(v, x3)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w)
    mergeFn(w, v)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// I3 C2 I2 C1 I1 L -- each isolate sits immediately before the link that
// reaches its attachment point. Rewind order is L I1 C1 I2 C2 I3, so every
// element is tracked exactly when its merge arrives. Full recognition, and the
// upper bound for this topology.
func fanthree_interleaved_far_first(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(v, x3) // expected-note {{'v' is connected to 'x3' which is accessible to code in the current isolation context}}
    mergeFn(w, v) // expected-note {{'w' is connected to 'v'}}
    mergeFn(w, x2)
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(z, x1)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// L I1 C1 I2 C2 I3 -- the mirror image: each isolate immediately AFTER its
// link. Rewind meets every isolate before the link that would have tracked its
// attachment point, so nothing is recognised at all. Same interleaving, other
// direction, opposite outcome.
func fanthree_interleaved_near_first(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(z, w)
    mergeFn(w, x2)
    mergeFn(w, v)
    mergeFn(v, x3)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// I1 I3 C2 C1 L I2 -- the MIDDLE isolate is newest, the rest in the
// best-case order. Isolates exactly one isolated value out of the result while
// leaving the chain walk otherwise intact.
func fanthree_middle_isolate_newest(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(v, x3)
    mergeFn(w, v)
    mergeFn(z, w)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(w, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// I2 I3 C2 C1 L I1 -- same, but the NEAREST isolate is the newest one. The
// dropped value is the one closest to the sent value, which is arguably the
// most relevant one to report.
func fanthree_near_isolate_newest(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(w, x2) // expected-note {{'w' is connected to 'x2' which is accessible to code in the current isolation context}}
    mergeFn(v, x3)
    mergeFn(w, v)
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x1)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// I1 I2 C2 C1 L I3 -- and the FARTHEST isolate newest. Completes the trio:
// with the other five merges fixed, moving each isolate to newest in turn
// should drop exactly that one value and nothing else.
func fanthree_far_isolate_newest(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(w, v)
    mergeFn(z, w)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(v, x3)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// I1 I2 I3 C2 L C1 -- the link sits between the two chain merges. Rewind hits
// C1 before y has reached z, so the near chain edge is missed and the walk
// stops one hop in. Tests that the link's position matters independently of
// the isolates'.
func fanthree_link_between_chains(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(v, x3)
    mergeFn(w, v)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// L I1 I2 I3 C2 C1 -- the sent value attaches FIRST, before anything else
// exists. Every other merge is newer, so rewind reaches all of them while
// tracking is still just {y}. Relevant to the "stop once the sentElement is an
// operand" rule: here that merge is the oldest, so the stop fires last rather
// than first.
func fanthree_link_oldest(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(v, x3)
    mergeFn(w, v)
    mergeFn(z, w)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// C1 C2 I1 I2 I3 L -- the whole carrier chain is built before any isolation
// arrives, and the sent value attaches last. Rewind gets L first, so the stop
// rule fires immediately; the chain edges are the oldest merges of all and are
// only reached after every isolate has been undone.
func fanthree_chains_before_isolates(_ x1: NS, _ x2: NS, _ x3: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(z, w)
    mergeFn(w, v)
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(v, x3)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: one isolated value merged DIRECTLY into the sent value, a second
// reached through a chain. y -- x1 direct, plus y -- z -- w -- x2.
// 3 disconnected, 2 isolated.
//
// This is the case that stresses "stop once we find a merge that has the
// sentElement as one of the operands". mergeFn(y, x1) is exactly such a merge,
// and it is also the direct case that is normally SUPPRESSED as adding no
// information. But y also reaches x2 through a real chain that does deserve
// notes. So the stopping rule and the suppression rule both fire on a merge
// that is not the whole story.
//
// Varying whether the direct merge is newest or oldest changes whether the
// walk meets it before or after it has explored the chain.
////////////////////////////////////////////////////////////////////////////////

func mixed_direct_newest(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w) // expected-note {{'z' is connected to 'w'}}
    mergeFn(w, x2) // expected-note {{'w' is connected to 'x2' which is accessible to code in the current isolation context}}
    mergeFn(y, x1) // no merge
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func mixed_direct_newest2(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w) // (x1 x2) (y z w)
    mergeFn(z, x2) // expected-note {{'z' is connected to 'x2' which is accessible to code in the current isolation context}}
    mergeFn(y, x1) // no merge
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func passenger_split(_ x1: NS) async {
    let y1 = NS()
    let y2 = NS()
    let y3 = NS()
    let y4 = NS()
    mergeFn(y3, y4) // expected-note {{'y3' is connected to 'y4'}}
    mergeFn(y1, y2) // expected-note {{'y1' is connected to 'y2'}}
    mergeFn(y2, y3) // expected-note {{'y2' is connected to 'y3'}}

    mergeFn(y4, x1) // expected-note {{'y4' is connected to 'x1' which is accessible to code in the current isolation context}}
    await transferToMain(y1) // expected-warning {{sending 'y1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Passenger splits, stressed. A passenger split is a merge that separates the
// two values we are trying to connect while naming NEITHER of them -- both are
// passengers, on opposite sides of the split. passenger_split above has one.
// These have several, so a rule that happens to survive a single one does not
// survive here.
//
// The shape is: build the carriers up in pairs, then join the pairs, then join
// the pairs of pairs. Every join separates the endpoints beneath it while naming
// neither. A balanced merge tree over 2^k leaves has 2^k - 1 internal merges, of
// which only the two adjacent to the endpoints are ever named by an operand.
//
// Why this is the stress case: recognition by "is an operand tracked" gets one
// of these merges wrong and then never recovers, because the endpoints it would
// need to track are exactly the ones it failed to learn. Recognition by "did
// this merge put my endpoints in different regions" is unaffected by how many
// there are, since it never consults operands at all.
//
// Note the discovery order is not the chain order. In passenger_split the
// chain is y1-y2-y3-y4-x1 but the merges are found 4th, 2nd, 1st, 3rd. The more
// splits there are the worse the permutation gets, so links have to be stitched
// by endpoint after the fact rather than chained as they are discovered.
////////////////////////////////////////////////////////////////////////////////

// Balanced tree over 8 carriers: 4 pairs, 2 pairs-of-pairs, 1 root. The sent
// value is the leftmost leaf and the isolated value attaches to the rightmost,
// so the two endpoints are maximally far apart in the tree. Every one of the
// 3 upper-level joins names neither endpoint.
func passenger_split_balanced8(_ x1: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()
    let a7 = NS()
    let a8 = NS()

    // Level 1: four disjoint pairs. Neither endpoint is a passenger yet.
    mergeFn(a1, a2) // expected-note {{'a1' is connected to 'a2'}}
    mergeFn(a3, a4) // expected-note {{'a3' is connected to 'a4'}}
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}
    mergeFn(a7, a8) // expected-note {{'a7' is connected to 'a8'}}

    // Level 2: join the pairs. mergeFn(a2, a3) already separates (a1, a8) while
    // naming neither of them.
    mergeFn(a2, a3) // expected-note {{'a2' is connected to 'a3'}}
    mergeFn(a6, a7) // expected-note {{'a6' is connected to 'a7'}}

    // Level 3: the root. Also separates (a1, a8) without naming either.
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}

    // Isolation enters at the far end.
    mergeFn(a8, x1) // expected-note {{'a8' is connected to 'x1' which is accessible to code in the current isolation context}}
    await transferToMain(a1) // expected-warning {{sending 'a1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Same tree, but the isolating merge comes FIRST. Now the whole tree is built
// under an already-isolated region, so at every split both sides are
// isolated. Distinguishes "did this split my endpoints" from anything that
// keys off isolation status of the operands.
func passenger_split_balanced8_isolate_first(_ x1: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()
    let a7 = NS()
    let a8 = NS()

    mergeFn(a8, x1) // expected-note {{'a8' is connected to 'x1' which is accessible to code in the current isolation context}}

    mergeFn(a1, a2) // expected-note {{'a1' is connected to 'a2'}}
    mergeFn(a3, a4) // expected-note {{'a3' is connected to 'a4'}}
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}
    mergeFn(a7, a8) // expected-note {{'a7' is connected to 'a8'}}
    mergeFn(a2, a3) // expected-note {{'a2' is connected to 'a3'}}
    mergeFn(a6, a7) // expected-note {{'a6' is connected to 'a7'}}
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}
    await transferToMain(a1) // expected-warning {{sending 'a1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Splits where the endpoints are in the MIDDLE of the tree rather than at the
// edges. The sent value is a4 and isolation enters at a5, so the single root
// mergeFn(a4, a5) names both endpoints -- but every merge under it separates
// some sub-pair without naming it. Tests that those merges below the answer do
// not generate spurious links once the answer is already found.
func passenger_split_endpoints_adjacent(_ x1: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()
    let a7 = NS()
    let a8 = NS()

    mergeFn(a1, a2)
    mergeFn(a3, a4)
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}
    mergeFn(a7, a8) // expected-note {{'a7' is connected to 'a8'}}
    mergeFn(a2, a3)
    mergeFn(a6, a7) // expected-note {{'a6' is connected to 'a7'}}
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}

    mergeFn(a8, x1) // expected-note {{'a8' is connected to 'x1' which is accessible to code in the current isolation context}}
    await transferToMain(a4) // expected-warning {{sending 'a4' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a4' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// A deliberately unbalanced tree: a long left spine that is repeatedly joined
// by single elements from the right. Each spine join separates the endpoints
// without naming them, and they arrive in a strictly nested order rather than a
// tree-shaped one.
func passenger_split_spine(_ x1: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()

    mergeFn(a1, a2) // expected-note {{'a1' is connected to 'a2'}}
    mergeFn(a2, a3) // expected-note {{'a2' is connected to 'a3'}}
    mergeFn(a3, a4) // expected-note {{'a3' is connected to 'a4'}}
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}

    mergeFn(a6, x1) // expected-note {{'a6' is connected to 'x1' which is accessible to code in the current isolation context}}
    await transferToMain(a1) // expected-warning {{sending 'a1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// The spine built from the far end inward, so the merges arrive in the reverse
// order relative to the chain. Pairs with passenger_split_spine: same
// topology, opposite build direction.
func passenger_split_spine_reversed(_ x1: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()

    mergeFn(a6, x1) // expected-note {{'a6' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}
    mergeFn(a3, a4) // expected-note {{'a3' is connected to 'a4'}}
    mergeFn(a2, a3) // expected-note {{'a2' is connected to 'a3'}}
    mergeFn(a1, a2) // expected-note {{'a1' is connected to 'a2'}}
    await transferToMain(a1) // expected-warning {{sending 'a1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Passenger splits plus a second isolated value, so the walk has two endpoints
// to reach through the same thicket of splits. x1 attaches at one end and x2 in
// the middle of the tree.
func passenger_split_two_isolated(_ x1: NS, _ x2: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()
    let a7 = NS()
    let a8 = NS()

    mergeFn(a1, a2) // expected-note {{'a1' is connected to 'a2'}}
    mergeFn(a3, a4) // expected-note {{'a3' is connected to 'a4'}}
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}
    mergeFn(a7, a8) // expected-note {{'a7' is connected to 'a8'}}
    mergeFn(a2, a3) // expected-note {{'a2' is connected to 'a3'}}
    mergeFn(a6, a7) // expected-note {{'a6' is connected to 'a7'}}
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}

    mergeFn(a8, x1) // expected-note {{'a8' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(a5, x2)
    await transferToMain(a1) // expected-warning {{sending 'a1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Passenger splits with a dead-end branch hanging off the middle. d is merged
// into the tree but is on no route from a1 to the isolated value, so nothing
// about it should be reported -- the over-reporting check, in the presence of
// passenger splits.
func passenger_split_with_deadend(_ x1: NS) async {
    let a1 = NS()
    let a2 = NS()
    let a3 = NS()
    let a4 = NS()
    let a5 = NS()
    let a6 = NS()
    let d = NS()

    mergeFn(a1, a2) // expected-note {{'a1' is connected to 'a2'}}
    mergeFn(a3, a4) // expected-note {{'a3' is connected to 'a4'}}
    mergeFn(a5, a6) // expected-note {{'a5' is connected to 'a6'}}
    mergeFn(a2, a3) // expected-note {{'a2' is connected to 'a3'}}
    mergeFn(a4, a5) // expected-note {{'a4' is connected to 'a5'}}

    mergeFn(a3, d)

    mergeFn(a6, x1) // expected-note {{'a6' is connected to 'x1' which is accessible to code in the current isolation context}}
    await transferToMain(a1) // expected-warning {{sending 'a1' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'a1' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func mixed_direct_oldest(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, x1) // expected-note {{'y' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(y, z) // (z y x1 x2) (w) // Then we do the same thing here.
    mergeFn(z, w) // (z w y x1 x2 w) // See that w splits out, is disconnected, and y is still in isolated region. So just recurse into y's region.
    mergeFn(w, x2) // No merge. Already in region.
    await transferToMain(y) // Error. // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func mixed_direct_middle(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z)
    mergeFn(y, x1) // expected-note {{'y' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(z, w)
    mergeFn(w, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: diamond. Two disjoint carrier paths to the SAME isolated value.
// y -- z -- x and y -- w -- x. 4 edges.
//
// DECIDED: report the single chain path. There is one originating value, so a
// second route adds no information the user needs -- it just spells out that
// the region could have been reached another way. Whichever route is reported
// must not depend on merge ordering.
////////////////////////////////////////////////////////////////////////////////

func diamond_left_first(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(y, w)
    mergeFn(w, x)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_interleaved(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(y, w)
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(w, x)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func diamond_isolate_first(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(w, x)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(y, w)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: split diamond. Two disjoint routes ending at DIFFERENT isolated
// values. y -- z -- x1 and y -- w -- x2. 3 disconnected, 2 isolated.
//
// The diamond above has one answer reachable two ways, so collapsing to a
// single chain loses only a route. Here the two routes end somewhere
// different, so collapsing loses an isolated value outright -- the user is
// told about one actor when two are involved.
//
// Also the cleanest test of route independence: the two halves share only y,
// so nothing about z should affect what is reported for w.
////////////////////////////////////////////////////////////////////////////////

func splitdiamond_left_route_first(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(y, w)
    mergeFn(w, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func splitdiamond_links_then_isolates(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(y, w)
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func splitdiamond_isolates_then_links(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(w, x2)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(y, w)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: diamond with unequal route lengths, two isolated values.
// Short route y -- z -- x1. Long route y -- w -- v -- x2.
// 4 disconnected, 2 isolated.
//
// Both routes must be walked to their own ends, and they end at different
// depths. A walk that keeps a single "current focus" has to abandon one route
// to follow the other; where it abandons is visible in which notes survive.
////////////////////////////////////////////////////////////////////////////////

func unevendiamond_short_first(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(y, w)
    mergeFn(w, v)
    mergeFn(v, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func unevendiamond_long_first(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(y, w) // expected-note {{'y' is connected to 'w'}}
    mergeFn(w, v) // expected-note {{'w' is connected to 'v'}}
    mergeFn(v, x2) // expected-note {{'v' is connected to 'x2' which is accessible to code in the current isolation context}}
    mergeFn(y, z)
    mergeFn(z, x1)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func unevendiamond_interleaved(_ x1: NS, _ x2: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    let v = NS()
    mergeFn(y, w)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(w, v)
    mergeFn(z, x1) // expected-note {{'z' is connected to 'x1' which is accessible to code in the current isolation context}}
    mergeFn(v, x2)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Two isolated values from DIFFERENT isolation domains, rather than two
// task-isolated parameters.
//
// Every multi-isolated case above uses parameters, which are all isolated the
// same way, so a region holding two of them is coherent. Here one value is
// main-actor isolated and the other belongs to an actor instance, which is a
// different domain.
//
// Not obviously a note-counting question: merging across domains is what
// IncompatibleRegionMergeError is for, so this may well diagnose before any
// isolation-history note is reached. Worth knowing which fires, since if the
// merge is rejected then a region can never hold two domains and the
// multi-isolated cases above are all single-domain by construction.
////////////////////////////////////////////////////////////////////////////////

actor SomeActor {
    var field = NS()
    func getField() -> NS { field }
}

@MainActor func crossdomain_two_isolated(_ a: SomeActor) async {
    let y = NS()
    let z = NS()
    // expected-note@+1{{'m' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
    let m = getMainNS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, m) // expected-note {{'z' is connected to 'm'}}
    await toConcurrent(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending main actor-isolated 'y' to @concurrent global function 'toConcurrent' risks causing data races between @concurrent and main actor-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: dead-end branch. A value joins the sent value's region but is not
// on the route to isolation. y -- d (never isolated), plus y -- z -- x.
//
// Expect 2 notes: the path1 answer. Nothing about 'd'. This is the only
// section that tests OVER-reporting -- every case above can only fail by
// emitting too few.
////////////////////////////////////////////////////////////////////////////////

func deadend_before(_ x: NS) async {
    let y = NS()
    let z = NS()
    let d = NS()
    mergeFn(y, d)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func deadend_between(_ x: NS) async {
    let y = NS()
    let z = NS()
    let d = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(y, d)
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func deadend_after(_ x: NS) async {
    let y = NS()
    let z = NS()
    let d = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(y, d)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Topology: cycle. A redundant edge closes a loop among the carriers.
// y -- z, z -- w, w -- y, then z -- x.
//
// Expect the path answer for whichever route is reported; the redundant edge
// must not multiply notes or make the walk revisit an element.
////////////////////////////////////////////////////////////////////////////////

func cycle_close_before_isolate(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w)
    mergeFn(w, y)
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func cycle_close_after_isolate(_ x: NS) async {
    let y = NS()
    let z = NS()
    let w = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, w)
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    mergeFn(w, y)
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Degenerate merges. Expect these to change nothing.
////////////////////////////////////////////////////////////////////////////////

// Self merge: exercises the lhs == rhs branch.
func degenerate_self_merge(_ x: NS) async {
    let y = NS()
    let z = NS()
    mergeFn(y, y)
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Re-merging two values already in one region. Partition::merge early-returns
// before pushing history when the regions are equal, so no node should exist
// and no extra note should appear.
func degenerate_redundant_merge(_ x: NS) async {
    let y = NS()
    let z = NS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(y, z)
    mergeFn(z, x) // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    await transferToMain(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

////////////////////////////////////////////////////////////////////////////////
// Survivor modifier. Same topology and ordering as path1, but the isolated
// value is a local declared AFTER the sent value instead of a parameter.
//
// Region labels are region minima, so declaration order picks which side of a
// merge survives the split -- and only the extracted side's passengers are
// recorded in the node. Flipping the survivor can make the sent value an
// unrecorded passenger, absent from the node entirely rather than merely
// unread. Compare against path1_link_isolate / path1_isolate_link.
//
// Whether these actually flip depends on the element numbering the pass
// assigns, which is worth reading off
// -Xllvm -sil-regionbasedisolation-log-isolation-history rather than
// predicting.
////////////////////////////////////////////////////////////////////////////////

@MainActor func survivor_isolated_declared_last_link_isolate() async {
    let y = NS()
    let z = NS()
    // expected-note@+1{{'x' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
    let x = getMainNS()
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    mergeFn(z, x) // expected-note {{'z' is connected to 'x'}}
    await toConcurrent(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending main actor-isolated 'y' to @concurrent global function 'toConcurrent' risks causing data races between @concurrent and main actor-isolated uses}}
}

@MainActor func survivor_isolated_declared_last_isolate_link() async {
    let y = NS()
    let z = NS()
    // expected-note@+1{{'x' is connected to result of 'getMainNS()' which is accessible to main actor-isolated code}}
    let x = getMainNS()
    mergeFn(z, x) // expected-note {{'z' is connected to 'x'}}
    mergeFn(y, z) // expected-note {{'y' is connected to 'z'}}
    await toConcurrent(y) // expected-warning {{sending 'y' risks causing data races; this is an error in the Swift 6 language mode}} expected-note {{sending main actor-isolated 'y' to @concurrent global function 'toConcurrent' risks causing data races between @concurrent and main actor-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// FIXME: A chain whose isolated source is an actor's stored property read
// straight into a local produces NO history note at all, so this diagnostic
// says a value is isolated without ever saying why -- on one of the most common
// ways a value gets into an isolated region. The case below pins that so it
// cannot change silently; when it is fixed, it should produce, on the
// 'let z = ns' line:
//
//   'z' is connected to 'self.ns' which is accessible to 'self'-isolated code
//
// Two heuristics collide. Naming a call result after its callee deliberately
// exempts accessors, since the user wrote a property rather than a call
// (VariableNameUtils.cpp: "An accessor's name is its storage's name"), so the
// getter's result is named after the variable it initializes -- 'z'. The walk's
// isSameUserValue then sees both ends of the merge named 'z' and drops it as an
// artifact of lowering, which is what that rule exists for: real SILGen
// temporaries. Here it discards a semantically distinct value, and since this
// was the only link the whole chain empties.
//
// Substituting anything that is not an accessor read restores the note -- see
// 'chain_actor_via_method' below, which is the same shape through a method call
// and does get its chain. That asymmetry is the bug's signature. Likely fix:
// under Flag::NameCallResultAfterCallee, name an accessor result after its
// storage rather than after the local, which is already what a read with no
// local binding prints.
////////////////////////////////////////////////////////////////////////////////

actor ActorWithStoredNS {
  var ns = NS()
  func getNS() -> NS { NS() }

  // FIXME: Should also produce the note quoted above, on the 'let z = ns' line.
  func chain_actor_via_property() async {
    let z = ns
    await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }

  // The control for the FIXME above: identical shape, but the isolated source is
  // a method result rather than an accessor result, so the chain is reported.
  func chain_actor_via_method() async {
    // expected-note@+1 {{'z' is connected to result of 'getNS()' which is accessible to 'self'-isolated code}}
    let z = getNS()
    await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }
}
