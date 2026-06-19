// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -Xllvm -sil-regionbasedisolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency

// Swift-source coverage test for the
// `-sil-regionbasedisolation-emit-isolation-history` flag, which makes the
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
  let y = Box1(x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y = Box1(x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let z = Box2(y) // expected-note {{'z' is connected to 'y'}}
  await transferToMain(z) // expected-warning {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func chain_three_step(_ x: NS) async {
  let y = Box1(x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let z = Box2(y) // expected-note {{'z' is connected to 'y'}}
  let w = Box3(z) // expected-note {{'w' is connected to 'z'}}
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
  let y = combine(x, a) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y = await Box1(getCustomNS()) // expected-note {{'y' is connected to 'getCustomNS' which is accessible to global actor 'CustomActor'-isolated code}}
  // expected-warning @-1 {{non-Sendable 'NS'-typed result can not be returned from global actor 'CustomActor'-isolated global function 'getCustomNS()' to nonisolated context}}
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
  bag.slot = x // expected-note {{'bag' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note @-1 {{'bag.slot' is connected to 'bag'}}
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
  var y = NS() // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  if flag {
    y = x
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
    y = x
  } else {
    y = NS() // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
    y = x
  } else {
    y = NS() // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  var y = NS() // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  for _ in 0..<count {
    y = x
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
    let y = self.slot // expected-note {{value was merged into 'self'-isolated code region here}}
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
  let m = getMainNS() // expected-note {{'y' is connected to 'm' which is accessible to main actor-isolated code}}
  let y = Box1(m)
  await transferToCustom(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'y' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and main actor-isolated uses}}
}

////////////////////////////////////////////////////////////////////////////////
// `consuming` and `borrowing` parameter conventions on a copyable
// non-Sendable type — same chain note machinery, different ownership
// conventions on the parameter.
////////////////////////////////////////////////////////////////////////////////

func consuming_param(_ x: consuming NS) async { // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  let y = Box1(x)
  await transferToMain(y) // expected-warning {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

func borrowing_param(_ x: borrowing NS) async {
  let y = Box1(copy x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y = IndirectChain.leaf(x) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y = AddrOnlyBox(x) // expected-note {{value was merged into code in the current isolation context region here}}
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
  let unknown = x // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  // expected-note@+1{{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  var y = NS()
  for _ in 0..<count {
    if flag {
      y = x
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
  @Wrap var y: NS = x // expected-note {{'_y' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y = try? makeOrThrow(x) // expected-note {{'y' is connected to 'z'}}
  if let z = y { // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    // expected-note @-1 {{'z' is connected to 'y'}}
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
  b.contents.append(x) // expected-note {{'b' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y = wrap(makeShared(x)) // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
  // expected-note @-1 {{'makeShared' is connected to 'y'}}
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
  var d: [String: NS] = [:] // expected-note {{'_allocateUninitializedArray' is connected to 'd'}}
  d["k"] = x // expected-note {{'d' is connected to 'x' which is accessible to code in the current isolation context}}
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
  let y: NS? = x // expected-note {{'y' is connected to 'z'}}
  if let z = y { // expected-note {{'z' is connected to 'x' which is accessible to code in the current isolation context}}
    // expected-note @-1 {{'z' is connected to 'y'}}
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
  let r: Result<NS, Error> = .success(x) // expected-note {{'r' is connected to 'y'}}
  if case let .success(y) = r { // expected-note {{'y' is connected to 'x' which is accessible to code in the current isolation context}}
    // expected-note @-1 {{'y' is connected to 'r'}}
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
  let kp = \KPBag.field // expected-note {{'kp' is connected to 'bag'}}
  let y = bag[keyPath: kp] // expected-note {{'swift_readAtKeyPath' is connected to 'kp'}}
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
    // expected-note @-1 {{value was merged into code in the current isolation context region here}}
    // expected-note @-2 {{'mid' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
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
