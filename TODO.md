# TODO: typed throws in `Task.init` / `Task.detached` / `Task.immediate`

Drop this file before opening a PR.

## Background

`Task.init` and friends adopted `throws(Failure)` twice and both times it was
reverted:

- #84802 was reverted by #86583: the non-AEIC `Task.value` getter
  (`$sScT5valuexvg`) changed its error convention, so errors were lost when
  running against an older runtime. #87103 fixed this with the `_abi_value`
  shim that is still in `stdlib/public/Concurrency/Task.swift`.
- #87103 (+ #87822, removal of the `Failure == Never` overloads) was reverted
  by #88172: source breaks of the form

  ```swift
  @MainActor class C {
    var task: Task<Void, any Error>?
    func start() { task = Task {} }
    // error: sending value of non-Sendable type '() async -> ()' risks
    // causing data races
  }
  ```

  Root cause is SILGen, not Sema: a non-throwing closure literal converted to
  an opaque `throws(Failure)` position (with `Failure` bound to `any Error`)
  was emitted at its own type plus a reabstraction thunk, and region isolation
  rejects the thunk's captured closure. The `Failure == Never` overloads were
  not the cause; the typed shape breaks with or without them.

## PR 1: SILGen / type lowering (this branch)

Change: `canEmitSpecializedClosureFunction` (`lib/SILGen/SILGenExpr.cpp`)
no longer bails for non-throwing literals under an opaque thrown error, and
`TypeConverter::makeConstantInterfaceType` (`lib/SIL/IR/TypeLowering.cpp`)
lowers such a closure with the destination's thrown error, so its
`@error_indirect` slot matches the prolog.

- [ ] Compile and link a frontend with BOTH changed TUs. The SILGen-only
      version failed the SIL verifier ("indirect error result type mismatch",
      closure lowered as `@error_indirect τ for <Never>` while the prolog set
      up `$*any Error`); the `TypeLowering.cpp` half is meant to fix that and
      has not been compiled yet.
- [ ] Mock repro (no stdlib change needed): a `Task`-shaped type with
      `init(@_inheritActorContext @_implicitSelfCapture operation: sending
      @escaping @isolated(any) () async throws(Failure) -> Success)`, assigned
      into `Task<Void, any Error>?` from a `@MainActor` class and returned
      from an actor method. Expect no diagnostics and no
      `[reabstraction_thunk]` in `-emit-silgen`.
- [ ] Fix up the new tests against real output (CHECK lines, mangled closure
      names, and the missing `expected-error`s for the positive control
      `stillDiagnosesRealRaces`):
  - `test/SILGen/typed_throws_conversion.swift` (both RUN lines, including
    `-enable-sil-opaque-values`)
  - `test/Concurrency/sending_closure_typed_throws_opaque_error.swift`
    (3 RUN lines; the `-default-isolation MainActor` line may need its own
    expectations)
  - `test/Interpreter/typed_throws_nonthrowing_closure_opaque_error.swift`
- [ ] Regression sweep: `test/SILGen`, `test/SIL`, `test/IRGen`,
      `test/Concurrency`, `test/Interpreter/*typed_throws*`,
      `test/embedded` (typed throws are common there), wasm typed-throws tests.
- [ ] Check the remaining edges:
  - untyped `throws` destination: `destTy->getThrownError()` is null; make
    sure `withThrows(true, Type())` lowers to the same type as an explicit
    `{ () throws in }` closure.
  - autoclosures (`AbstractClosureExpr` that is not a `ClosureExpr`).
  - the `$error` debug placeholder in `emitBasicProlog`
    (`SILGenProlog.cpp`) now that the closure's formal type is throwing.
  - non-escaping closures and `nonisolated(nonsending)` destinations.
  - generic contexts: the archetype guard must keep the thunk path
    (`testGenericError` in `typed_throws_conversion.swift`).
- [ ] Reviewers: Doug Gregor / John McCall (peephole and GH #71401 history),
      Kavon Farvardin (415e6133772), Max Desiatov (d12bc5d1f56).

## PR 2 (optional hardening): region isolation

- [ ] Treat a `[reabstraction_thunk]` `partial_apply` around an isolated
      closure like the closure itself, so thunks introduced for other
      reasons do not produce the same false positive.
- [ ] `getUnderlyingPartialApply` (`RegionAnalysis.cpp`) uses
      `getArgument(0)`, which is the isolation operand for `[isolated_any]`
      thunks, not the closure.
- [ ] Coordinate with Michael Gottesman / Pavel Yaskevich.

## PR 3: stdlib re-adoption

- [ ] In `Task+init.swift.gyb` and `Task+immediate.swift.gyb`, keep both
      existing overloads (`Failure == Never` non-throwing and
      `Failure == any Error` untyped `throws`) and ADD the unconstrained
      `throws(Failure)` overload. Do not restore the single-overload shape
      of #87822. With mock types, the regression cases resolve to the
      concrete overloads (today's codegen), and `Task { () throws(E) in }`
      resolves to the generic one.
- [ ] Keep the `Task.value` ABI shim (AEIC `$sScT6_valuexvg` + `_abi_value`
      at `$sScT5valuexvg`); only switch the AEIC getter to `throws(Failure)`.
- [ ] Keep the `#if $Embedded` handling.
- [ ] Known intentional change: passing a typed-throws function value by
      reference (`Task(operation: f)`) yields `Task<_, E>` instead of
      `Task<_, any Error>`. Measure; if it breaks real code, gate the generic
      overload with `@available(swift, introduced: 7)`.
- [ ] Unannotated `try` closures still infer `any Error` (`FullTypedThrows`
      is experimental), so the user-visible win needs explicit
      `throws(E)` closures or a typed context.

## Gates before merging PR 3

- [ ] Lit source-compat test with the reductions above against the real
      `Task`: `-swift-version 5 -strict-concurrency=complete`,
      `-swift-version 6`, `-default-isolation MainActor`,
      `NonisolatedNonsendingByDefault`; cover `detached`, `immediate`,
      `immediateDetached` and the `executorPreference:` variants.
- [ ] Back-deployment executable test: errors thrown through `Task.value`
      must be visible when running against an older runtime (the reproducer
      attached to rdar://168273419).
- [ ] ABI: `test/abi` concurrency baselines and
      `test/api-digester/stability-concurrency-abi.test` (only `Added:`
      lines, never edit baselines).
- [ ] SE-0520 unused-throwing-task warning tests (#87439) still pass with
      three overloads.
- [ ] Type-checker performance of large `Task {}` bodies with 3 overloads per
      API instead of 2.
- [ ] `@swift-ci please test source compatibility`, plus broader source
      compatibility testing beyond the OSS suite, since the March breakage
      was not covered by it.
- [ ] Land on main only, no release-branch cherry-pick, and let it ride a full
      cycle.
- [ ] Update the SE-0520 text (it states the adoption already happened) or
      cover it in the unstructured task errors proposal
      (swift-evolution #2990).

## Local prototype notes

`/tmp/typed-throws-proto/` holds `compile.sh` / `link.sh`, which rebuild a
frontend from the changed TUs into `/tmp/typed-throws-proto/bin` without
touching the shared build dir. lit picks it up via
`SWIFT_FRONTEND=/tmp/typed-throws-proto/bin/swift-frontend`; executable tests
need `swiftc -driver-use-frontend-path`. `compile.sh` and `link.sh` only cover
`SILGenExpr.cpp` / `libswiftSILGen.a` so far; extend them for
`TypeLowering.cpp` / `libswiftSIL.a`.
