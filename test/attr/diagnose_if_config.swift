// RUN: %target-typecheck-verify-swift -verify-additional-prefix lenient-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix strict- -DSTRICT

@available(*, deprecated)
func dep() -> Bool { return false }

// `dep()` is deprecated, so it produces a warning by default. With `STRICT`
// defined the inner `@diagnose(..., as: error)` is active and promotes the
// warning to an error.
#if STRICT
@diagnose(DeprecatedDeclaration, as: error)
#endif
func onlyInsideIf() {
  let _ = dep()
  // expected-lenient-warning@-1 {{'dep()' is deprecated}}
  // expected-strict-error@-2 {{'dep()' is deprecated}}
}

// The outer `@diagnose(..., as: warning)` always applies. When `STRICT` is set,
// the inner `@diagnose(..., as: error)` comes after it in source order and
// wins.
@diagnose(DeprecatedDeclaration, as: warning)
#if STRICT
@diagnose(DeprecatedDeclaration, as: error)
#endif
func outerPlusIf() {
  let _ = dep()
  // expected-lenient-warning@-1 {{'dep()' is deprecated}}
  // expected-strict-error@-2 {{'dep()' is deprecated}}
}

// Active branch picks the corresponding attribute.
#if STRICT
@diagnose(DeprecatedDeclaration, as: error)
#else
@diagnose(DeprecatedDeclaration, as: ignored)
#endif
func ifElse() {
  let _ = dep()
  // No diagnostic in lenient mode (silenced by `as: ignored`).
  // expected-strict-error@-2 {{'dep()' is deprecated}}
}

#if STRICT
@diagnose(DeprecatedDeclaration, as: error)
#elseif OTHER
@diagnose(DeprecatedDeclaration, as: ignored)
#else
@diagnose(DeprecatedDeclaration, as: warning)
#endif
func elseifChain() {
  let _ = dep()
  // expected-lenient-warning@-1 {{'dep()' is deprecated}}
  // expected-strict-error@-2 {{'dep()' is deprecated}}
}

// Both layers must be active for the inner `@diagnose` to apply.
@diagnose(DeprecatedDeclaration, as: warning)
#if STRICT
#if NESTED
@diagnose(DeprecatedDeclaration, as: error)
#endif
#endif
func nestedIf() {
  let _ = dep()
  // expected-lenient-warning@-1 {{'dep()' is deprecated}}
  // expected-strict-warning@-2 {{'dep()' is deprecated}}
}

// Only present in the strict run; the lenient run must not see this decl at
// all (otherwise the verifier would complain about an unmatched diagnostic).
#if STRICT
@diagnose(DeprecatedDeclaration, as: error)
func wholeDeclOnlyIfStrict() {
  let _ = dep()
  // expected-strict-error@-1 {{'dep()' is deprecated}}
}
#endif
