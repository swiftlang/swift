// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func wrongFixit() {
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{3-12=_}}
}

func noActualFixit() {
  // The compiler emits this diagnostic with no fix-it, so the expected
  // fix-it should be removed entirely.
  undefined() // expected-error{{cannot find 'undefined' in scope}} {{1-1=oops}}
}

func noneToFixit() {
  // {{none}} forbids any fix-its; verifier complains and we replace it with
  // the actual fix-it (preserving the {{none}} so the constraint stays).
  let c = 2 // expected-warning{{initialization of immutable value 'c' was never used}} {{none}}
}

@available(*, deprecated, renamed: "bar(example:)")
func foo(b: Int) {}

// Returning Int so it can be used in expressions like
// `fooReturn(b: 1) + fooReturn(b: 2)` without a separate "result unused"
// warning getting in the way of the count test.
@available(*, deprecated, renamed: "bar(example:)")
func fooReturn(b: Int) -> Int { 0 }

func unexpectedFixitWithNone() {
  // The expected fix-it list matches one of the actual fix-its, and {{none}}
  // forbids any extras; the compiler emits an additional fix-it. Verifier
  // reports "unexpected fix-it seen" and we widen the list while preserving
  // {{none}}.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{none}}
  foo(b: 1)
}

func multipleActualFixitsPlural() {
  // The compiler emits two fix-its on the note. Source has a single wrong
  // fix-it, so verifier reports the plural form
  // "actual fix-its seen: {{...}} {{...}}". Both markers should land in the
  // updated source.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{1-2=wrong}}
  foo(b: 1)
}

func twoSourceFixitsReplaced() {
  // Source has two fix-its in a single run; neither matches an actual
  // fix-it. The verifier's replacement range covers from the first '{{' to
  // the last '}}' so the entire source run is rewritten.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{1-2=a}} {{2-3=b}}
  foo(b: 1)
}

func alternationRoundTrip() {
  // '||' is a verifier-side alternation operator: any one of the markers
  // separated by '||' may match. Here the first alternative matches the
  // first actual fix-it, so the verifier reports nothing on this line and
  // update-verify-tests must preserve the '||' run verbatim.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} || {{3-6=other}} {{7-8=example}}
  foo(b: 1)
}

func alternationReplaced() {
  // Neither alternative matches an actual fix-it. The replacement range
  // covers the entire run including '||', so the alternation collapses
  // into the actual marker(s).
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{1-2=a}} || {{2-3=b}}
  foo(b: 1)
}

func fixitBeforeDocFile() {
  // The fix-it appears before the {{documentation-file=...}} marker on the
  // warning line. consume_trailing_fixits should capture the fix-it and stop
  // at the documentation-file marker, leaving it intact across the update.
  // The compiler does not actually emit a fix-it on this warning, so the
  // verifier reports "expected fix-it not seen" with no actual fix-it; the
  // wrong fix-it should be dropped while the documentation-file is preserved.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}} {{1-2=wrong}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}}
  foo(b: 1)
}

func greedyTrailingBraces() {
  // The verifier's fix-it parser is greedy: trailing '}' characters are
  // absorbed into the marker so {{1-1=}}}} represents one fix-it whose
  // replacement is the literal '}}'. The Python regex must match the same
  // run, otherwise the source line shape would drift after the update.
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{1-1=}}}}
}

struct OperatorTestType {}

// Fix-its that insert a newline are emitted by the verifier with a literal
// '\n' escape in the replacement text. update-verify-tests preserves the
// marker as opaque text, so the escape round-trips byte-for-byte.
func <=>(lhs: OperatorTestType, rhs: OperatorTestType) -> OperatorTestType {} // expected-error {{operator implementation without matching operator declaration}} {{1-2=wrong}}

@available(*, deprecated, message: "old")
func deprecatedNoRename(_ x: Int) -> Int { 0 }

func countWithFixit() {
  // The 'N' between the kind and the message duplicates the expectation N
  // times; each copy carries the same fix-it list. The compiler emits two
  // warnings on the line below with no fix-its on either, so the verifier
  // emits two fix-it mismatches at the same location. update-verify-tests
  // should drop the wrong fix-it idempotently.
  // expected-warning@+1 2 {{'deprecatedNoRename' is deprecated: old}} {{1-2=wrong}}{{documentation-file=deprecated-declaration}}
  _ = deprecatedNoRename(1) + deprecatedNoRename(2)
}

// The fix-it lands on a line different from the diagnostic, so the marker
// uses 'line:col' syntax in the start position (with a relative line offset
// here on the source side). The verifier accepts this expectation exactly,
// so consume_trailing_fixits must capture the entire marker including the
// colon and round-trip it unchanged.
extension OperatorTestType {
    static func <=>(lhs: OperatorTestType, rhs: OperatorTestType) -> OperatorTestType { lhs } // expected-error {{operator implementation without matching operator declaration}} {{-1:1-1=infix operator <=> : <# Precedence Group #>\n}}
}

// Same diagnostic shape, but the source has {{none}} instead of the correct
// fix-it. The verifier reports "expected no fix-its; actual fix-it seen:
// {{N:1-1=...}}" with an absolute line number; update-verify-tests should
// insert the actual marker (colon, line:col offset and all) while
// preserving the {{none}} marker after it.
extension OperatorTestType {
    static func <=<(lhs: OperatorTestType, rhs: OperatorTestType) -> OperatorTestType { lhs } // expected-error {{operator implementation without matching operator declaration}} {{none}}
}

func multiLineFixitErrors() {
  // Two unrelated diagnostics on adjacent lines, each with its own wrong
  // fix-it. The per-Diag actual_fixits state must be independent across
  // lines.
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{1-2=A}}
  let b = 3 // expected-warning{{initialization of immutable value 'b' was never used}} {{1-2=B}}
}

func groupNamePreserved(_ x: Int) {
  // {{group-name=...}} is parsed by the verifier as a separate field, not a
  // fix-it. consume_trailing_fixits must skip it (and any
  // {{documentation-file=...}}) when consuming fix-it markers, and
  // _render_fixits must re-emit those preserved markers in source order
  // when the run is rewritten.
  // expected-warning@+1 {{'if' condition is always true}}{{group-name=UselessConditionalStatement}} {{1-2=wrong}}
  if case _ = x {
    _ = 0
  }
}

func countWithDifferentFixits() {
  // The note directive below carries both `count = 2` and a fix-it on the
  // same line. The two fooReturn calls land on different columns, so each
  // emits its own rename fix-its and the verifier reports two distinct
  // FixitErrors against this directive. update_lines must split the count
  // into one count-1 directive per occurrence so each actual fix-it set
  // lands on its own note.
  // expected-warning@+2 2 {{'fooReturn(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 2 {{use 'bar(example:)' instead}} {{1-2=wrong}}
  _ = fooReturn(b: 1) + fooReturn(b: 2)
}

func wrongCategoryWithFixit() {
  // The expected category mismatches the actual. The verifier reports both
  // a wrong-category error and a fix-it mismatch on the same diagnostic.
  // The fix-it from the FixitError must be transferred from the dead
  // (wrong-category) diag onto the synthesized replacement diag, so it is
  // not silently dropped.
  let a = 2 // expected-error{{initialization of immutable value 'a' was never used}} {{1-2=wrong}}
}

//--- test.swift.expected
func wrongFixit() {
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{3-8=_}}
}

func noActualFixit() {
  // The compiler emits this diagnostic with no fix-it, so the expected
  // fix-it should be removed entirely.
  undefined() // expected-error{{cannot find 'undefined' in scope}}
}

func noneToFixit() {
  // {{none}} forbids any fix-its; verifier complains and we replace it with
  // the actual fix-it (preserving the {{none}} so the constraint stays).
  let c = 2 // expected-warning{{initialization of immutable value 'c' was never used}} {{3-8=_}} {{none}}
}

@available(*, deprecated, renamed: "bar(example:)")
func foo(b: Int) {}

// Returning Int so it can be used in expressions like
// `fooReturn(b: 1) + fooReturn(b: 2)` without a separate "result unused"
// warning getting in the way of the count test.
@available(*, deprecated, renamed: "bar(example:)")
func fooReturn(b: Int) -> Int { 0 }

func unexpectedFixitWithNone() {
  // The expected fix-it list matches one of the actual fix-its, and {{none}}
  // forbids any extras; the compiler emits an additional fix-it. Verifier
  // reports "unexpected fix-it seen" and we widen the list while preserving
  // {{none}}.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}} {{none}}
  foo(b: 1)
}

func multipleActualFixitsPlural() {
  // The compiler emits two fix-its on the note. Source has a single wrong
  // fix-it, so verifier reports the plural form
  // "actual fix-its seen: {{...}} {{...}}". Both markers should land in the
  // updated source.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}}
  foo(b: 1)
}

func twoSourceFixitsReplaced() {
  // Source has two fix-its in a single run; neither matches an actual
  // fix-it. The verifier's replacement range covers from the first '{{' to
  // the last '}}' so the entire source run is rewritten.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}}
  foo(b: 1)
}

func alternationRoundTrip() {
  // '||' is a verifier-side alternation operator: any one of the markers
  // separated by '||' may match. Here the first alternative matches the
  // first actual fix-it, so the verifier reports nothing on this line and
  // update-verify-tests must preserve the '||' run verbatim.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} || {{3-6=other}} {{7-8=example}}
  foo(b: 1)
}

func alternationReplaced() {
  // Neither alternative matches an actual fix-it. The replacement range
  // covers the entire run including '||', so the alternation collapses
  // into the actual marker(s).
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}}
  foo(b: 1)
}

func fixitBeforeDocFile() {
  // The fix-it appears before the {{documentation-file=...}} marker on the
  // warning line. consume_trailing_fixits should capture the fix-it and stop
  // at the documentation-file marker, leaving it intact across the update.
  // The compiler does not actually emit a fix-it on this warning, so the
  // verifier reports "expected fix-it not seen" with no actual fix-it; the
  // wrong fix-it should be dropped while the documentation-file is preserved.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}}
  foo(b: 1)
}

func greedyTrailingBraces() {
  // The verifier's fix-it parser is greedy: trailing '}' characters are
  // absorbed into the marker so {{1-1=}}}} represents one fix-it whose
  // replacement is the literal '}}'. The Python regex must match the same
  // run, otherwise the source line shape would drift after the update.
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{3-8=_}}
}

struct OperatorTestType {}

// Fix-its that insert a newline are emitted by the verifier with a literal
// '\n' escape in the replacement text. update-verify-tests preserves the
// marker as opaque text, so the escape round-trips byte-for-byte.
func <=>(lhs: OperatorTestType, rhs: OperatorTestType) -> OperatorTestType {} // expected-error {{operator implementation without matching operator declaration}} {{1-1=infix operator <=> : <# Precedence Group #>\n}}

@available(*, deprecated, message: "old")
func deprecatedNoRename(_ x: Int) -> Int { 0 }

func countWithFixit() {
  // The 'N' between the kind and the message duplicates the expectation N
  // times; each copy carries the same fix-it list. The compiler emits two
  // warnings on the line below with no fix-its on either, so the verifier
  // emits two fix-it mismatches at the same location. update-verify-tests
  // should drop the wrong fix-it idempotently.
  // expected-warning@+1 2 {{'deprecatedNoRename' is deprecated: old}}{{documentation-file=deprecated-declaration}}
  _ = deprecatedNoRename(1) + deprecatedNoRename(2)
}

// The fix-it lands on a line different from the diagnostic, so the marker
// uses 'line:col' syntax in the start position (with a relative line offset
// here on the source side). The verifier accepts this expectation exactly,
// so consume_trailing_fixits must capture the entire marker including the
// colon and round-trip it unchanged.
extension OperatorTestType {
    static func <=>(lhs: OperatorTestType, rhs: OperatorTestType) -> OperatorTestType { lhs } // expected-error {{operator implementation without matching operator declaration}} {{-1:1-1=infix operator <=> : <# Precedence Group #>\n}}
}

// Same diagnostic shape, but the source has {{none}} instead of the correct
// fix-it. The verifier reports "expected no fix-its; actual fix-it seen:
// {{N:1-1=...}}" with an absolute line number; update-verify-tests should
// insert the actual marker (colon, line:col offset and all) while
// preserving the {{none}} marker after it.
extension OperatorTestType {
    static func <=<(lhs: OperatorTestType, rhs: OperatorTestType) -> OperatorTestType { lhs } // expected-error {{operator implementation without matching operator declaration}} {{-1:1-1=infix operator <=< : <# Precedence Group #>\n}} {{none}}
}

func multiLineFixitErrors() {
  // Two unrelated diagnostics on adjacent lines, each with its own wrong
  // fix-it. The per-Diag actual_fixits state must be independent across
  // lines.
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{3-8=_}}
  let b = 3 // expected-warning{{initialization of immutable value 'b' was never used}} {{3-8=_}}
}

func groupNamePreserved(_ x: Int) {
  // {{group-name=...}} is parsed by the verifier as a separate field, not a
  // fix-it. consume_trailing_fixits must skip it (and any
  // {{documentation-file=...}}) when consuming fix-it markers, and
  // _render_fixits must re-emit those preserved markers in source order
  // when the run is rewritten.
  // expected-warning@+1 {{'if' condition is always true}}{{group-name=UselessConditionalStatement}} {{3-+2:4=_ = 0}}
  if case _ = x {
    _ = 0
  }
}

func countWithDifferentFixits() {
  // The note directive below carries both `count = 2` and a fix-it on the
  // same line. The two fooReturn calls land on different columns, so each
  // emits its own rename fix-its and the verifier reports two distinct
  // FixitErrors against this directive. update_lines must split the count
  // into one count-1 directive per occurrence so each actual fix-it set
  // lands on its own note.
  // expected-note@+3  {{use 'bar(example:)' instead}} {{7-16=bar}} {{17-18=example}}
  // expected-warning@+2 2 {{'fooReturn(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1  {{use 'bar(example:)' instead}} {{25-34=bar}} {{35-36=example}}
  _ = fooReturn(b: 1) + fooReturn(b: 2)
}

func wrongCategoryWithFixit() {
  // The expected category mismatches the actual. The verifier reports both
  // a wrong-category error and a fix-it mismatch on the same diagnostic.
  // The fix-it from the FixitError must be transferred from the dead
  // (wrong-category) diag onto the synthesized replacement diag, so it is
  // not silently dropped.
  // expected-warning@+1{{initialization of immutable value 'a' was never used}} {{3-8=_}}
  let a = 2
}

