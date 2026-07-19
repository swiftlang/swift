// Relative line offsets (`@+N` / `@-N`) are rejected on diagnostics and child
// notes nested inside an expansion block: a nested line is an absolute line
// within the virtual expansion buffer. Relative locations in the virtual buffer
// would refer to the outer buffer instead. This is more confusing to read than
// it should be, so we ban this footgun. Instead a location outside the
// expansion can be referred to using the `@#marker` syntax.
//
// For the same reason a location marker may not be *defined* inside an
// expansion block (a `// #name` there would name a physical line in the outer
// buffer, not a location in the virtual buffer).
//
// No macro is actually loaded, so the expansions themselves are reported as "not
// produced"; that noise is irrelevant to what is under test here.

// RUN: not %target-typecheck-verify-swift -verify-child-notes 2>&1 | %FileCheck %s --implicit-check-not "relative line offsets" --implicit-check-not "cannot be defined inside" --sanitize TEST=%s --match-full-lines
// Duplicate-marker definitions are diagnosed by the pre-scan and printed before
// the per-file diagnostics, so they are checked with a separate prefix whose
// matches are independent of the ordering above.
// RUN: not %target-typecheck-verify-swift -verify-child-notes 2>&1 | %FileCheck %s --check-prefix=CHECK-DUP --sanitize TEST=%s --match-full-lines

func anchor() {} // #site

// CHECK: TEST:[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside [[EXP:expected.expansion]]; use an absolute line number or a '@#marker' reference
// expected-expansion@+3:14{{
//   expected-error@+1{{cannot use relative location using +}}
// }}
func usePlus() {}

// CHECK: TEST:[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside [[EXP]]; use an absolute line number or a '@#marker' reference
// expected-expansion@+3:14{{
//   expected-error@-1{{cannot use relative location using -}}
// }}
func useMinus() {}

// CHECK: TEST:[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside [[EXP]]; use an absolute line number or a '@#marker' reference
// expected-expansion@+3:14{{
//   expected-error@+0{{+0 is still relative}}
// }}
func usePlusZero() {}

// CHECK: TEST:[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside [[EXP]]; use an absolute line number or a '@#marker' reference
// expected-expansion@+3:14{{
//   expected-error@-0{{-0 is still relative}}
// }}
func useMinusZero() {}

// CHECK: TEST:[[@LINE+3]]:{{[0-9]+}}: error: relative line offsets are not allowed inside [[EXP]]; use an absolute line number or a '@#marker' reference
// expected-expansion@+5:14{{
//   expected-error@2{{oops}} {{children:
//     expected-note@-1{{a child note nested inside an expansion may not use a relative offset either}}
//   }}
// }}
func childRelative() {}

// expected-expansion@+5:14{{
//   expected-error@2{{oops}} {{children:
//     expected-note@#site{{child}}
//   }}
// }}
func accepted() {}

// expected-error@+3{{oops}} {{children:
//   expected-note@+2{{this does not count as nested since it doesn't have a separate buffer}}
// }}
func accepted2() {}

// CHECK: TEST:[[@LINE+2]]:{{[0-9]+}}: error: location marker '#trailingMarker' inside [[EXP]] must use '#trailingMarker@LineNo' syntax
// expected-expansion@+3:14{{
//   expected-error@1{{oops}} // #trailingMarker
// }}
func markerTrailing() {}

// CHECK: TEST:[[@LINE+3]]:{{[0-9]+}}: error: location marker '#ownLineMarker' inside [[EXP]] must use '#ownLineMarker@LineNo' syntax
// expected-expansion@+4:14{{
//   expected-error@1{{oops}}
//   // #ownLineMarker
// }}
func markerOwnLine() {}

// CHECK: TEST:[[@LINE+1]]:{{[0-9]+}}: error: location marker '#outsideMarker@3' is only allowed inside [[EXP]]
// #outsideMarker@3
func markerOutside() {}

// CHECK: TEST:[[@LINE+2]]:{{[0-9]+}}: error: use of undefined location marker '#nope'
// expected-expansion@+3:14{{
//   expected-error@#nope{{oops}}
// }}
func undefinedMarkerRef() {}

func dupAnchor() {} // #dupPlain
// CHECK-DUP: TEST:[[@LINE+3]]:{{[0-9]+}}: error: location marker '#dupPlain' already defined
// expected-expansion@+3:14{{
//   expected-error@1{{oops}}
//   #dupPlain@2
// }}
func dupPlainInside() {}

// CHECK: TEST:[[@LINE+1]]:{{[0-9]+}}: error: location marker '#dupExp@3' is only allowed inside [[EXP]]
// #dupExp@3
// CHECK-DUP: TEST:[[@LINE+3]]:{{[0-9]+}}: error: location marker '#dupExp' already defined
// expected-expansion@+3:14{{
//   expected-error@1{{oops}}
//   #dupExp@2
// }}
func dupExpInside() {}
