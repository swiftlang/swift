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

// RUN: not %target-typecheck-verify-swift -verify-child-notes 2>&1 | %FileCheck %s --implicit-check-not "relative line offsets" --implicit-check-not "cannot be defined inside"

func anchor() {} // #site

// CHECK: :[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside {{expect}}ed-expansion
// expected-expansion@+3:14{{
//   expected-error@+1{{cannot use relative location using +}}
// }}
func usePlus() {}

// CHECK: :[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside
// expected-expansion@+3:14{{
//   expected-error@-1{{cannot use relative location using -}}
// }}
func useMinus() {}

// CHECK: :[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside
// expected-expansion@+3:14{{
//   expected-error@+0{{+0 is still relative}}
// }}
func usePlusZero() {}

// CHECK: :[[@LINE+2]]:{{[0-9]+}}: error: relative line offsets are not allowed inside
// expected-expansion@+3:14{{
//   expected-error@-0{{-0 is still relative}}
// }}
func useMinusZero() {}

// CHECK: :[[@LINE+3]]:{{[0-9]+}}: error: relative line offsets are not allowed inside
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

// CHECK: :[[@LINE+2]]:{{[0-9]+}}: error: location marker '#trailingMarker' cannot be defined inside
// expected-expansion@+3:14{{
//   expected-error@1{{oops}} // #trailingMarker
// }}
func markerTrailing() {}

// CHECK: :[[@LINE+3]]:{{[0-9]+}}: error: location marker '#ownLineMarker' cannot be defined inside
// expected-expansion@+4:14{{
//   expected-error@1{{oops}}
//   // #ownLineMarker
// }}
func markerOwnLine() {}
