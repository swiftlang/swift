// Synthetic-input tests: pipe handcrafted verifier output to
// update-verify-tests so we can exercise marker shapes the compiler does
// not naturally emit (literal backslashes and brace escapes in fix-it
// replacements).

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// '\\' in a fix-it replacement: the verifier emits literal two-character
// escape '\\'; update-verify-tests stores the entire marker as opaque text
// and writes it back byte-for-byte.
// RUN: printf '%%s\n' \
// RUN:   '%t/backslash.swift:1:55: error: expected fix-it not seen; actual fix-it seen: {{1-1=foo\\}}' \
// RUN:   "let a = 0 // expected-warning{{msg}} {{1-1=wrong}}" \
// RUN:   "                                    ^~~~~~~~~~~~" \
// RUN:   "                                    {{1-1=foo\\}}" \
// RUN:   | %update-verify-tests
// RUN: %diff %t/backslash.swift %t/backslash.swift.expected

// '\}' in a fix-it replacement: same round-trip story; the marker terminates
// at the first unescaped '}}' and the inner '\}' is preserved as text.
// RUN: printf '%%s\n' \
// RUN:   '%t/brace.swift:1:55: error: expected fix-it not seen; actual fix-it seen: {{1-1=a\}b}}' \
// RUN:   "let a = 0 // expected-warning{{msg}} {{1-1=wrong}}" \
// RUN:   "                                    ^~~~~~~~~~~~" \
// RUN:   "                                    {{1-1=a\}b}}" \
// RUN:   | %update-verify-tests
// RUN: %diff %t/brace.swift %t/brace.swift.expected

// 'expected-X N {{msg}} {{fix-it}}' with distinct actual fix-its per
// occurrence. Putting the count and the fix-it on the same warning
// directive (no separate note involved) directly exercises the split
// logic on a single Diag with count > 1.
// RUN: printf '%%s\n' \
// RUN:   '%t/count.swift:1:30: error: expected fix-it not seen; actual fix-it seen: {{1-1=A}}' \
// RUN:   "// expected-warning@+1 2 {{msg}} {{1-1=wrong}}" \
// RUN:   "                             ^~~~~~~~~~~~" \
// RUN:   "                             {{1-1=A}}" \
// RUN:   '%t/count.swift:1:30: error: expected fix-it not seen; actual fix-it seen: {{1-1=B}}' \
// RUN:   "// expected-warning@+1 2 {{msg}} {{1-1=wrong}}" \
// RUN:   "                             ^~~~~~~~~~~~" \
// RUN:   "                             {{1-1=B}}" \
// RUN:   | %update-verify-tests
// RUN: %diff %t/count.swift %t/count.swift.expected

//--- backslash.swift
let a = 0 // expected-warning{{msg}} {{1-1=wrong}}

//--- backslash.swift.expected
let a = 0 // expected-warning{{msg}} {{1-1=foo\\}}

//--- brace.swift
let a = 0 // expected-warning{{msg}} {{1-1=wrong}}

//--- brace.swift.expected
let a = 0 // expected-warning{{msg}} {{1-1=a\}b}}

//--- count.swift
// expected-warning@+1 2 {{msg}} {{1-1=wrong}}
let x = 1

//--- count.swift.expected
// expected-warning@+2  {{msg}} {{1-1=A}}
// expected-warning@+1  {{msg}} {{1-1=B}}
let x = 1

