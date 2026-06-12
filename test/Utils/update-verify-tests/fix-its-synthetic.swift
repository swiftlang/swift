// Synthetic-input tests: feed handcrafted verifier output to
// update-verify-tests so we can exercise marker shapes the compiler does
// not naturally emit (literal backslashes and brace escapes in fix-it
// replacements).
//
// The synthetic verifier output lives in its own split-file sections rather
// than being assembled by `printf` on the RUN line. That avoids
// MSYS/Windows command-line argument unescaping (`\\` collapsing to `\`)
// when lit's quote_windows_command hands a Windows-quoted command line to
// an MSYS-based printf.exe; split-file copies bytes verbatim. We `cd %t`
// so update-verify-tests resolves the bare filename in the input against
// the test's scratch directory.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// '\\' in a fix-it replacement: the verifier emits literal two-character
// escape '\\'; update-verify-tests stores the entire marker as opaque text
// and writes it back byte-for-byte.
// RUN: cd %t && %update-verify-tests < %t/backslash.input
// RUN: %diff %t/backslash.swift %t/backslash.swift.expected

// '\}' in a fix-it replacement: same round-trip story; the marker terminates
// at the first unescaped '}}' and the inner '\}' is preserved as text.
// RUN: cd %t && %update-verify-tests < %t/brace.input
// RUN: %diff %t/brace.swift %t/brace.swift.expected

//--- backslash.input
backslash.swift:1:55: error: expected fix-it not seen; actual fix-it seen: {{1-1=foo\\}}
let a = 0 // expected-warning{{msg}} {{1-1=wrong}}
                                    ^~~~~~~~~~~~
                                    {{1-1=foo\\}}

//--- backslash.swift
let a = 0 // expected-warning{{msg}} {{1-1=wrong}}

//--- backslash.swift.expected
let a = 0 // expected-warning{{msg}} {{1-1=foo\\}}

//--- brace.input
brace.swift:1:55: error: expected fix-it not seen; actual fix-it seen: {{1-1=a\}b}}
let a = 0 // expected-warning{{msg}} {{1-1=wrong}}
                                    ^~~~~~~~~~~~
                                    {{1-1=a\}b}}

//--- brace.swift
let a = 0 // expected-warning{{msg}} {{1-1=wrong}}

//--- brace.swift.expected
let a = 0 // expected-warning{{msg}} {{1-1=a\}b}}

