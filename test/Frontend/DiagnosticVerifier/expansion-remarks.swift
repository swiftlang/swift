// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UnstringifyMacroDefinition) -module-name=UnstringifyMacroDefinition %S/Inputs/macro/unstringify-macro.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -Rmacro-expansions -typecheck %t/main.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -Rmacro-expansions -typecheck %t/empty-no-expansion.swift 2>&1 | %FileCheck --check-prefix=CHECK-MISSING %t/empty-no-expansion.swift

// RUN: %target-swift-frontend-verify -swift-version 5 -verify-child-notes -typo-correction-limit 10 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-positive.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -verify-child-notes -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-negative.swift 2>&1 | %FileCheck %t/cross-buffer-location-negative.swift --implicit-check-not error: --implicit-check-not warning:
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-badline.swift 2>&1 | %FileCheck --check-prefix=CHECK-BADLINE %t/cross-buffer-location-badline.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-wrongline.swift 2>&1 | %FileCheck --check-prefix=CHECK-WRONGLINE %t/cross-buffer-location-wrongline.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-dup.swift 2>&1 | %FileCheck --check-prefix=CHECK-DUP %t/cross-buffer-location-dup.swift

// RUN: %target-swift-frontend-verify -swift-version 5 -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/ignore-macro-note.swift
// RUN: %target-swift-frontend-verify -swift-version 5 -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/ignore-macro-note.swift -verify-child-notes
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/ignore-macro-note.swift 2>&1 | %FileCheck --check-prefix=DISABLED %t/ignore-macro-note.swift

// Multiple expansions attached at the same source location (several peer macros
// on one declaration). Each produces a distinct sibling expansion buffer, and
// expansion directives are matched to them by position.
// RUN: %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multi-both.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multi-misrouting.swift 2>&1 | %FileCheck --check-prefix=CHECK-MISROUTE %t/multi-misrouting.swift
// RUN: %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multi-markers-outside.swift
// RUN: %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multi-marker-sibling.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multi-more-directives.swift 2>&1 | %FileCheck --check-prefix=CHECK-MOREDIRS %t/multi-more-directives.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multi-more-expansions.swift 2>&1 | %FileCheck --check-prefix=CHECK-MOREEXP %t/multi-more-expansions.swift

//--- main.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 *{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("func foo(_ x: Int) {\nlet a = 2\nlet b = x\n}")
func foo() {}
/*
expected-expansion@-2:14{{
    expected-remark@1{{macro content: |func foo(_ x: Int) {|}}
    expected-remark@2{{macro content: |    let a = 2|}}
    expected-ignored-error@42{{this diagnostic should not affect the expansion}}
    expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
    expected-remark@3{{macro content: |    let b = x|}}
    expected-warning@3{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
    expected-remark@4{{macro content: |}|}}
}}
*/

@freestanding(expression) public macro ExprMacro() -> String = #file

func bar() {
    // expected-note@+1{{in expansion of macro 'ExprMacro' here}}
    let _ = #ExprMacro()
    /*
    expected-expansion@-2:13{{
        expected-remark@1{{macro content: |#file|}}
    }}
    */
}

//--- empty-no-expansion.swift
// CHECK-MISSING:     error: expected expansion not produced
// CHECK-MISSING-NOT: didn't find '}}'
// CHECK-MISSING-NOT: no expansion with diagnostics starting at

// expected-expansion@+2:14{{
// }}
func qux() {}

//--- cross-buffer-location-positive.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {\na = 2\nb = x\n}") // #here
func foo() {}
// Every diagnostic emitted inside the expansion carries the same "in expansion
// of macro" child note, whose location is the '@unstringifyPeer' attribute in
// this (outer) file.
// expected-expansion@-4:14{{
//   #xLine@1
//   #aLine@2
//   expected-error@#bLine{{cannot find 'b' in scope; did you mean 'x'?}} {{children:
//     expected-note@#here{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
//     expected-note@1{{'x' declared here}}
//   }}
//   #bLine@3
// }}
// Refer to location inside expansion from outside of it:
// expected-error@#aLine{{cannot find 'a' in scope; did you mean 'x'?}} {{children:
//   expected-note@#here{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
//   expected-note@#xLine{{'x' declared here}}
// }}

//--- cross-buffer-location-negative.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// The child note message is wrong, so the real child note is reported as
// unexpected and the expected child note is not produced.
// CHECK: :[[@LINE+1]]:1: error: unexpected child note produced: in expansion of macro 'unstringifyPeer' on global function 'foo()' here
@unstringifyPeer("func foo(_ y: Int) {\nqqq()\n}") // #here2
func foo() {}
// expected-error@#asdf {{missing error}}
// expected-expansion@-2:14{{
//   CHECK: :[[@LINE+1]]:6: note: for parent matched here
//   expected-error@2{{cannot find 'qqq' in scope}} {{children:
//     CHECK: :[[@LINE-4]]:4: error: expected error not produced
//     CHECK: :[[@LINE+1]]:8: error: expected note not produced
//     expected-note@#here2{{WRONG MESSAGE}}
//   }}
//   #asdf@2
// }}

//--- cross-buffer-location-badline.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// The expansion buffer only spans four lines, so '@99' names a line that does
// not exist.
@unstringifyPeer("func foo(_ x: Int) {\na = 2\nb = x\n}")
func foo() {}
// expected-expansion@-1:14{{
//   CHECK-BADLINE: :[[@LINE+1]]:{{[0-9]+}}: error: line 99 does not exist in the expansion buffer
//   #bad@99
//   expected-error@2{{cannot find 'a' in scope}}
//   expected-error@3{{cannot find 'b' in scope}}
// }}

//--- cross-buffer-location-wrongline.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {\na = 2\nb = x\n}")
func foo() {}
// expected-expansion@-1:14{{
//   #wrong@3
//   expected-error@2{{cannot find 'a' in scope}}
//   expected-error@3{{cannot find 'b' in scope}}
// }}
// CHECK-WRONGLINE: :[[@LINE+1]]:4: error: expected error not produced
// expected-error@#wrong{{cannot find 'a' in scope; did you mean 'x'?}}

//--- cross-buffer-location-dup.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {\na = 2\nb = x\n}") // #dup
func foo() {}
// expected-expansion@-1:14{{
//   CHECK-DUP: :[[@LINE+1]]:{{[0-9]+}}: error: location marker '#dup' already defined
//   #dup@2
//   expected-error@2{{cannot find 'a' in scope}}
//   expected-error@3{{cannot find 'b' in scope}}
// }}

//--- ignore-macro-note.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// DISABLED: ignore-macro-note.swift:[[@LINE+1]]:1: error: unexpected note produced: in expansion of macro 'unstringifyPeer' on global function 'foo()' here
@unstringifyPeer("func foo(_ x: Int) {\nlet a = 2\n}")
func foo() {}
// expected-expansion@-1:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}

//--- multi-both.swift
// Two peer macros attached to the same declaration expand at the same source
// location (column 14 of 'func baz() {}'), producing two sibling expansion
// buffers. The verifier gives each a positional "expansion index" following the
// source order of the macro attributes, and routes the expansion directives to
// those indices in the order the directives appear in the source. Here each
// sibling produces its own distinct diagnostic: the first directive matches
// expansion 0 (from unstringifyPeer) and the second matches expansion 1 (from
// unstringifyPeer2). This covers a diagnostic present in expansion 0, one
// present in expansion 1, and thus both expansions carrying a diagnostic.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("func baz(_ x: Int) {\nlet a = x\n}")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("func baz(_ y: String) {\nlet b = y\n}")
// expected-expansion@+6:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- multi-misrouting.swift
// The same two-expansion setup as multi-both.swift, but the two directives are
// swapped: the first directive (expansion 0) expects the diagnostic that only
// occurs in expansion 1, and vice versa..
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("func baz(_ x: Int) {\nlet a = x\n}")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("func baz(_ y: String) {\nlet b = y\n}")
// The nested diagnostics are matched against the wrong sibling buffers.
// CHECK-MISROUTE: :[[@LINE+2]]:6: error: expected warning not produced
// expected-expansion@+13:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// CHECK-MISROUTE: :[[@LINE+2]]:6: error: expected warning not produced
// expected-expansion@+9:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// The real diagnostics are then reported as unexpected against the sibling that
// actually produced them.
// CHECK-MISROUTE: :2:9: error: unexpected warning produced: initialization of immutable value 'b'
// CHECK-MISROUTE: :[[@LINE+3]]:14: note: in expansion 1 from here
// CHECK-MISROUTE: :2:9: error: unexpected warning produced: initialization of immutable value 'a'
// CHECK-MISROUTE: :[[@LINE+1]]:14: note: in expansion 0 from here
func baz() {}

//--- multi-markers-outside.swift
// A '#name@N' location marker defined inside a sibling expansion binds to line
// N of that expansion's buffer, not merely the first sibling. Here a
// marker is defined in each of the two expansions and referenced by directives
// outside the expansion blocks; each must resolve into its own buffer.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("func baz(_ x: Int) {\nlet a = x\n}")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("func baz(_ y: String) {\nlet b = y\n}")
// expected-expansion@+6:14{{
//   #aMarker@2
// }}
// expected-expansion@+3:14{{
//   #bMarker@2
// }}
func baz() {}
// expected-warning@#aMarker{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// expected-warning@#bMarker{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}

//--- multi-marker-sibling.swift
// A '#name@N' marker defined inside one sibling expansion (expansion 1) is
// referenced by a directive that lives inside the other sibling's block
// (expansion 0). The reference must resolve to the defining expansion's buffer,
// exercising cross-sibling marker routing rather than binding to the block the
// reference textually sits in.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("func baz(_ x: Int) {\nlet a = x\n}")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("func baz(_ y: String) {\nlet b = y\n}")
// The '@#bMarker' reference below sits in expansion 0's block but resolves to
// expansion 1's buffer, where '#bMarker' is defined.
// expected-expansion@+7:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-warning@#bMarker{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+3:14{{
//   #bMarker@2
// }}
func baz() {}

//--- multi-more-directives.swift
// More expansion directives than expansions actually produced. The
// surplus directive has no sibling to bind to and is reported as "expected
// expansion not produced". 'baz' exercises a surplus directive that
// carries nested expectations; 'qux' exercises an empty surplus directive.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("func baz(_ x: Int) {\nlet a = x\n}")
// expected-expansion@+9:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// The surplus directive, and its nested expectation, have no expansion to bind.
// CHECK-MOREDIRS: :[[@LINE+2]]:4: error: expected expansion not produced
// CHECK-MOREDIRS: :[[@LINE+2]]:6: error: expected warning not produced
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'qux()' here}}
@unstringifyPeer("func qux(_ x: Int) {\nlet a = x\n}")
// expected-expansion@+6:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// CHECK-MOREDIRS: :[[@LINE+1]]:4: error: expected expansion not produced
// expected-expansion@+2:14{{
// }}
func qux() {}

//--- multi-more-expansions.swift
// Fewer expansion directives than expansions actually produced. The
// single directive claims expansion 0; the remaining sibling's diagnostics are
// left over and reported as unexpected, tagged with the index of the expansion
// they came from. 'baz' uses a directive with nested expectations; 'qux' uses
// an empty directive, so both siblings' diagnostics are left unclaimed.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("func baz(_ x: Int) {\nlet a = x\n}")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("func baz(_ y: String) {\nlet b = y\n}")
// The lone directive claims expansion 0 ('a'); expansion 1 ('b') is unclaimed.
// CHECK-MOREEXP: :2:9: error: unexpected warning produced: initialization of immutable value 'b'
// CHECK-MOREEXP: :[[@LINE+4]]:14: note: in expansion 1 from here
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'qux()' here}}
@unstringifyPeer("func qux(_ x: Int) {\nlet a = x\n}")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'qux()' here}}
@unstringifyPeer2("func qux(_ y: String) {\nlet b = y\n}")
// The empty directive claims expansion 0 but expects nothing, so both siblings'
// diagnostics are unclaimed.
// CHECK-MOREEXP: :2:9: error: unexpected warning produced: initialization of immutable value 'b'
// CHECK-MOREEXP: :[[@LINE+5]]:14: note: in expansion 1 from here
// CHECK-MOREEXP: :2:9: error: unexpected warning produced: initialization of immutable value 'a'
// CHECK-MOREEXP: :[[@LINE+3]]:14: note: in expansion 0 from here
// expected-expansion@+2:14{{
// }}
func qux() {}
