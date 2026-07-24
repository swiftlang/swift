// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Building this macro takes some time, so amortise the cost by using it for multiple sub tests
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UnstringifyMacroDefinition) -module-name=UnstringifyMacroDefinition \
// RUN:   %S/Inputs/unstringify-macro.swift -g -no-toolchain-stdlib-rpath



// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/single.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/single.swift
// RUN: %diff %t/single.swift %t/single.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multiple.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multiple.swift
// RUN: %diff %t/multiple.swift %t/multiple.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/existing.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/existing.swift
// RUN: %diff %t/existing.swift %t/existing.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/gone.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/gone.swift
// RUN: %diff %t/gone.swift %t/gone.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/wrong-location.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/wrong-location.swift
// RUN: %diff %t/wrong-location.swift %t/wrong-location.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested.swift
// RUN: %diff %t/nested.swift %t/nested.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multiple-at-same-location.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multiple-at-same-location.swift
// RUN: %diff %t/multiple-at-same-location.swift %t/multiple-at-same-location.swift.expected

// Sibling expansions where each expansion emits several diagnostics: every
// diagnostic must route into the matching sibling and accumulate there.
// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-multiple-diags.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-multiple-diags.swift
// RUN: %diff %t/sibling-multiple-diags.swift %t/sibling-multiple-diags.swift.expected

// Sibling expansions with pre-existing (empty) directives already present: the
// diagnostics must be routed into the existing directives by index rather than
// synthesizing new ones.
// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-existing.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-existing.swift
// RUN: %diff %t/sibling-existing.swift %t/sibling-existing.swift.expected

// Three sibling expansions at one location, to check index routing scales past
// two.
// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-three.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-three.swift
// RUN: %diff %t/sibling-three.swift %t/sibling-three.swift.expected

// One sibling directive already exists (index 0); the other sibling must be
// synthesized (index 1) and laid out after the existing one so source order
// matches expansion-index order.
// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-partial.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-partial.swift
// RUN: %diff %t/sibling-partial.swift %t/sibling-partial.swift.expected

// Stale sibling directives whose target declaration was deleted, so their
// offsets dangle past the end of the file: they must be dropped rather than
// crashing on the out-of-range target.
// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-gone.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/sibling-gone.swift
// RUN: %diff %t/sibling-gone.swift %t/sibling-gone.swift.expected

// RUN: not %target-swift-frontend-verify -I %t -plugin-path %swift-plugin-dir -typecheck %t/unparsed.swift 2>%t/output.txt -Rmacro-expansions
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-UNPARSED %s

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/escaped.swift 2>%t/output.txt -Rmacro-expansions
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/escaped.swift -Rmacro-expansions
// RUN: %diff %t/escaped.swift %t/escaped.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/empty-expansion-fill.swift 2>%t/output.txt -Rmacro-expansions
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/empty-expansion-fill.swift -Rmacro-expansions
// RUN: %diff %t/empty-expansion-fill.swift %t/empty-expansion-fill.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/empty-expansion-remove.swift 2>%t/output.txt -Rmacro-expansions
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/empty-expansion-remove.swift -Rmacro-expansions
// RUN: %diff %t/empty-expansion-remove.swift %t/empty-expansion-remove.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/empty-expansion-for-prefix.swift 2>%t/output.txt -Rmacro-expansions
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/empty-expansion-for-prefix.swift -Rmacro-expansions
// RUN: %diff %t/empty-expansion-for-prefix.swift %t/empty-expansion-for-prefix.swift.expected

// A diagnostic emitted inside a macro expansion may carry a child note whose
// own location is inside the same expansion buffer (e.g. a redeclaration's
// "previously declared here" note). update-verify-tests can synthesize such a
// note using an absolute '@n' line, just like the other diagnostics inside an
// expansion. The outward "in expansion of macro" child note that every
// in-expansion diagnostic also carries is dropped with -verify-ignore-macro-note
// so only the synthesizable inward note remains.
// RUN: not %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/accept-child-note.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/accept-child-note.swift
// RUN: %diff %t/accept-child-note.swift %t/accept-child-note.swift.expected

// A "cannot find in scope" error carries its typo-correction "did you mean"
// notes as child notes, so inside an expansion they are synthesized into the
// parent error's `{{children:...}}` block rather than as free-standing notes.
// RUN: not %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/accept-typo-child-note.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/accept-typo-child-note.swift
// RUN: %diff %t/accept-typo-child-note.swift %t/accept-typo-child-note.swift.expected

// Without -verify-ignore-macro-note, that outward "in expansion of macro" note
// is verified too. It points back into the outer file and is only expressible
// with '@#marker' syntax, which update-verify-tests cannot synthesize, so it
// must fail with a clear message rather than emit a directive the verifier
// would reject.
// RUN: not %target-swift-frontend-verify -verify-child-notes -typo-correction-limit 10 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/reject-child-notes.swift 2>%t/output.txt
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-CHILD-NOTES %s

// CHECK-CHILD-NOTES: cannot synthesize a child note that points out of a macro expansion

// The mirror image: a diagnostic *outside* an expansion (in the outer file)
// whose child note lives *inside* the expansion buffer. Referencing into the
// expansion likewise needs '@#marker' syntax, which update-verify-tests cannot
// synthesize, so this too must fail with a clear message.
// RUN: not %target-swift-frontend-verify -verify-child-notes -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/reject-child-note-inward.swift 2>%t/output.txt
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-CHILD-NOTES-INWARD %s

// CHECK-CHILD-NOTES-INWARD: cannot synthesize a child note that points into a macro expansion

// A parent diagnostic in one expansion whose child note lives in a different
// expansion.
// RUN: not %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/reject-child-note-cross.swift 2>%t/output.txt
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-CHILD-NOTES-CROSS %s
// RUN: not %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/reject-child-note-cross-matched.swift 2>%t/output.txt
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-CHILD-NOTES-CROSS %s

// CHECK-CHILD-NOTES-CROSS: cannot synthesize a child note that lives in a different macro expansion

// A diagnostic inside a nested expansion.
// Its child notes are anchored via a chain of "in expansion from here" notes
// whose innermost entry lives in the intermediate expansion buffer, not the
// outer file. With -verify-child-notes this cannot currently be synthesized by
// update-verify-tests since the child note is in a different expansion than
// the parent diagnostic.
// RUN: not %target-swift-frontend-verify -verify-child-notes -verify-ignore-macro-note -typo-correction-limit 10 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested-child-note.swift 2>%t/output.txt
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-CHILD-NOTES-NESTED %s
// CHECK-CHILD-NOTES-NESTED: cannot synthesize a child note that lives in a different macro expansion

// Without -verify-child-notes we can synthesize the note directive in its
// own expansion directive.
// RUN: not %target-swift-frontend-verify -verify-ignore-macro-note -typo-correction-limit 10 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested-child-note.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -verify-ignore-macro-note -typo-correction-limit 10 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested-child-note.swift
// RUN: %diff %t/nested-child-note.swift %t/nested-non-child-note.swift.expected

//--- single.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    let a = x
}
""")
func foo() {}

//--- single.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    let a = x
}
""")
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func foo() {}

//--- multiple.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
func foo() {}

//--- multiple.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 2{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
// expected-expansion@+5:14{{
//   expected-note@1 2{{'x' declared here}}
//   expected-error@2{{cannot find 'a' in scope; did you mean 'x'?}}
//   expected-error@3{{cannot find 'b' in scope; did you mean 'x'?}}
// }}
func foo() {}

//--- existing.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
  //expected-expansion@+4:14{{
    //   expected-note@1   {{'x' declared here}}
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
  //}}
func foo() {}

//--- existing.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 2{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
  //expected-expansion@+5:14{{
    //   expected-note@1   2{{'x' declared here}}
    //   expected-error@2   {{cannot find 'a' in scope; did you mean 'x'?}}
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
  //}}
func foo() {}

//--- gone.swift
  //expected-expansion@+4:14{{
    //   expected-note@1   {{'x' declared here}}
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
  //}}
func foo() {}

//--- gone.swift.expected
func foo() {}

//--- wrong-location.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

  // expected-expansion@2:14{{
    //   expected-note@2 {{'x' declared here}}
    //   expected-error@3 {{cannot find 'b' in scope; did you mean 'x'?}}
  // }}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
func foo() {}

//--- wrong-location.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 2{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
// expected-expansion@+5:14{{
//   expected-note@1 2{{'x' declared here}}
//   expected-error@2{{cannot find 'a' in scope; did you mean 'x'?}}
//   expected-error@3{{cannot find 'b' in scope; did you mean 'x'?}}
// }}
func foo() {}

//--- nested.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
// hack to make this seem non-recursive
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func bar(_ y: Int) {
    @unstringifyPeer2(\"""
    func foo(_ x: Int) {
        a = 2
        b = x
    }
    \""")
    func foo() {}
    foo(y)
}
""")
func bar() {}

//--- nested.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
// hack to make this seem non-recursive
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 3{{in expansion of macro 'unstringifyPeer' on global function 'bar()' here}}
@unstringifyPeer("""
func bar(_ y: Int) {
    @unstringifyPeer2(\"""
    func foo(_ x: Int) {
        a = 2
        b = x
    }
    \""")
    func foo() {}
    foo(y)
}
""")
// expected-expansion@+10:14{{
//   expected-note@1 2{{did you mean 'y'?}}
//   expected-note@2 2{{in expansion of macro 'unstringifyPeer2' on local function 'foo()' here}}
//   expected-expansion@9:6{{
//     expected-note@1 2{{did you mean 'x'?}}
//     expected-error@2{{cannot find 'a' in scope}}
//     expected-error@3{{cannot find 'b' in scope}}
//   }}
//   expected-error@10{{argument passed to call that takes no arguments}}
// }}
func bar() {}

//--- multiple-at-same-location.swift
// Two peer macros attached to the same declaration expand at the same source
// location, so their diagnostics share one expansion source location but land
// in distinct expansion buffers. The verifier reports each nested diagnostic
// with the index of the expansion it came from ("in expansion N from here"),
// and update-verify-tests must route each into the matching sibling
// expansion directive rather than merging them or swapping them.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
func baz() {}

//--- multiple-at-same-location.swift.expected
// Two peer macros attached to the same declaration expand at the same source
// location, so their diagnostics share one expansion source location but land
// in distinct expansion buffers. The verifier reports each nested diagnostic
// with the index of the expansion it came from ("in expansion N from here"),
// and update-verify-tests must route each into the matching sibling
// expansion directive rather than merging them or swapping them.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
// expected-expansion@+6:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- sibling-multiple-diags.swift
// Each of the two sibling expansions emits multiple diagnostics; every one must
// be routed into its own sibling directive and accumulate there.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
    let b = x
}
""")
@unstringifyPeer2("""
func baz(_ y: String) {
    let c = y
    let d = y
}
""")
func baz() {}

//--- sibling-multiple-diags.swift.expected
// Each of the two sibling expansions emits multiple diagnostics; every one must
// be routed into its own sibling directive and accumulate there.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 2{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
    let b = x
}
""")
// expected-note@+1 2{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let c = y
    let d = y
}
""")
// expected-expansion@+8:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-warning@3{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+4:14{{
//   expected-warning@2{{initialization of immutable value 'c' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-warning@3{{initialization of immutable value 'd' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- sibling-existing.swift
// The sibling expansion directives already exist but are empty; their
// diagnostics must be routed into the existing directives by index instead of
// synthesizing new ones.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
// expected-expansion@+4:14{{
// }}
// expected-expansion@+2:14{{
// }}
func baz() {}

//--- sibling-existing.swift.expected
// The sibling expansion directives already exist but are empty; their
// diagnostics must be routed into the existing directives by index instead of
// synthesizing new ones.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
// expected-expansion@+6:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- sibling-three.swift
// Three sibling expansions at one location, to check that index routing scales
// past two.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer3(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
@unstringifyPeer3("""
func baz(_ z: Double) {
    let c = z
}
""")
func baz() {}

//--- sibling-three.swift.expected
// Three sibling expansions at one location, to check that index routing scales
// past two.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer3(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer3' on global function 'baz()' here}}
@unstringifyPeer3("""
func baz(_ z: Double) {
    let c = z
}
""")
// expected-expansion@+9:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+6:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'c' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- sibling-partial.swift
// One sibling expansion directive (index 0) is already present and correct; the
// other sibling's diagnostic is unexpected and must be added as a new directive
// placed after the existing one, not ahead of it.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- sibling-partial.swift.expected
// One sibling expansion directive (index 0) is already present and correct; the
// other sibling's diagnostic is unexpected and must be added as a new directive
// placed after the existing one, not ahead of it.
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'baz()' here}}
@unstringifyPeer("""
func baz(_ x: Int) {
    let a = x
}
""")
// expected-note@+1{{in expansion of macro 'unstringifyPeer2' on global function 'baz()' here}}
@unstringifyPeer2("""
func baz(_ y: String) {
    let b = y
}
""")
// expected-expansion@+6:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func baz() {}

//--- sibling-gone.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// The declaration these sibling directives targeted was deleted, so their
// '@+N' offsets now dangle past the end of the file.
// expected-expansion@+4:14{{
//   expected-warning@2{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
// }}
// expected-expansion@+2:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}

//--- sibling-gone.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// The declaration these sibling directives targeted was deleted, so their
// '@+N' offsets now dangle past the end of the file.

//--- unparsed.h
// CHECK-UNPARSED: no files updated: found diagnostics in unparsed files TMP_DIR{{/|\\}}unparsed.h
void foo(int len, int *p) __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

//--- module.modulemap
module UnparsedClang {
  header "unparsed.h"
  export *
}

//--- unparsed.swift
import UnparsedClang

func bar() {
  let a: CInt = 1
  var b: CInt = 13
  foo(a, &b)
}

//--- escaped.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    let a = "\\(x)"
    let b = "\\(x)"
}
""")
// NB: DiagnosticVerifier interprets "\\(x)" as "\(x)"
// expected-expansion@+3:30{{
//   expected-remark@2{{macro content: |let a = "\\(x)"|}}
// }}
func foo() { let _ = "\(2)" }

//--- escaped.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 6{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    let a = "\\(x)"
    let b = "\\(x)"
}
""")
// NB: DiagnosticVerifier interprets "\\(x)" as "\(x)"
// expected-expansion@+8:30{{
//   expected-remark@1{{macro content: |func foo(_ x: Int) {|}}
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-remark@2{{macro content: |    let a = "\\(x)"|}}
//   expected-warning@3{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-remark@3{{macro content: |    let b = "\\(x)"|}}
//   expected-remark@4{{macro content: |}|}}
// }}
func foo() { let _ = "\(2)" }

//--- empty-expansion-fill.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) -> Int {
    return x
}
""")
// expected-expansion@+2:30{{
// }}
func foo() { let _ = "\(2)" }

//--- empty-expansion-fill.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 3{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) -> Int {
    return x
}
""")
// expected-expansion@+5:30{{
//   expected-remark@1{{macro content: |func foo(_ x: Int) -> Int {|}}
//   expected-remark@2{{macro content: |    return x|}}
//   expected-remark@3{{macro content: |}|}}
// }}
func foo() { let _ = "\(2)" }

//--- empty-expansion-remove.swift
// expected-expansion@+2:14{{
// }}
func foo() {}

//--- empty-expansion-remove.swift.expected
func foo() {}

//--- empty-expansion-for-prefix.swift
// expected-expansion@+5:14{{
//   expected-asdf-remark@1{{asdf}}
//   expected-hjkl-remark@1{{hjkl}}
//   expected-hjkl-remark@2{{hjkl}}
// }}
func foo() {}

//--- empty-expansion-for-prefix.swift.expected
// expected-asdf-expansion@+7:14{{
//   expected-asdf-remark@1{{asdf}}
// }}
// expected-hjkl-expansion@+4:14{{
//   expected-hjkl-remark@1{{hjkl}}
//   expected-hjkl-remark@2{{hjkl}}
// }}
func foo() {}

//--- accept-child-note.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {}\nfunc foo(_ x: Int) {}")
func foo() {}
// expected-expansion@-1:14{{
//   expected-error@3{{invalid redeclaration of 'foo'}}
// }}

//--- accept-child-note.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {}\nfunc foo(_ x: Int) {}")
func foo() {}
// expected-expansion@-1:14{{
//   expected-error@3{{invalid redeclaration of 'foo'}} {{children:
//     expected-note@1{{'foo' previously declared here}}
//   }}
// }}

//--- accept-typo-child-note.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func f(alpha: Int, alpaca: Int) {\n_ = alpar\n}")
func f() {}

//--- accept-typo-child-note.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func f(alpha: Int, alpaca: Int) {\n_ = alpar\n}")
// expected-expansion@+6:12{{
//   expected-error@2{{cannot find 'alpar' in scope}} {{children:
//     expected-note@1{{did you mean 'alpha'?}}
//     expected-note@1{{did you mean 'alpaca'?}}
//   }}
// }}
func f() {}

//--- reject-child-notes.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {\nqqq()\n}")
func foo() {}
// expected-expansion@-1:14{{
//   expected-error@2{{cannot find 'qqq' in scope}}
// }}

//--- reject-child-note-inward.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// The macro peer generates `func foo(_ x: Int)` inside the expansion; the outer
// redeclaration below is the one diagnosed, so the parent error is in this file
// but its "previously declared here" child note points inside the expansion.
@unstringifyPeer("func foo(_ x: Int) {}")
func foo() {}
func foo(_ x: Int) {}

//--- reject-child-note-cross.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// Each peer generates `func foo(_ x: Int)` inside its own expansion; the two
// generated declarations conflict, so the redeclaration error is in the second
// expansion while its "previously declared here" child note is in the first.
@unstringifyPeer("func foo(_ x: Int) {}")
func foo(_ a: Bool) {}

@unstringifyPeer("func foo(_ x: Int) {}")
func foo(_ b: String) {}

//--- reject-child-note-cross-matched.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// As above, but with a parent matcher.
@unstringifyPeer("func foo(_ x: Int) {}")
func foo(_ a: Bool) {}

@unstringifyPeer("func foo(_ x: Int) {}")
func foo(_ b: String) {}
// expected-expansion@-1:25{{
//   expected-error@1{{invalid redeclaration of 'foo'}}
// }}

//--- nested-child-note.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
// hack to make this seem non-recursive
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func bar(_ y: Int) {
    @unstringifyPeer2(\"""
    func foo(_ x: Int) {
        a = 2
        b = x
    }
    \""")
    func foo() {}
    foo(y)
}
""")
func bar() {}

//--- nested-non-child-note.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
// hack to make this seem non-recursive
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func bar(_ y: Int) {
    @unstringifyPeer2(\"""
    func foo(_ x: Int) {
        a = 2
        b = x
    }
    \""")
    func foo() {}
    foo(y)
}
""")
// expected-expansion@+9:14{{
//   expected-note@1 2{{did you mean 'y'?}}
//   expected-expansion@9:6{{
//     expected-note@1 2{{did you mean 'x'?}}
//     expected-error@2{{cannot find 'a' in scope}}
//     expected-error@3{{cannot find 'b' in scope}}
//   }}
//   expected-error@10{{argument passed to call that takes no arguments}}
// }}
func bar() {}

