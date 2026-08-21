// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// --- child-notes-merge: Children with relative offsets merge -------------
// RUN: %minimize-verify-tests %t/child-notes-merge.swift
// RUN: %diff %t/child-notes-merge.swift %t/child-notes-merge.swift.expected

// --- child-notes-differ: Different child note text stays separate --------
// RUN: %minimize-verify-tests %t/child-notes-differ.swift
// RUN: %diff %t/child-notes-differ.swift %t/child-notes-differ.swift.expected

// --- child-notes-mixed: One diag has children, the other does not --------
// RUN: %minimize-verify-tests %t/child-notes-mixed.swift
// RUN: %diff %t/child-notes-mixed.swift %t/child-notes-mixed.swift.expected

// --- child-notes-no-children-merge: Same message, no children, merges ---
// RUN: %minimize-verify-tests %t/child-notes-no-children-merge.swift
// RUN: %diff %t/child-notes-no-children-merge.swift %t/child-notes-no-children-merge.swift.expected

// --- child-notes-multiline: Multi-line children blocks merge ------------
// RUN: %minimize-verify-tests %t/child-notes-multiline.swift
// RUN: %diff %t/child-notes-multiline.swift %t/child-notes-multiline.swift.expected

// --- child-notes-in-expansion: Children on diags inside expansions -------
// RUN: %minimize-verify-tests %t/child-notes-in-expansion.swift
// RUN: %diff %t/child-notes-in-expansion.swift %t/child-notes-in-expansion.swift.expected

//--- child-notes-merge.swift
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct S {}
struct S {}
// expected-a-error @-1 {{invalid redeclaration of 'S'}} {{children: expected-note@-2 {{'S' previously declared here}} }}
// expected-b-error @-2 {{invalid redeclaration of 'S'}} {{children: expected-note@-3 {{'S' previously declared here}} }}
//--- child-notes-merge.swift.expected
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct S {}
struct S {}
// expected-error @-1 {{invalid redeclaration of 'S'}} {{children: expected-note@-2 {{'S' previously declared here}} }}
//--- child-notes-differ.swift
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct T {}
struct T {}
// expected-a-error @-1 {{invalid redeclaration of 'T'}} {{children: expected-note@-2 {{'T' previously declared here}} }}
// expected-b-error @-2 {{invalid redeclaration of 'T'}} {{children: expected-note@-3 {{other note}} }}
//--- child-notes-differ.swift.expected
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct T {}
struct T {}
// expected-a-error @-1 {{invalid redeclaration of 'T'}} {{children: expected-note@-2 {{'T' previously declared here}} }}
// expected-b-error @-2 {{invalid redeclaration of 'T'}} {{children: expected-note@-3 {{other note}} }}
//--- child-notes-mixed.swift
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct U {}
struct U {}
// expected-a-error @-1 {{invalid redeclaration of 'U'}} {{children: expected-note@-2 {{'U' previously declared here}} }}
// expected-b-error @-2 {{invalid redeclaration of 'U'}}
//--- child-notes-mixed.swift.expected
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct U {}
struct U {}
// expected-a-error @-1 {{invalid redeclaration of 'U'}} {{children: expected-note@-2 {{'U' previously declared here}} }}
// expected-b-error @-2 {{invalid redeclaration of 'U'}}
//--- child-notes-no-children-merge.swift
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

func foo(_ x: Int) {}
foo("hello") // expected-a-error {{cannot convert value of type 'String' to expected argument type 'Int'}}
// expected-b-error @-1 {{cannot convert value of type 'String' to expected argument type 'Int'}}
//--- child-notes-no-children-merge.swift.expected
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

func foo(_ x: Int) {}
foo("hello") // expected-error {{cannot convert value of type 'String' to expected argument type 'Int'}}
//--- child-notes-multiline.swift
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct V {}
struct V {}
// expected-a-error @-1 {{invalid redeclaration of 'V'}} {{children:
//   expected-note@-3 {{'V' previously declared here}}
// }}
// expected-b-error @-4 {{invalid redeclaration of 'V'}} {{children:
//   expected-note@-6 {{'V' previously declared here}}
// }}
//--- child-notes-multiline.swift.expected
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

struct V {}
struct V {}
// expected-error @-1 {{invalid redeclaration of 'V'}} {{children:
//   expected-note@-3 {{'V' previously declared here}}
// }}
//--- child-notes-in-expansion.swift
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

@freestanding(expression)
macro myMacro() = #externalMacro(module: "M", type: "T")
let x = #myMacro
// expected-a-expansion@-1:9{{
//   expected-a-error@1 {{redecl}} {{children: expected-note@2 {{prev}} }}
//   expected-a-error@3 {{other}} {{children: expected-note@4 {{different a}} }}
//   expected-a-warning@2 {{unused}}
// }}
// expected-b-expansion@-6:9{{
//   expected-b-error@1 {{redecl}} {{children: expected-note@2 {{prev}} }}
//   expected-b-error@3 {{other}} {{children: expected-note@4 {{different b}} }}
//   expected-b-warning@2 {{unused}}
// }}
//--- child-notes-in-expansion.swift.expected
// RUN: true -verify -verify-child-notes -verify-additional-prefix a-
// RUN: true -verify -verify-child-notes -verify-additional-prefix b-

@freestanding(expression)
macro myMacro() = #externalMacro(module: "M", type: "T")
let x = #myMacro
// expected-expansion@-1:9{{
//   expected-error@1 {{redecl}} {{children: expected-note@2 {{prev}} }}
//   expected-warning@2 {{unused}}
//   expected-a-error@3 {{other}} {{children: expected-note@4 {{different a}} }}
//   expected-b-error@3 {{other}} {{children: expected-note@4 {{different b}} }}
// }}
