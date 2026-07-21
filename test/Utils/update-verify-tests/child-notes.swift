// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The second RUN of each pair re-verifies the updated file to prove the tool
// produced a file that actually passes verification; the %diff locks the exact
// formatting against the .expected golden.

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/add.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/add.swift
// RUN: %diff %t/add.swift %t/add.swift.expected

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/new-parent.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/new-parent.swift
// RUN: %diff %t/new-parent.swift %t/new-parent.swift.expected

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/fix.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/fix.swift
// RUN: %diff %t/fix.swift %t/fix.swift.expected

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/partial.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/partial.swift
// RUN: %diff %t/partial.swift %t/partial.swift.expected

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/remove.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/remove.swift
// RUN: %diff %t/remove.swift %t/remove.swift.expected

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/no-notes.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/no-notes.swift
// RUN: %diff %t/no-notes.swift %t/no-notes.swift.expected

// RUN: not %target-swift-frontend-verify -verify-child-notes -typecheck %t/note-relocated.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -verify-child-notes -typecheck %t/note-relocated.swift
// RUN: %diff %t/note-relocated.swift %t/note-relocated.swift.expected

// RUN: not %target-swift-frontend-verify -typecheck %t/flag-disabled.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/flag-disabled.swift
// RUN: %diff %t/flag-disabled.swift %t/flag-disabled.swift.expected

//--- add.swift
struct A {}
struct A {} // expected-error {{invalid redeclaration of 'A'}}

//--- add.swift.expected
struct A {}
struct A {} // expected-error {{invalid redeclaration of 'A'}} {{children:
//   expected-note@-2{{'A' previously declared here}}
// }}

//--- new-parent.swift
struct B {}
struct B {}

//--- new-parent.swift.expected
struct B {}
// expected-error@+3{{invalid redeclaration of 'B'}} {{children:
//   expected-note@-2{{'B' previously declared here}}
// }}
struct B {}

//--- fix.swift
struct C {}
struct C {} // expected-error {{invalid redeclaration of 'C'}} {{children:
//   expected-note@-2 {{wrong message}}
// }}

//--- fix.swift.expected
struct C {}
struct C {} // expected-error {{invalid redeclaration of 'C'}} {{children:
//   expected-note@-2{{'C' previously declared here}}
// }}

//--- partial.swift
struct D {}
struct D {} // expected-error {{invalid redeclaration of 'D'}} {{children:
//   expected-note@-2{{'D' previously declared here}}
//   expected-note@-3{{stale extra note}}
// }}

//--- partial.swift.expected
struct D {}
struct D {} // expected-error {{invalid redeclaration of 'D'}} {{children:
//   expected-note@-2{{'D' previously declared here}}
// }}

//--- remove.swift
struct E {}
struct F {} // expected-error {{invalid redeclaration of 'E'}} {{children:
//   expected-note@-2 {{'E' previously declared here}}
// }}

//--- remove.swift.expected
struct E {}
struct F {}

//--- no-notes.swift
let x: Int = "hello" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}} {{children:
//   expected-note@-1 {{this note does not exist}}
// }}

//--- no-notes.swift.expected
let x: Int = "hello" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}

//--- note-relocated.swift
struct G {}
// expected-note@-1 {{'G' previously declared here}}
struct G {} // expected-error {{invalid redeclaration of 'G'}}
undefinedName()

//--- note-relocated.swift.expected
struct G {}
struct G {} // expected-error {{invalid redeclaration of 'G'}} {{children:
//   expected-note@-2{{'G' previously declared here}}
// }}
// expected-error@+1{{cannot find 'undefinedName' in scope}}
undefinedName()

//--- flag-disabled.swift
struct H {}
struct H {} // expected-error {{invalid redeclaration of 'H'}} {{children:
//   expected-note@-2 {{'H' previously declared here}}
// }}

//--- flag-disabled.swift.expected
// expected-note@+1{{'H' previously declared here}}
struct H {}
struct H {} // expected-error {{invalid redeclaration of 'H'}}

