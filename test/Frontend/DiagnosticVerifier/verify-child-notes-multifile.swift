// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainA.swift %t/otherA.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainB.swift %t/otherB.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainC.swift %t/otherC.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainD.swift %t/otherD.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainE.swift %t/otherE.swift

//--- otherA.swift
struct CrossBuf {} // #crossbuf-orig

//--- mainA.swift
struct CrossBuf {}
// expected-error@-1 {{invalid redeclaration of 'CrossBuf'}} {{children:
//   expected-note@#crossbuf-orig {{'CrossBuf' previously declared here}}
// }}


//--- otherB.swift
struct CrossBuf {} // #crossbuf-orig

//--- mainB.swift
struct CrossBuf {} // #unused-tag
// expected-error@-1 {{invalid redeclaration of 'CrossBuf'}} {{children:
//   expected-note@#crossbuf-orig {{'CrossBuf' previously declared here}}
// }}


//--- otherC.swift
struct CrossBuf {} // #crossbuf-orig

//--- mainC.swift
struct CrossBuf {} // #crossbuf-redecl
// expected-error@#crossbuf-redecl {{invalid redeclaration of 'CrossBuf'}} {{children:
//   expected-note@#crossbuf-orig {{'CrossBuf' previously declared here}}
// }}


//--- otherD.swift
struct CrossBuf {} // #crossbuf-orig
// expected-error@#crossbuf-redecl {{invalid redeclaration of 'CrossBuf'}} {{children:
//   expected-note@#crossbuf-orig {{'CrossBuf' previously declared here}}
// }}

//--- mainD.swift
struct CrossBuf {} // #crossbuf-redecl


//--- otherE.swift
struct CrossBuf {}
// expected-error@#crossbuf-redecl {{invalid redeclaration of 'CrossBuf'}} {{children:
//   expected-note@-2 {{'CrossBuf' previously declared here}}
// }}

//--- mainE.swift
struct CrossBuf {} // #crossbuf-redecl
