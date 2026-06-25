// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: not %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainA.swift %t/otherA.swift 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not warning: --implicit-check-not note: --check-prefix A

// RUN: not %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainB.swift %t/otherB.swift 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not warning: --implicit-check-not note: --check-prefix B

// RUN: not %target-swift-frontend -typecheck -verify -verify-child-notes %t/mainC.swift %t/otherC.swift 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not warning: --implicit-check-not note: --check-prefix C

//--- otherA.swift
struct CrossBuf {} // #crossbuf-orig

//--- mainA.swift
struct CrossBuf {}
// A: [[@LINE+3]]:{{[0-9]+}}: error: expected note not produced
// A: {{.*}}otherA.swift:[[@LINE-5]]:{{[0-9]+}}: error: unexpected child note produced: 'CrossBuf' previously declared here
// A: [[@LINE+1]]:{{[0-9]+}}: note: for parent matched here
// expected-error@-4 {{invalid redeclaration of 'CrossBuf'}} {{children: expected-note@#crossbuf-orig {{wrong message}} }}

//--- otherB.swift
struct CrossBuf1 {} // #1
struct CrossBuf2 {} // #2

//--- mainB.swift
struct CrossBuf1 {}
// expected-error@-1 {{invalid redeclaration of 'CrossBuf1'}} {{children: expected-note@#1 {{wrong message}} }}
// B: mainB.swift:[[@LINE-1]]:{{[0-9]+}}: error: expected note not produced
// B: otherB.swift:[[@LINE-7]]:{{[0-9]+}}: error: unexpected child note produced: 'CrossBuf1' previously declared here
// B: mainB.swift:[[@LINE-3]]:{{[0-9]+}}: note: for parent matched here
struct CrossBuf2 {}
// B: otherB.swift:[[@LINE-9]]:{{[0-9]+}}: error: unexpected child note produced: 'CrossBuf2' previously declared here
// B: mainB.swift:[[@LINE+1]]:{{[0-9]+}}: note: for parent matched here
// expected-error@-3 {{invalid redeclaration of 'CrossBuf2'}}

//--- mainC.swift
struct CrossBuf {}
// C: mainC.swift:[[@LINE-1]]:{{[0-9]+}}: error: unexpected error produced: invalid redeclaration of 'CrossBuf'

//--- otherC.swift
struct CrossBuf {}
// C: otherC.swift:[[@LINE-1]]:{{[0-9]+}}: note: with child note: 'CrossBuf' previously declared here

