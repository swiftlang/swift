// Tests -serialize-diagnostics=<format>.

// RUN: %empty-directory(%t)

// '=dia' produces the same file as the bare flag.
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics=dia -serialize-diagnostics-path %t/explicit.dia %s
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics -serialize-diagnostics-path %t/implicit.dia %s
// RUN: diff %t/explicit.dia %t/implicit.dia

// The last of the bare flag, which means 'dia', and a format wins.
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics=sarif -serialize-diagnostics -serialize-diagnostics-path %t/last.dia %s
// RUN: diff %t/explicit.dia %t/last.dia

// The format flag enables serialization on its own, and derives the path.
// RUN: cp %s %t/derived.swift
// RUN: cd %t && %target-swift-frontend -typecheck -serialize-diagnostics=dia derived.swift
// RUN: test -f %t/derived.dia

// An unknown format is an error wherever it appears.
// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=bogus -serialize-diagnostics-path %t/bogus.dia %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=bogus -serialize-diagnostics -serialize-diagnostics-path %t/bogus.dia %s 2>&1 | %FileCheck %s
// CHECK: error: unsupported argument 'bogus' to option '-serialize-diagnostics='

func f() { let unused = 1 }
