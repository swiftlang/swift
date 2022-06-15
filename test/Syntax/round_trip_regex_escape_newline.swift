// RUN: %empty-directory(%t)
// RUN: not %swift-syntax-test -input-source-filename %s -parse-gen -fail-on-parse-error > %t/afterRoundtrip.swift 2> %t/errors.swift
// RUN: diff -u %s %t/afterRoundtrip.swift
// RUN: cat %t/errors.swift | %FileCheck %s

// Escaping newlines is not supported
_ = /\
/

// CHECK: 7:7: error: unterminated regex literal
