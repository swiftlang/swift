// Member lists are built on demand: nothing asks for the members of a type in
// a secondary file that is not looked into, so a syntax error inside one is
// never reported.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -enable-experimental-feature ParserASTGen -primary-file %t/a.swift %t/b.swift

// RUN: not %target-swift-frontend -typecheck -enable-experimental-feature ParserASTGen -diagnostic-style=llvm -primary-file %t/c.swift %t/b.swift 2>&1 | %FileCheck %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

//--- a.swift
func use(_ x: Int) {}

//--- b.swift
struct Unused {
  var a: Int = ,
}

//--- c.swift
func use(_ x: Unused) {
  _ = x.a
}

// CHECK: error: expected expression in variable
