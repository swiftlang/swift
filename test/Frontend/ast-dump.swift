// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' >%t/a.swift
// RUN: echo 'public func b() { }' >%t/b.swift
// RUN: echo 'public func main() {a(); b()}' >%t/main.swift

// Test printing to stdout
// RUN: %target-swift-frontend -dump-ast -primary-file %t/a.swift %t/b.swift %t/main.swift -module-name main -o - 2>%t/a.swift.stderr | %FileCheck -check-prefix A-AST %s
// RUN: %target-swift-frontend -dump-ast %t/a.swift -primary-file %t/b.swift %t/main.swift -module-name main -o - 2>%t/b.swift.stderr | %FileCheck -check-prefix B-AST %s
// RUN: %target-swift-frontend -dump-ast %t/a.swift %t/b.swift -primary-file %t/main.swift -module-name main -o - 2>%t/main.swift.stderr | %FileCheck -check-prefix MAIN-AST %s

// Test printing to files
// RUN: %target-swift-frontend -dump-ast -primary-file %t/a.swift %t/b.swift %t/main.swift -module-name main -o %t/a.ast
// RUN: %FileCheck -check-prefix A-AST %s < %t/a.ast
// RUN: %target-swift-frontend -dump-ast %t/a.swift -primary-file %t/b.swift %t/main.swift -module-name main -o %t/b.ast
// RUN: %FileCheck -check-prefix B-AST %s < %t/b.ast
// RUN: %target-swift-frontend -dump-ast %t/a.swift %t/b.swift -primary-file %t/main.swift -module-name main -o %t/main.ast
// RUN: %FileCheck -check-prefix MAIN-AST %s < %t/main.ast

// Test batch mode
// RUN: %target-swift-frontend -dump-ast -primary-file %t/a.swift -primary-file %t/b.swift -primary-file %t/main.swift -module-name main -o %t/a_batch.ast -o %t/b_batch.ast -o %t/main_batch.ast
// RUN: %FileCheck -check-prefix A-AST %s < %t/a_batch.ast
// RUN: %FileCheck -check-prefix B-AST %s < %t/b_batch.ast
// RUN: %FileCheck -check-prefix MAIN-AST %s < %t/main_batch.ast


// Check a.swift's AST
// A-AST: (source_file
// A-AST-SAME: a.swift

// A-AST-NEXT: (func_decl
// A-AST-SAME: a()


// Check b.swift's AST
// B-AST: (source_file
// B-AST-SAME: b.swift

// B-AST-NEXT: (func_decl
// B-AST-SAME: b()


// Check main.swift's AST
// MAIN-AST: (source_file
// MAIN-AST-SAME: main.swift

// MAIN-AST: (func_decl
// MAIN-AST-SAME: main()

// MAIN-AST: (call_expr
// MAIN-AST-NEXT: a()

// MAIN-AST: (call_expr
// MAIN-AST-NEXT: b()
