// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' >%t/a.swift
// RUN: echo 'public func b() { }' >%t/b.swift
// RUN: echo 'public func main() {a(); b()}' >%t/main.swift

// Test printing to stdout
// RUN: %target-swift-frontend -dump-ast -primary-file %t/a.swift %t/b.swift %t/main.swift -module-name main -o - 2>&1 | %FileCheck -check-prefix A-AST %s
// RUN: %target-swift-frontend -dump-ast %t/a.swift -primary-file %t/b.swift %t/main.swift -module-name main -o - 2>&1 | %FileCheck -check-prefix B-AST %s
// RUN: %target-swift-frontend -dump-ast %t/a.swift %t/b.swift -primary-file %t/main.swift -module-name main -o - 2>&1 | %FileCheck -check-prefix MAIN-AST %s

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
// A-AST: (source_file "{{.*}}a.swift"
// A-AST-NEXT: (func_decl
// A-AST-SAME: "a()"

// Check b.swift's AST
// B-AST: (source_file "{{[^)]*}}b.swift"
// B-AST-NEXT: (func_decl
// B-AST-SAME: "b()"

// Check main.swift's AST
// MAIN-AST: (source_file "{{.*}}main.swift"
// MAIN-AST-NEXT: (func_decl {{[^)]*}} "main()" interface type='() -> ()' access=public
// MAIN-AST: (call_expr
// MAIN-AST-NEXT: decl=main.(file).a()@{{.*}}a.swift
// MAIN-AST-NEXT: (tuple_expr type='()'
// MAIN-AST: (call_expr
// MAIN-AST-NEXT: decl=main.(file).b()@{{.*}}b.swift
// MAIN-AST-NEXT: (tuple_expr type='()'
