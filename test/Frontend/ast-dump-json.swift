// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' >%t/a.swift
// RUN: echo 'public func b() { }' >%t/b.swift
// RUN: echo 'public func main() {a(); b()}' >%t/main.swift

// RUN: %target-swift-frontend -dump-ast -dump-ast-format json -primary-file %t/a.swift %t/b.swift %t/main.swift -module-name main -o - 2>%t/a.swift.stderr | %FileCheck -check-prefix A-AST %s
// RUN: %target-swift-frontend -dump-ast -dump-ast-format json %t/a.swift -primary-file %t/b.swift %t/main.swift -module-name main -o - 2>%t/b.swift.stderr | %FileCheck -check-prefix B-AST %s
// RUN: %target-swift-frontend -dump-ast -dump-ast-format json %t/a.swift %t/b.swift -primary-file %t/main.swift -module-name main -o - 2>%t/main.swift.stderr | %FileCheck -check-prefix MAIN-AST %s

// Check a.swift's AST
// A-AST: "filename":"{{[^"]+}}/a.swift"
// A-AST-SAME: "_kind":"func_decl"
// A-AST-SAME: "usr":"s:4main1ayyF"


// Check b.swift's AST
// B-AST: "filename":"{{[^"]+}}/b.swift"
// B-AST-SAME: "_kind":"func_decl"
// B-AST-SAME: "usr":"s:4main1byyF"


// Check main.swift's AST
// MAIN-AST: "filename":"{{[^"]+}}/main.swift"
// MAIN-AST-SAME: "_kind":"func_decl"
// MAIN-AST-SAME: "usr":"s:4mainAAyyF"

// MAIN-AST-SAME: "_kind":"call_expr"
// MAIN-AST-SAME: "decl_usr":"s:4main1ayyF"
// MAIN-AST-SAME: "_kind":"call_expr"
// MAIN-AST-SAME: "decl_usr":"s:4main1byyF"
