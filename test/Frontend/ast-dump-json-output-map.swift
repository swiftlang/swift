// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' >%t/a.swift
// RUN: echo 'public func b() { }' >%t/b.swift
// RUN: echo 'public func main() {a(); b()}' >%t/main.swift
// RUN: echo '{"%/t/a.swift": {"ast-dump": "%/t/a.ast"}, "%/t/b.swift": {"ast-dump": "%/t/b.ast"}, "%/t/main.swift": {"ast-dump": "%/t/main.ast"}}' > %t/outputs.json

// RUN: %swiftc_driver -dump-ast -dump-ast-format json -output-file-map %t/outputs.json %t/a.swift %t/b.swift %t/main.swift -module-name main
// RUN: %FileCheck -check-prefix A-AST %s < %t/a.ast
// RUN: %FileCheck -check-prefix B-AST %s < %t/b.ast
// RUN: %FileCheck -check-prefix MAIN-AST %s < %t/main.ast


// Check a.swift's AST
// A-AST: "file":"{{[^"]+}}/a.swift"
// A-AST-SAME: "kind":"FuncDecl"
// A-AST-SAME: "usr":"s:4main1ayyF"


// Check b.swift's AST
// B-AST: "file":"{{[^"]+}}/b.swift"
// B-AST-SAME: "kind":"FuncDecl"
// B-AST-SAME: "usr":"s:4main1byyF"


// Check main.swift's AST
// MAIN-AST: "file":"{{[^"]+}}/main.swift"
// MAIN-AST-SAME: "kind":"FuncDecl"
// MAIN-AST-SAME: "usr":"s:4mainAAyyF"

// MAIN-AST-SAME: "kind":"CallExpr"
// MAIN-AST-SAME: "declUSR":"s:4main1ayyF"
// MAIN-AST-SAME: "kind":"CallExpr"
// MAIN-AST-SAME: "declUSR":"s:4main1byyF"
