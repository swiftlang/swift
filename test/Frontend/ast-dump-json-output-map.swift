// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' > %t/a.swift
// RUN: echo 'public func b() { }' > %t/b.swift
// RUN: echo 'public func main() {a(); b()}' > %t/main.swift
// RUN: echo "{\"%/t/a.swift\": {\"ast-dump\": \"%/t/a.ast\"}, \"%/t/b.swift\": {\"ast-dump\": \"%/t/b.ast\"}, \"%/t/main.swift\": {\"ast-dump\": \"%/t/main.ast\"}}" > %t/outputs.json

// RUN: %target-build-swift -dump-ast -dump-ast-format json -output-file-map %t/outputs.json %/t/a.swift %/t/b.swift %/t/main.swift -module-name main
// RUN: %FileCheck -check-prefix A-AST %s < %t/a.ast
// RUN: %FileCheck -check-prefix B-AST %s < %t/b.ast
// RUN: %FileCheck -check-prefix MAIN-AST %s < %t/main.ast


// Check a.swift's AST
// A-AST: "filename":"{{[^"]+}}a.swift"
// A-AST-SAME: "_kind":"func_decl"
// A-AST-SAME: "usr":"s:4main1ayyF"


// Check b.swift's AST
// B-AST: "filename":"{{[^"]+}}b.swift"
// B-AST-SAME: "_kind":"func_decl"
// B-AST-SAME: "usr":"s:4main1byyF"


// Check main.swift's AST
// MAIN-AST: "filename":"{{[^"]+}}main.swift"
// MAIN-AST-SAME: "_kind":"func_decl"
// MAIN-AST-SAME: "usr":"s:4mainAAyyF"

// MAIN-AST-SAME: "_kind":"call_expr"
// MAIN-AST-SAME: "decl_usr":"s:4main1ayyF"
// MAIN-AST-SAME: "_kind":"call_expr"
// MAIN-AST-SAME: "decl_usr":"s:4main1byyF"
