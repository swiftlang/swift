// RUN: %target-swift-ide-test -print-expr-type -source-filename %S/Inputs/ExprTypeFiltered.swift -swift-version 5 -module-name filtered -usr-filter 's:8filtered9ProtEmptyP' | %FileCheck %s -check-prefix=EMPTY
// RUN: %target-swift-ide-test -print-expr-type -source-filename %S/Inputs/ExprTypeFiltered.swift -swift-version 5 -module-name filtered -usr-filter 's:8filtered4ProtP' | %FileCheck %s -check-prefix=PROTO
// RUN: %target-swift-ide-test -print-expr-type -source-filename %S/Inputs/ExprTypeFiltered.swift -swift-version 5 -module-name filtered -usr-filter 's:8filtered5Prot1P' | %FileCheck %s -check-prefix=PROTO1

// EMPTY: class Clas: Prot {
// EMPTY:   var value: Clas { return self }
// EMPTY:   func getValue() -> Clas { return self }
// EMPTY: }
// EMPTY: struct Stru: Prot, Prot1 {
// EMPTY:   var value: Stru { return self }
// EMPTY:   func getValue() -> Stru { return self }
// EMPTY: }

// PROTO: class Clas: Prot {
// PROTO:   var value: Clas { return <expr type:"Clas">self</expr> }
// PROTO:   func getValue() -> Clas { return <expr type:"Clas">self</expr> }
// PROTO: }
// PROTO: struct Stru: Prot, Prot1 {
// PROTO:   var value: Stru { return <expr type:"Stru">self</expr> }
// PROTO:   func getValue() -> Stru { return <expr type:"Stru">self</expr> }
// PROTO: }

// PROTO1: class Clas: Prot {
// PROTO1:   var value: Clas { return self }
// PROTO1:   func getValue() -> Clas { return self }
// PROTO1: }
// PROTO1: struct Stru: Prot, Prot1 {
// PROTO1:   var value: Stru { return <expr type:"Stru">self</expr> }
// PROTO1:   func getValue() -> Stru { return <expr type:"Stru">self</expr> }
// PROTO1: }