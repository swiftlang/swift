// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## State 1' == \
// RUN:   -req=cursor -pos=5:7 %t/file.swift -- %t/file.swift == \
// RUN:   -shell -- echo '## State 2' == \
// RUN:   -shell -- cp %t/State2.swift %t/file.swift == \
// RUN:   -req=cursor -pos=6:7 %t/file.swift -- %t/file.swift == \
// RUN:   -shell -- echo '## State 3' == \
// RUN:   -shell -- cp %t/State3.swift %t/file.swift == \
// RUN:   -req=cursor -pos=4:7 %t/file.swift -- %t/file.swift == \
// RUN:   -shell -- echo '## State 4' == \
// RUN:   -shell -- cp %t/State4.swift %t/file.swift == \
// RUN:   -req=cursor -pos=4:7 %t/file.swift -- %t/file.swift == \
// RUN:   -shell -- echo '## State 5' == \
// RUN:   -shell -- cp %t/State5.swift %t/file.swift == \
// RUN:   -req=cursor -pos=4:7 %t/file.swift -- %t/file.swift == \
// RUN:   -shell -- echo '## State 6' == \
// RUN:   -shell -- cp %t/State6.swift %t/file.swift == \
// RUN:   -req=cursor -pos=4:7 %t/file.swift -- %t/file.swift > %t/response.txt
// RUN: %FileCheck %s < %t/response.txt

// CHECK-LABEL: ## State 1
// CHECK: source.lang.swift.decl.var.local (5:7-5:18)
// CHECK: <Declaration>let inFunctionB: <Type usr="s:SS">String</Type></Declaration>
// CHECK: DID REUSE AST CONTEXT: 0
// CHECK-LABEL: ## State 2
// CHECK: source.lang.swift.decl.var.local (6:7-6:18)
// CHECK: <Declaration>let inFunctionB: <Type usr="s:SS">String</Type></Declaration>
// CHECK: DID REUSE AST CONTEXT: 1
// CHECK-LABEL: ## State 3
// CHECK: source.lang.swift.decl.var.local (4:7-4:18)
// CHECK: <Declaration>let inFunctionB: <Type usr="s:SS">String</Type></Declaration>
// CHECK: DID REUSE AST CONTEXT: 1
// CHECK-LABEL: ## State 4
// CHECK: source.lang.swift.decl.var.local (4:7-4:16)
// CHECK: <Declaration>let myNewName: <Type usr="s:SS">String</Type></Declaration>
// CHECK: DID REUSE AST CONTEXT: 1
// CHECK-LABEL: ## State 5
// CHECK: source.lang.swift.decl.var.local (4:7-4:16)
// CHECK: <Declaration>let myNewName: <Type usr="s:SS">String</Type></Declaration>
// CHECK: DID REUSE AST CONTEXT: 0
// CHECK-LABEL: ## State 6
// CHECK: source.lang.swift.decl.var.local (4:7-4:16)
// CHECK: <Declaration>let myNewName: <Type usr="s:Si">Int</Type></Declaration>
// CHECK: DID REUSE AST CONTEXT: 1

//--- file.swift
func unrelated() {}

func foo() {
  let inFunctionA = 1
  let inFunctionB = "hi"
}

//--- State2.swift
func unrelated() {}

func foo() {
  let newlyAddedMember = 3
  let inFunctionA = 1
  let inFunctionB = "hi"
}

//--- State3.swift
func unrelated() {}

func foo() {
  let inFunctionB = "hi"
}

//--- State4.swift
func unrelated() {}

func foo() {
  let myNewName = "hi"
}

//--- State5.swift
func unrelated() {}

func foo(param: Int) {
  let myNewName = "hi"
}

//--- State6.swift
func unrelated() {}

func foo(param: Int) {
  let myNewName = 7
}
