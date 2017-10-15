func test() {
  // stmt
  ()
  let foo: // type
  for x in  { } // foreach.sequence
}

// ===--- Errors

// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error1.json | %FileCheck %s -check-prefix=ERROR-MISSING-RESULTS
// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error2.json | %FileCheck %s -check-prefix=ERROR-MISSING-RESULTS
// ERROR-MISSING-RESULTS: missing 'key.results'

// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error3.json | %FileCheck %s -check-prefix=ERROR-MISSING-NAME
// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error4.json | %FileCheck %s -check-prefix=ERROR-MISSING-NAME
// ERROR-MISSING-NAME: missing 'key.name'

// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error5.json | %FileCheck %s -check-prefix=ERROR-MISSING-KIND
// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error6.json | %FileCheck %s -check-prefix=ERROR-MISSING-KIND
// ERROR-MISSING-KIND: missing 'key.kind'

// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error7.json | %FileCheck %s -check-prefix=ERROR-MISSING-CONTEXT
// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error8.json | %FileCheck %s -check-prefix=ERROR-MISSING-CONTEXT
// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error9.json | %FileCheck %s -check-prefix=ERROR-MISSING-CONTEXT
// ERROR-MISSING-CONTEXT: missing 'key.context'

// RUN: not %sourcekitd-test -json-request-path %S/Inputs/custom-completion/error10.json | %FileCheck %s -check-prefix=ERROR-INVALID-CONTEXT
// ERROR-INVALID-CONTEXT: invalid value for 'key.context'


// ===--- Custom completions

// RUN: %sourcekitd-test -json-request-path %S/Inputs/custom-completion/custom.json == \
// RUN:     -req=complete.open -pos=2:1 %s -- %s | %FileCheck %s -check-prefix=STMT

// STMT-NOT: myuid
// STMT: key.kind: myuid.customExprOrStmtOrType
// STMT-NEXT: key.name: "customExprOrStmtOrType"
// STMT-NOT: myuid
// STMT: key.kind: myuid.customStmt
// STMT-NEXT: key.name: "customStmt"
// STMT-NOT: myuid

// RUN: %sourcekitd-test -json-request-path %S/Inputs/custom-completion/custom.json == \
// RUN:     -req=complete.open -pos=3:4 %s -- %s | %FileCheck %s -check-prefix=EXPR

// EXPR-NOT: myuid
// EXPR: key.kind: myuid.customExpr
// EXPR-NEXT: key.name: "customExpr"
// EXPR-NOT: myuid
// EXPR: key.kind: myuid.customExprOrStmtOrType
// EXPR-NEXT: key.name: "customExprOrStmtOrType"
// EXPR-NOT: myuid

// RUN: %sourcekitd-test -json-request-path %S/Inputs/custom-completion/custom.json == \
// RUN:     -req=complete.open -pos=4:12 %s -- %s | %FileCheck %s -check-prefix=TYPE

// TYPE-NOT: myuid
// TYPE: key.kind: myuid.customExprOrStmtOrType
// TYPE-NEXT: key.name: "customExprOrStmtOrType"
// TYPE-NOT: myuid
// TYPE: key.kind: myuid.customType
// TYPE-NEXT: key.name: "customType"
// TYPE-NOT: myuid

// RUN: %sourcekitd-test -json-request-path %S/Inputs/custom-completion/custom.json == \
// RUN:     -req=complete.open -pos=5:12 %s -- %s | %FileCheck %s -check-prefix=FOREACH

// FOREACH-NOT: myuid
// FOREACH: myuid.customForEach
// FOREACH-NEXT: key.name: "customForEach"
// FOREACH-NOT: myuid

// ===--- Filtering.

// RUN: %sourcekitd-test -json-request-path %S/Inputs/custom-completion/custom.json == \
// RUN:     -req=complete.open -pos=3:4 %s -req-opts=filtertext=custExp -- %s | %FileCheck %s -check-prefix=EXPR

// RUN: %sourcekitd-test -json-request-path %S/Inputs/custom-completion/custom.json == \
// RUN:     -req=complete.open -pos=3:4 %s -req-opts=filtertext=asdffdsa -- %s | %FileCheck %s -check-prefix=NOCUSTOM
// NOCUSTOM-NOT: myuid
