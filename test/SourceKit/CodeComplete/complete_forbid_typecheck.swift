// RUN: %sourcekitd-test -req=complete -pos=4:1 %S/Inputs/forbid_typecheck_primary.swift -- -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NOTYPECHECK %S/Inputs/forbid_typecheck_2.swift %S/Inputs/forbid_typecheck_primary.swift | %FileCheck %s

// CHECK-DAG: key.name: "globalPrim"
// CHECK-DAG: key.name: "globalSec"
// CHECK-DAG: key.name: "ClsSec"
// CHECK-DAG: key.name: "SSec"
// CHECK-DAG: key.name: "primFn()"

// RUN: %sourcekitd-test -req=complete -pos=5:20 %S/Inputs/forbid_typecheck_primary.swift -- -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NOTYPECHECK %S/Inputs/forbid_typecheck_2.swift %S/Inputs/forbid_typecheck_primary.swift | %FileCheck %s -check-prefix=MEMBER

// MEMBER: key.name: "member"
