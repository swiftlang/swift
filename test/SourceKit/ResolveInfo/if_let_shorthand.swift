var a: Int? = 1

if let a {
    print(a)
}

// RUN: %sourcekitd-test -req=collect-resolve-info %s -- %s | %FileCheck %s
// CHECK: <Declarations>
// CHECK-NEXT: Reference (7, 10):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:Si
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Math/Integers
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (32, 37):
// CHECK-NEXT: 	Kind: Func
// CHECK-NEXT: 	USR: s:s5print_9separator10terminatoryypd_S2StF
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Misc
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (38, 39):
// CHECK-NEXT: 	Kind: Var
// CHECK-NEXT: 	USR: s:16if_let_shorthand1aL_Sivp
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/if_let_shorthand.swift
// CHECK-NEXT: 	Range: (24, 25)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: </Declarations>