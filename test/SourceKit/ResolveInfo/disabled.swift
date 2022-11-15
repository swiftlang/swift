let a = 1
#if false
print(a)
#endif
print(a)



// RUN: %sourcekitd-test -req=collect-resolve-info %s -- %s | %FileCheck %s
// CHECK: <Declarations>
// CHECK-NEXT: Reference (36, 41):
// CHECK-NEXT: 	Kind: Func
// CHECK-NEXT: 	USR: s:s5print_9separator10terminatoryypd_S2StF
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Misc
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (42, 43):
// CHECK-NEXT: 	Kind: Var
// CHECK-NEXT: 	USR: s:8disabled1aSivp
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/disabled.swift
// CHECK-NEXT: 	Range: (4, 5)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: </Declarations>