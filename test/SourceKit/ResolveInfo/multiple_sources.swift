class C {}

let c = C(msg: "Hello")

// RUN: %sourcekitd-test -req=collect-resolve-info %s -- %s %S/Inputs/other.swift | %FileCheck %s
// CHECK: <Declarations>
// CHECK-NEXT: Reference (20, 21):
// CHECK-NEXT:	Kind: Constructor
// CHECK-NEXT:	USR: s:4main1CC3msgACSS_tcfc
// CHECK-NEXT:	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/Inputs/other.swift
// CHECK-NEXT:	Range: (27, 44)
// CHECK-NEXT:	Synthesized: 0
// CHECK-NEXT:	Secondary declaration:
// CHECK-NEXT:		Kind: Class
// CHECK-NEXT:		USR: s:4main1CC
// CHECK-NEXT:		Source: SOURCE_DIR/test/SourceKit/ResolveInfo/multiple_sources.swift
// CHECK-NEXT:		Range: (6, 7)
// CHECK-NEXT:		Synthesized: 0
// CHECK-NEXT: </Declarations>