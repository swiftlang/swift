class C {
    init() {}
}

let c = C()

extension Int {
    init(str: String) {
     self.init(0.0)
    }
}

let num = Int(str: "Hello")

// RUN: %sourcekitd-test -req=collect-resolve-info %s -- %s | %FileCheck %s
// CHECK: <Declarations>
// CHECK-NEXT: Reference (35, 36):
// CHECK-NEXT: 	Kind: Constructor
// CHECK-NEXT: 	USR: s:14secondary_decl1CCACycfc
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/secondary_decl.swift
// CHECK-NEXT: 	Range: (14, 20)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: 	Secondary declaration:
// CHECK-NEXT: 		Kind: Class
// CHECK-NEXT: 		USR: s:14secondary_decl1CC
// CHECK-NEXT: 		Source: SOURCE_DIR/test/SourceKit/ResolveInfo/secondary_decl.swift
// CHECK-NEXT: 		Range: (6, 7)
// CHECK-NEXT: 		Synthesized: 0
// CHECK-NEXT: Reference (50, 53):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:Si
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Math/Integers
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (70, 76):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:SS
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: String
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (85, 89):
// CHECK-NEXT: 	Kind: Param
// CHECK-NEXT: 	USR: s:Si14secondary_declE3strSiSS_tcfc4selfL_Sivp
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/secondary_decl.swift
// CHECK-NEXT: 	Range: (60, 64)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (90, 94):
// CHECK-NEXT: 	Kind: Constructor
// CHECK-NEXT: 	USR: s:SiySiSdcfc
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Math/Integers
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (119, 122):
// CHECK-NEXT: 	Kind: Constructor
// CHECK-NEXT: 	USR: s:Si14secondary_declE3strSiSS_tcfc
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/secondary_decl.swift
// CHECK-NEXT: 	Range: (60, 77)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: 	Secondary declaration:
// CHECK-NEXT: 		Kind: Struct
// CHECK-NEXT: 		USR: s:Si
// CHECK-NEXT: 		Module: Swift
// CHECK-NEXT: 		Module group: Math/Integers
// CHECK-NEXT: 		Synthesized: 0
// CHECK-NEXT: </Declarations>