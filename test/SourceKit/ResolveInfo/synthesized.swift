class H: Hashable {
    static func == (lhs: H, rhs: H) -> Bool {
        true
    }

    func hash(into hasher: inout Hasher) {}

    func _rawHashValue(seed: Int) -> Int {
        21
    }
}

let h = H()

let _ = h.hashValue

// RUN: %sourcekitd-test -req=collect-resolve-info %s -- %s | %FileCheck %s
// CHECK: <Declarations>
// CHECK-NEXT: Reference (9, 17):
// CHECK-NEXT: 	Kind: Protocol
// CHECK-NEXT: 	USR: s:SH
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Hashing
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (45, 46):
// CHECK-NEXT: 	Kind: Class
// CHECK-NEXT: 	USR: s:11synthesized1HC
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/synthesized.swift
// CHECK-NEXT: 	Range: (6, 7)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (53, 54):
// CHECK-NEXT: 	Kind: Class
// CHECK-NEXT: 	USR: s:11synthesized1HC
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/synthesized.swift
// CHECK-NEXT: 	Range: (6, 7)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (59, 63):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:Sb
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Bool
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (119, 125):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:s6HasherV
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Hashing
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (160, 163):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:Si
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Math/Integers
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (168, 171):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:Si
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Math/Integers
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (202, 203):
// CHECK-NEXT: 	Kind: Constructor
// CHECK-NEXT: 	USR: s:11synthesized1HCACycfc
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/synthesized.swift
// CHECK-NEXT: 	Range: (6, 10)
// CHECK-NEXT: 	Synthesized: 1
// CHECK-NEXT: 	Secondary declaration:
// CHECK-NEXT: 		Kind: Class
// CHECK-NEXT: 		USR: s:11synthesized1HC
// CHECK-NEXT: 		Source: SOURCE_DIR/test/SourceKit/ResolveInfo/synthesized.swift
// CHECK-NEXT: 		Range: (6, 7)
// CHECK-NEXT: 		Synthesized: 0
// CHECK-NEXT: Reference (215, 216):
// CHECK-NEXT: 	Kind: Var
// CHECK-NEXT: 	USR: s:11synthesized1hAA1HCvp
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/synthesized.swift
// CHECK-NEXT: 	Range: (198, 199)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (217, 226):
// CHECK-NEXT: 	Kind: Var
// CHECK-NEXT: 	USR: s:11synthesized1HC9hashValueSivp
// CHECK-NEXT: 	Module: synthesized
// CHECK-NEXT: 	Synthesized: 1
// CHECK-NEXT: </Declarations>