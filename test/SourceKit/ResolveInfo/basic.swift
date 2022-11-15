let x: Int = 3
let y = "abc"

let z = x

struct A {}

func bar() {
    let x = 4.0
    let y = A()
    print(x)
}


// RUN: %sourcekitd-test -req=collect-resolve-info %s -- %s | %FileCheck %s
// CHECK: <Declarations>
// CHECK-NEXT: Reference (7, 10):
// CHECK-NEXT: 	Kind: Struct
// CHECK-NEXT: 	USR: s:Si
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Math/Integers
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (38, 39):
// CHECK-NEXT: 	Kind: Var
// CHECK-NEXT: 	USR: s:5basic1xSivp
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/basic.swift
// CHECK-NEXT: 	Range: (4, 5)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (95, 96):
// CHECK-NEXT: 	Kind: Constructor
// CHECK-NEXT: 	USR: s:5basic1AVACycfc
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/basic.swift
// CHECK-NEXT: 	Range: (48, 52)
// CHECK-NEXT: 	Synthesized: 1
// CHECK-NEXT: 	Secondary declaration:
// CHECK-NEXT: 		Kind: Struct
// CHECK-NEXT: 		USR: s:5basic1AV
// CHECK-NEXT: 		Source: SOURCE_DIR/test/SourceKit/ResolveInfo/basic.swift
// CHECK-NEXT: 		Range: (48, 49)
// CHECK-NEXT: 		Synthesized: 0
// CHECK-NEXT: Reference (103, 108):
// CHECK-NEXT: 	Kind: Func
// CHECK-NEXT: 	USR: s:s5print_9separator10terminatoryypd_S2StF
// CHECK-NEXT: 	Module: Swift
// CHECK-NEXT: 	Module group: Misc
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: Reference (109, 110):
// CHECK-NEXT: 	Kind: Var
// CHECK-NEXT: 	s:5basic3baryyF1xL_Sdvp
// CHECK-NEXT: 	Source: SOURCE_DIR/test/SourceKit/ResolveInfo/basic.swift
// CHECK-NEXT: 	Range: (75, 76)
// CHECK-NEXT: 	Synthesized: 0
// CHECK-NEXT: </Declarations>