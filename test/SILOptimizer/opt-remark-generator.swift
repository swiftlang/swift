// RUN: %target-swiftc_driver -O -Rpass-missed=sil-opt-remark-gen -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -wmo -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil -save-optimization-record=yaml  -save-optimization-record-path %t/note.yaml %s -o /dev/null && %FileCheck --input-file=%t/note.yaml %s

// CHECK: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 49, Column: 5 }
// CHECK-NEXT: Function:        'getGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          Unable to remove retain
// CHECK-NEXT: ...
// CHECK-NEXT: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 56, Column: 5 }
// CHECK-NEXT: Function:        'useGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          Unable to remove retain
// CHECK-NEXT: ...
// CHECK-NEXT: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 56, Column: 12 }
// CHECK-NEXT: Function:        'useGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          Unable to remove release
// CHECK-NEXT: ...
// CHECK-NEXT: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 56, Column: 12 }
// CHECK-NEXT: Function:        'useGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          Unable to remove release
// CHECK-NEXT: ...

public class Klass {}

public var global = Klass()

@inline(never)
public func getGlobal() -> Klass {
    return global // expected-remark @:5 {{Unable to remove retain}}
}

public func useGlobal() {
    let x = getGlobal()
    // Make sure that the retain msg is at the beginning of the print and the
    // releases are the end of the print.
    print(x) // expected-remark @:5 {{Unable to remove retain}}
    // expected-remark @-1:12 {{Unable to remove release}}
    // expected-remark @-2:12 {{Unable to remove release}}
}
