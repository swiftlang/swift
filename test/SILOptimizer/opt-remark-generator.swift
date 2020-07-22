// RUN: %target-swiftc_driver -O -Rpass-missed=sil-opt-remark-gen -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -wmo -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil -save-optimization-record=yaml  -save-optimization-record-path %t/note.yaml %s -o /dev/null && %FileCheck --input-file=%t/note.yaml %s

// CHECK: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 59, Column: 5 }
// CHECK-NEXT: Function:        'getGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          'Found retain:'
// CHECK-NEXT:   - InferredValue:   'on value:'
// CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
// CHECK-NEXT:                        Line: 55, Column: 12 }
// CHECK-NEXT: ...
// CHECK-NEXT: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 67, Column: 5 }
// CHECK-NEXT: Function:        'useGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          'Found retain:'
// CHECK-NEXT:   - InferredValue:   'on value:'
// CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
// CHECK-NEXT:                        Line: 64, Column: 9 }
// CHECK-NEXT: ...
// CHECK-NEXT: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 67, Column: 12 }
// CHECK-NEXT: Function:        'useGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          'Found release:'
// CHECK-NEXT:   - InferValueFailure: Unable to infer any values being released.
// CHECK-NEXT: ...
// CHECK-NEXT: --- !Missed
// CHECK-NEXT: Pass:            sil-opt-remark-gen
// CHECK-NEXT: Name:            sil.memory-management
// CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift', 
// CHECK-NEXT:                    Line: 67, Column: 12 }
// CHECK-NEXT: Function:        'useGlobal()'
// CHECK-NEXT: Args:
// CHECK-NEXT:   - String:          'Found release:'
// CHECK-NEXT:   - InferredValue:   'on value:'
// CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
// CHECK-NEXT:                        Line: 64, Column: 9 }
// CHECK-NEXT: ...

public class Klass {}

public var global = Klass()

@inline(never)
public func getGlobal() -> Klass {
    return global // expected-remark @:5 {{Found retain:}}
                  // expected-note @-5:12 {{on value:}}
}

public func useGlobal() {
    let x = getGlobal()
    // Make sure that the retain msg is at the beginning of the print and the
    // releases are the end of the print.
    print(x) // expected-remark @:5 {{Found retain:}}
             // expected-note @-4:9 {{on value:}}
             // expected-remark @-2:12 {{Found release:}}
             // expected-note @-3:12 {{Unable to infer any values being released.}}
             // expected-remark @-4:12 {{Found release:}}
             // expected-note @-8:9 {{on value:}}
}
