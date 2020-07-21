// RUN: %target-swiftc_driver -O -Rpass-missed=sil-opt-remark-gen -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -wmo -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil -save-optimization-record=yaml  -save-optimization-record-path %t/note.yaml %s -o /dev/null && %FileCheck --input-file=%t/note.yaml %s

public class Klass {}

public var global = Klass()

@inline(never)
public func getGlobal() -> Klass {

    // CHECK: --- !Missed
    // CHECK-NEXT: Pass:            sil-opt-remark-gen
    // CHECK-NEXT: Name:            sil.memory-management
    // CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                    Line: [[#@LINE+8]], Column: 5 }
    // CHECK-NEXT: Function:        'getGlobal()'
    // CHECK-NEXT: Args:
    // CHECK-NEXT:   - String:          'Found retain:'
    // CHECK-NEXT:   - InferredValue:   'on value:'
    // CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                        Line: [[#@LINE-15]], Column: 12 }
    // CHECK-NEXT: ...
    return global // expected-remark @:5 {{Found retain:}}
                  // expected-note @-18:12 {{on value:}}
}

public func useGlobal() {
    let x = getGlobal()

    // CHECK-NEXT: --- !Missed
    // CHECK-NEXT: Pass:            sil-opt-remark-gen
    // CHECK-NEXT: Name:            sil.memory-management
    // CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                    Line: [[# @LINE + 35]], Column: 5 }
    // CHECK-NEXT: Function:        'useGlobal()'
    // CHECK-NEXT: Args:
    // CHECK-NEXT:   - String:          'Found retain:'
    // CHECK-NEXT:   - InferredValue:   'on value:'
    // CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                        Line: [[# @LINE - 12]], Column: 9 }
    // CHECK-NEXT: ...
    // CHECK-NEXT: --- !Missed
    // CHECK-NEXT: Pass:            sil-opt-remark-gen
    // CHECK-NEXT: Name:            sil.memory-management
    // CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                    Line: [[# @LINE + 23]], Column: 12 }
    // CHECK-NEXT: Function:        'useGlobal()'
    // CHECK-NEXT: Args:
    // CHECK-NEXT:   - String:          'Found release:'
    // CHECK-NEXT:   - InferredValue:   'on variadic argument array for argument 0:'
    // CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                        Line: [[# @LINE + 17]], Column: 5 }
    // CHECK-NEXT: ...
    // CHECK-NEXT: --- !Missed
    // CHECK-NEXT: Pass:            sil-opt-remark-gen
    // CHECK-NEXT: Name:            sil.memory-management
    // CHECK-NEXT: DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                    Line: [[# @LINE + 11]], Column: 12 }
    // CHECK-NEXT: Function:        'useGlobal()'
    // CHECK-NEXT: Args:
    // CHECK-NEXT:   - String:          'Found release:'
    // CHECK-NEXT:   - InferredValue:   'on value:'
    // CHECK-NEXT:     DebugLoc:        { File: '{{.*}}opt-remark-generator.swift',
    // CHECK-NEXT:                        Line: [[# @LINE - 36]], Column: 9 }
    // CHECK-NEXT: ...
    //
    // Make sure that the retain msg is at the beginning of the print and the
    // releases are the end of the print.
    print(x) // expected-remark @:5 {{Found retain:}}
             // expected-note @-42:9 {{on value:}}
             // expected-remark @-2:12 {{Found release:}}
             // expected-note @-3:5 {{on variadic argument array for argument 0:}}
             // expected-remark @-4:12 {{Found release:}}
             // expected-note @-46:9 {{on value:}}
}
