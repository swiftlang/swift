// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -O -wmo -save-optimization-record %s -module-name optrecordmod -o %t/opt-record 2>&1 | %FileCheck -allow-empty %s
// RUN: %FileCheck -check-prefix=YAML %s < %t/optrecordmod.opt.yaml
// RUN: %target-swiftc_driver -O -wmo -save-optimization-record-path %t/specified.opt.yaml %s -module-name optrecordmod -o %t/opt-record 2>&1 | %FileCheck -allow-empty %s
// RUN: %FileCheck -check-prefix=YAML %s < %t/specified.opt.yaml

// CHECK-NOT: remark

var a: Int = 1

func foo() {
  a = 2
}

public func bar() {
  // YAML:      --- !Passed
  // YAML-NEXT: Pass:            sil-inliner
  // YAML-NEXT: Name:            Inlined
  // YAML-NEXT: DebugLoc:
  // YAML-NEXT:   File:            {{.*}}opt-record.swift
  // YAML-NEXT:   Line:            42
  // YAML-NEXT:   Column:          3
  // YAML-NEXT: Function:        'bar()'
  // YAML-NEXT: Args:
  // YAML-NEXT:   - Callee:          '"foo()"'
  // YAML-NEXT:     DebugLoc:
  // YAML-NEXT:       File:            {{.*}}opt-record.swift
  // YAML-NEXT:       Line:            11
  // YAML-NEXT:       Column:          6
  // YAML-NEXT:   - String:          ' inlined into '
  // YAML-NEXT:   - Caller:          '"bar()"'
  // YAML-NEXT:     DebugLoc:
  // YAML-NEXT:       File:            {{.*}}opt-record.swift
  // YAML-NEXT:       Line:            15
  // YAML-NEXT:       Column:          13
  // YAML-NEXT:   - String:          ' (cost = '
  // YAML-NEXT:   - Cost:            '{{.*}}'
  // YAML-NEXT:   - String:          ', benefit = '
  // YAML-NEXT:   - Benefit:         '{{.*}}'
  // YAML-NEXT:   - String:          ')'
  // YAML-NEXT: ...
  foo()
}
