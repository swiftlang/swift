// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -O -wmo -save-optimization-record %s -module-name optrecordmod -o %t/opt-record 2>&1 | %FileCheck -allow-empty %s
// RUN: %FileCheck -check-prefix=YAML %s < %t/optrecordmod.opt.yaml
// RUN: %target-swiftc_driver -O -wmo -save-optimization-record-path %t/specified.opt.yaml %s -module-name optrecordmod -o %t/opt-record 2>&1 | %FileCheck -allow-empty %s
// RUN: %FileCheck -check-prefix=YAML %s < %t/specified.opt.yaml
// RUN: %target-swiftc_driver -O -wmo -save-optimization-record -save-optimization-record-passes sil-inliner %s -module-name optrecordmod -o %t/opt-record 2>&1 | %FileCheck -allow-empty %s
// RUN: %FileCheck -check-prefix=YAML %s < %t/optrecordmod.opt.yaml
// RUN: %target-swiftc_driver -O -wmo -save-optimization-record -save-optimization-record-passes unknown -save-optimization-record-path %t/optrecordmod-filtered.opt.yaml %s -module-name optrecordmod -o %t/opt-record 2>&1 | %FileCheck -allow-empty %s
// RUN: %FileCheck -allow-empty -check-prefix=YAML-FILTERED %s < %t/optrecordmod-filtered.opt.yaml

// CHECK-NOT: remark

var a: Int = 1

#sourceLocation(file: "custom.swift", line: 2000)
func foo() {
  a = 2
}
#sourceLocation() // reset

public func bar() {
  foo()
  // YAML:      --- !Passed
  // YAML-NEXT: Pass:            sil-inliner
  // YAML-NEXT: Name:            sil.Inlined
  // YAML-NEXT: DebugLoc:
  // YAML:        File:            {{.*}}opt-record.swift
  // YAML:        Line:            [[@LINE-6]]
  // YAML:        Column:          3
  // YAML-NEXT: Function:        '$s12optrecordmod3baryyF'
  // YAML-NEXT: Args:
  // YAML-NEXT:   - Callee:          '"optrecordmod.foo()"'
  // YAML-NEXT:     DebugLoc:
  // YAML:            File:            custom.swift
  // YAML:            Line:            2000
  // YAML:            Column:          6
  // YAML-NEXT:   - String:          ' inlined into '
  // YAML-NEXT:   - Caller:          '"optrecordmod.bar()"'
  // YAML-NEXT:     DebugLoc:
  // YAML:            File:            {{.*}}opt-record.swift
  // YAML:            Line:            [[@LINE-20]]
  // YAML:            Column:          13
  // YAML-NEXT:   - String:          ' (cost = '
  // YAML-NEXT:   - Cost:            '{{.*}}'
  // YAML-NEXT:   - String:          ', benefit = '
  // YAML-NEXT:   - Benefit:         '{{.*}}'
  // YAML-NEXT:   - String:          ')'
  // YAML-NEXT: ...
}

// YAML-FILTERED-NOT: sil-inliner
