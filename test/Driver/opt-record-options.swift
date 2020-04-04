// RUN: not %target-swiftc_driver -wmo -save-optimization-record=unknown %s -module-name optrecordmod -o /dev/null 2>&1 | %FileCheck --check-prefix=ERROR-FORMAT %s
// ERROR-FORMAT: error while creating remark serializer: 'Unknown remark format: 'unknown''

// RUN: %target-swiftc_driver -wmo -save-optimization-record %s -module-name optrecordmod -o /dev/null 2>&1 -### | %FileCheck --check-prefix=YAML-PATH %s
// RUN: %target-swiftc_driver -wmo -save-optimization-record=yaml %s -module-name optrecordmod -o /dev/null 2>&1 -### | %FileCheck --check-prefix=YAML-PATH %s
// YAML-PATH: -save-optimization-record-path {{.*}}optrecordmod.opt.yaml

// RUN: %target-swiftc_driver -wmo -save-optimization-record=bitstream %s -module-name optrecordmod -o /dev/null 2>&1 -### | %FileCheck --check-prefix=BITSTREAM-PATH %s
// BITSTREAM-PATH: -save-optimization-record=bitstream
// BITSTREAM-PATH: -save-optimization-record-path {{.*}}optrecordmod.opt.bitstream

// RUN: %target-swiftc_driver -wmo -save-optimization-record -save-optimization-record-passes sil-inliner %s -module-name optrecordmod -o /dev/null 2>&1 -### | %FileCheck --check-prefix=FILTER-PASS %s
// FILTER-PASS: -save-optimization-record-passes sil-inliner

var a: Int = 1
#sourceLocation(file: "custom.swift", line: 2000)
func foo() {
  a = 2
}
#sourceLocation() // reset
