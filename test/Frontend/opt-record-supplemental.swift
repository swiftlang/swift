// RUN: %empty-directory(%t)
// RUN: echo '"%s": { yaml-opt-record: "%t/foo.opt.yaml" }' > %t/filemap.yaml.yaml
// RUN: echo '"%s": { bitstream-opt-record: "%t/foo.opt.bitstream" }' > %t/filemap.bitstream.yaml

// RUN: %target-swift-frontend -c -O -wmo -save-optimization-record=bitstream %s -module-name foo -o %t/foo.o -supplementary-output-file-map %t/filemap.bitstream.yaml
// RUN: llvm-bcanalyzer -dump "%t/foo.opt.bitstream" | %FileCheck -check-prefix=BITSTREAM %s

// RUN: %target-swift-frontend -c -O -wmo -save-optimization-record=yaml %s -module-name foo -o %t/foo.o -supplementary-output-file-map %t/filemap.yaml.yaml
// RUN: %FileCheck %s -check-prefix=YAML --input-file=%t/foo.opt.yaml

// REQUIRES: VENDOR=apple

var a: Int = 1

#sourceLocation(file: "custom.swift", line: 2000)
func foo() {
  a = 2
}
#sourceLocation() // reset

public func bar() {
  foo()
  // BITSTREAM: <Remark NumWords=13 BlockCodeSize=4>
  // BITSTREAM: </Remark>

  // YAML: sil-assembly-vision-remark-gen
}
