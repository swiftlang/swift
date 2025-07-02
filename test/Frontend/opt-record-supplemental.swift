// RUN: %empty-directory(%t)
// RUN: echo '"%s": { yaml-opt-record: "%t/foo.opt.yaml" }' > %t/filemap.yaml.yaml
// RUN: echo '"%s": { bitstream-opt-record: "%t/foo.opt.bitstream" }' > %t/filemap.bitstream.yaml

// RUN: %target-swift-frontend -c -O -num-threads 2 -save-optimization-record=bitstream %s %S/Inputs/opt-record-2.swift -module-name foo -o %t/foo.o  -o %t/opt-record-2.o -supplementary-output-file-map %t/filemap.bitstream.yaml
// RUN: llvm-bcanalyzer -dump "%t/foo.opt.bitstream" | %FileCheck -check-prefix=BITSTREAM %s
// RUN: llvm-bcanalyzer -dump "%t/opt-record-2.opt.bitstream" | %FileCheck -check-prefix=BITSTREAM2 %s

// RUN: %empty-directory(%t)
// RUN: echo '"%s": { yaml-opt-record: "%t/foo.opt.yaml" }' > %t/filemap.yaml.yaml
// RUN: echo '"%s": { bitstream-opt-record: "%t/foo.opt.bitstream" }' > %t/filemap.bitstream.yaml

// RUN: %target-swift-frontend -c -O -num-threads 2 -save-optimization-record=yaml %s %S/Inputs/opt-record-2.swift -module-name foo -o %t/foo.o -o %t/opt-record-2.o -supplementary-output-file-map %t/filemap.yaml.yaml
// RUN: %FileCheck %s -check-prefix=YAML --input-file=%t/foo.opt.yaml
// RUN: %FileCheck %s -check-prefix=YAML2 --input-file=%t/opt-record-2.opt.yaml

// REQUIRES: VENDOR=apple

var a: Int = 1

#sourceLocation(file: "custom.swift", line: 2000)
func foo() {
  a = 2
}
#sourceLocation() // reset

@_assemblyVision
public func bar() {
  foo()

  runSomeTest(C())
  // BITSTREAM: <Remark NumWords=13 BlockCodeSize=4>
  // BITSTREAM: </Remark>

  // BITSTREAM2: <Remark NumWords={{[0-9]+}} BlockCodeSize={{[0-9]*}}>
  // BITSTREAM2: </Remark>

  // YAML: sil-assembly-vision-remark-gen

  // YAML2: Pass: asm-printer
  // YAML2: Name: InstructionCount
}
