// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -O -sil-based-debuginfo -Xllvm -sil-print-debuginfo -emit-ir -o %t/out.ir
// RUN: %FileCheck %s < %t/out.ir
// RUN: %FileCheck %s --check-prefix=CHECK_OUT_SIL < %t/out.ir.sil_dbg_0.sil

// Second test: check that we don't crash with multi-threaded IRGen
// RUN: %target-swift-frontend -c %s %S/Inputs/testclass.swift -wmo -O -num-threads 1 -sil-based-debuginfo -o %t/sil_based_dbg.o -o %t/testclass.o

// CHECK: !DIFile(filename: "{{.+}}sil_based_dbg.swift", directory: "{{.+}}")
// CHECK: [[F:![0-9]+]] = !DIFile(filename: "{{.+}}out.ir.sil_dbg_0.sil",
// CHECK: !DISubprogram(linkageName: "$s3out6testityyF", scope: !{{[0-9]+}}, file: [[F]], line: {{[1-9][0-9]+}},

// CHECK_OUT_SIL: sil @$s3out6testityyF : $@convention(thin) () -> () {
public func testit() {
  print("Hello")
}

