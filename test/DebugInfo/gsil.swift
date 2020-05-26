// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -O -gsil -Xllvm -sil-print-debuginfo -emit-ir -o %t/out.ir
// RUN: %FileCheck %s < %t/out.ir
// RUN: %FileCheck %s --check-prefix=CHECK_OUT_SIL < %t/out.ir.gsil_0.sil

// Second test: check that we don't crash with multi-threaded IRGen
// RUN: %target-swift-frontend -c %s %S/Inputs/testclass.swift -wmo -O -num-threads 1 -gsil -o %t/gsil.o -o %t/testclass.o

// CHECK: !DIFile(filename: "{{.+}}gsil.swift", directory: "{{.+}}")
// CHECK: [[F:![0-9]+]] = !DIFile(filename: "{{.+}}out.ir.gsil_0.sil",
// CHECK: !DISubprogram(linkageName: "$s3out6testityyF", scope: !{{[0-9]+}}, file: [[F]], line: {{[1-9][0-9]+}},

// CHECK_OUT_SIL: sil @$s3out6testityyF : $@convention(thin) () -> () {
public func testit() {
  print("Hello")
}

