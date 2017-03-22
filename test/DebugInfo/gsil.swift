// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend %s -O -gsil -emit-ir -o %t/out.ir
// RUN: %FileCheck %s < %t/out.ir
// RUN: %FileCheck %s --check-prefix=CHECK_OUT_SIL < %t/out.ir.gsil_0.sil

// CHECK: [[F:![0-9]+]] = !DIFile(filename: "out.ir.gsil_0.sil", directory: "{{.+}}")
// CHECK: !DISubprogram(linkageName: "_T03out6testityyF", scope: !{{[0-9]+}}, file: [[F]], line: {{[1-9][0-9]+}},

// CHECK_OUT_SIL: sil @_T03out6testityyF : $@convention(thin) () -> () {
public func testit() {
  print("Hello")
}

