// RUN: %target-swift-frontend -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_no_presumed_loc %s | %FileCheck %s

func foo() {
  var x : Int = 0

// CHECK-LABEL: sil_coverage_map {{.*}}// foo()
// CHECK: [[@LINE+3]]:10 -> [[@LINE+6]]:4
// CHECK-NOT: 200:10 -> 100:4
#sourceLocation(file: "foo.swift", line: 200)
  repeat {
    x += 1
#sourceLocation(file: "bar.swift", line: 100)
  } while x < 10
}
