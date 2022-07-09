// RUN: %{python} %S/../Inputs/timeout.py 10 %target-swift-frontend -O -parse-as-library %s -emit-sil | %FileCheck %s

public func callit() {
    testit { false }
}

// Check if the compiler terminates and does not full into an infinite optimization
// loop between the ClosureSpecializer and CapturePropagation.

// CHECK-LABEL: sil @$s23closure_specialize_loop6testit1cySbyc_tF
public func testit(c: @escaping () -> Bool) {
  if c() {
    testit { !c() }
  }
}

