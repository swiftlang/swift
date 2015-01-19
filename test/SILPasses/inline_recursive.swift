// RUN: %target-swift-frontend -primary-file %s -disable-func-sig-opts -parse-as-library -emit-sil -O | FileCheck %s

private func recFunc(x: Int) -> Int {
  if x > 0 {
    return recFunc(x - 1)
  }
  return 0
}

//CHECK-LABEL: sil {{.*}}callit
// CHECK: bb0:
// CHECK-NEXT: integer_literal {{.*}}, 0
// CHECK-NEXT: struct
// CHECK-NEXT: return

func callit() -> Int {
  return recFunc(3)
}

