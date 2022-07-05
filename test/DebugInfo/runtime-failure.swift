// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

public func f(_ input: Int) -> Int {
  return input * 2 + 1
}

// CHECK: distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow"
// CHECK-SAME: scope: ![[FILE:[0-9]+]]
// CHECK-SAME: file: ![[FILE]]
// CHECK-NOT: "Swift runtime failure: arithmetic overflow"
