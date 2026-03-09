// RUN: %target-run-simple-swift(-language-mode 5) | %FileCheck %s
// RUN: %target-run-simple-swift(-language-mode 6) | %FileCheck %s
//
// REQUIRES: executable_test
//
// https://github.com/swiftlang/swift/issues/87566
protocol Attribute {}
struct Anchor: Attribute {}

extension Anchor {
  func yolo(_ str: String, hardMode: Bool = true) {
    print(#function)
  }
}

extension Attribute {
  func yolo(_ str: String) {
    print(#function)
  }
}

Anchor().yolo("")

// CHECK: yolo(_:)
