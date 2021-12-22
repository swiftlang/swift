// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

enum Payload {
  case int(Int)
  case keyValue(String, Int)
  case empty
}

// CHECK-LABEL: internal func test(payload: Payload) -> Int? {
func test(payload: Payload) -> Int? {
  switch payload {
  case .int(let int):
    return int
  case .keyValue(_, let int):
    return int
  case .empty:
    return nil
  }
}
// CHECK-LABEL: switch payload {
// CHECK-LABEL: case .int(let int):
// CHECK-LABEL:   return int
// CHECK-LABEL: case .keyValue(_, let int):
// CHECK-LABEL:   return int
// CHECK-LABEL: case .empty:
// CHECK-LABEL:   return nil
// CHECK-LABEL: }
// CHECK-LABEL:}
