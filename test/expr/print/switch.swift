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

func process(payload: Payload) {
  if case .empty = payload {
    return
  }
  _ = test(payload: payload)
}
// CHECK-LABEL: internal func process(payload: Payload) {
// CHECK-LABEL:   if .empty = payload {
// CHECK-LABEL:     return
// CHECK-LABEL:   }
// CHECK-LABEL:   _ = test(payload: payload)
// CHECK-LABEL: }

func foo(_ x: Int?) {
  switch x {
  case let x?:
    break
  case nil:
    break
  }
}
// CHECK-LABEL: internal func foo(_ x: Int?) {
// CHECK-LABEL:   switch x {
// CHECK-LABEL:   case let x?:
// CHECK-LABEL:     break
// CHECK-LABEL:   case .none:
// CHECK-LABEL:     break
// CHECK-LABEL:   }
// CHECK-LABEL: }
