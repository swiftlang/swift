// RUN: %target-swift-frontend -module-name Swift -parse-stdlib -emit-silgen %s | FileCheck %s

enum Optional<T> {
  case Some(T), None
}

protocol BooleanType {
  var boolValue: Bool { get }
}

struct Bool : BooleanType {
  var value: Builtin.Int1
  func _getBuiltinLogicValue() -> Builtin.Int1 { return value }
  var boolValue: Bool { return self }
}

// CHECK-LABEL: sil hidden @_TFs5test1
func test1(bi: Bool) {
  var b = bi
  for var c = b; b; b = c {
    if b {
      break
    }
    continue
  }
}
