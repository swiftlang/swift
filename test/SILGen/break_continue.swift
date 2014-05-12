// RUN: %swift -module-name=Swift -parse-stdlib -emit-silgen %s | FileCheck %s

protocol LogicValue {
  func getLogicValue() -> Bool
}

struct Bool : LogicValue {
  var value: Builtin.Int1
  func _getBuiltinLogicValue() -> Builtin.Int1 { return value }
  func getLogicValue() -> Bool { return self }
}

// CHECK-LABEL: sil  @_TFSs5test1
func test1(bi: Bool) {
  var b = bi
  for var c = b; b; b = c {
    if b {
      break
    }
    continue
  }
}
