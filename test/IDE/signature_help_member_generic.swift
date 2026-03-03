// RUN: %target-swift-ide-test -signature-help -code-completion-token=MEMBER_GENERIC -source-filename=%s | %FileCheck %s --check-prefix=MEMBER_GENERIC

struct Vector<Value> {
  init(elements: [Value]) { }
  
  func dot(with other: Vector<Value>) -> Value { fatalError() }
}

let vec = Vector(elements: [1.0, 2.1, 3.4])
vec.dot(with: #^MEMBER_GENERIC^#)
// MEMBER_GENERIC:     Begin signatures, 1 items
// MEMBER_GENERIC-DAG: Signature[Active]: dot(<param name="other" active>with: Vector<Double></param>) -> Double
