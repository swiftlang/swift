// RUN: %target-swift-frontend -emit-ir -O -wmo %s | %FileCheck %s

// CHECK-DAG: @"$S38nested_private_type_context_descriptor1X33{{.................................}}LLVMn" = hidden constant
// CHECK-DAG: @"$S38nested_private_type_context_descriptor1X33{{.................................}}LLV1YVMn" = hidden constant

fileprivate struct X {
  fileprivate struct Y {}
}

public func force_metadata() -> Any.Type {
  return X.Y.self
}
