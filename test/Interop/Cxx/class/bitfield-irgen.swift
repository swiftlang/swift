// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default -disable-llvm-optzns -O | %FileCheck %s

import BitFields

public func readB(_ s: BitFields) -> UInt32 {
  return s.b
}

public func writeB(_ s: inout BitFields, _ v: UInt32) {
  s.b = v
}

// CHECK: define {{.*}}$So9BitFieldsV$b$getter
// CHECK: define {{.*}}$So9BitFieldsV$b$setter
