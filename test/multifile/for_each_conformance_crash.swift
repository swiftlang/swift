// RUN: %target-swift-frontend -emit-ir %S/Inputs/for_each_conformance_crashB.swift -primary-file %s -o -

extension Q where A == MyStruct {
  func foo() {
    for _ in getArray() { }
  }
}
