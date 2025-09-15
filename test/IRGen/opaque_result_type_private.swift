// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -c -primary-file %s %S/Inputs/opaque_result_type_private_2.swift

// This test used to crash during IRGen.

private struct C : P {
  var x = 1
  func d() -> some P {
    return self
  }
}

public func test() {
    var x = B(C())
    print(x)
}
