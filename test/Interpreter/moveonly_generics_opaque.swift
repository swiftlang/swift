// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -O) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol Marked {
  func announce()
}
extension Marked {
  func announce() { print("\(Self.self) is Marked") }
}

struct ConcreteWrapper<Wrapped> {}
extension ConcreteWrapper: Marked where Wrapped: Marked {}

struct Hello<T: ~Copyable> {}
extension Hello: Marked {}

func makeWrapper<P>(wrapping _: P.Type) -> some Marked {
    ConcreteWrapper<Hello<P>>()
}

do {
  let markedVal = makeWrapper(wrapping: String.self)
  markedVal.announce() // CHECK: ConcreteWrapper<Hello<String>> is Marked
}
