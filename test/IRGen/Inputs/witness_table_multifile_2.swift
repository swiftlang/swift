protocol P {
  var x: String? { get set }
  func foo()
}

struct X : P {
  var x: String
  func foo() { print("X(x: \(x))") }
}

func go() -> P { return X("hello") }

protocol ProtocolOnlyUsedAsAType {
}
