public protocol P { }

extension Int : P {}

struct Container {
  // This opaque result type is private to this file and its type metadata is
  // not accessible from another TU. Therefore, Container's fields are not ABI
  // accessible from another TU.
  private let mem : some P = 5
}
