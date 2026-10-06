// Just enough definition here to test the behavior of referencing the names.
public struct Cell<Value: ~Copyable>: ~Copyable {
  init() {}
}
public struct ConstCell<Value: ~Copyable>: ~Copyable {
  init() {}
}

public struct Int {}

// Make sure the stdlib is able to refer to its own declarations irrespective
// of feature flag settings.
public func referenceCell(x: borrowing Cell<Int>) {}
public func referenceConstCell(x: borrowing ConstCell<Int>) {}
