// RUN: %target-swift-emit-silgen %s

func calleeWithEmptyTuple<A>(_: A = ()) {}
func calleeWithLoadable<A>(_: A = 3) {}
func calleeWithAddressOnly<A>(_: A = (3 as Any)) {}
func calleeWithTupleOfLoadable<A>(_: A = (3, 4)) {}
func calleeWithTupleOfAddressOnly<A>(_: A = (3 as Any, 4 as Any)) {}
func calleeWithTupleOfMixed<A>(_: A = (3, 4 as Any)) {}

func testConcreteDefaultArguments() {
  calleeWithEmptyTuple()
  calleeWithLoadable()
  calleeWithAddressOnly()
  calleeWithTupleOfLoadable()
  calleeWithTupleOfAddressOnly()
  calleeWithTupleOfMixed()
}