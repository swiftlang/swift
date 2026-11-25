// rdar://190060986

// RUN: %target-swift-frontend -emit-sil -parse-as-library -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes %s -o /dev/null -verify
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

@_lifetime(borrow parent)
func callOneArg(_ parent: borrowing S) -> Span<Int> { parent.oneArg(0) }
func callTwoArg(_ parent: borrowing S) -> Span<Int> { parent.twoArg(0, 1) }
func callPackArg(_ parent: borrowing S) -> Span<Int> { parent.onePackArg(0, 1, 2) }
func callTupleArg(_ parent: borrowing S) -> Span<Int> { parent.oneTupleArg((0, 1)) }

@_addressableForDependencies
struct Box {
  var x: Int
  var span: Span<Int> {
    @_lifetime(borrow self) get { fatalError() }
  }
}

struct S {
  let storage: Box

  @_lifetime(borrow self) func oneArg(_ n: Int) -> Span<Int> { storage.span }
  @_lifetime(borrow self) func twoArg(_ n: Int, _ m: Int) -> Span<Int> { storage.span }
  @_lifetime(borrow self) func onePackArg<each T>(_ nn: repeat each T) -> Span<Int> { storage.span }
  @_lifetime(borrow self) func oneTupleArg(_ nm: (Int, Int)) -> Span<Int> { storage.span }
}
