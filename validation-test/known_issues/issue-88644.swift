// https://github.com/swiftlang/swift/issues/88644
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple
protocol Seq<Elt> {
  associatedtype Elt
}
struct Zip<each S: Seq>: Seq {
  typealias Elt = (repeat (each S).Elt)
}
struct Test<each S: Seq> {
  func test() {
    // expected-error@+1:46{{pack expansion requires that 'Zip<repeat each S>.Elt' (aka 'repeat each S.Elt') and 'each S' have the same shape}}
    let x: some Seq<(repeat (each S).Elt)> = Zip<repeat each S>()
  }
}
