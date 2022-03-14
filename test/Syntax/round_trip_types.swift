// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %s

typealias TwoInts = (Int, Int)
typealias TwoNamedInts = (a: Int, b: Int)
typealias VoidTuple = ()
typealias TupleWithTrivia = (   Int ,  b  :Int   )

func testAnyType() {
  protocol MyProto {}

  let foo: [Int: any MyProto]
}
