// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --round-trip-lex
// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --round-trip-parse

typealias TwoInts = (Int, Int)
typealias TwoNamedInts = (a: Int, b: Int)
typealias VoidTuple = ()
typealias TupleWithTrivia = (   Int ,  b  :Int   )
