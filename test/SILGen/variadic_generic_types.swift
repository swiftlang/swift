// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s -target %target-swift-5.9-abi-triple
// RUN: %target-swift-emit-silgen %s -target %target-swift-5.9-abi-triple

struct Variadic<each T> where repeat each T: Equatable {}

_ = Variadic<Int, String>()
