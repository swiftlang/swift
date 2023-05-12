// RUN: %target-swift-emit-silgen %s -enable-experimental-feature VariadicGenerics -disable-availability-checking

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

struct Variadic<each T> where repeat each T: Equatable {}

_ = Variadic<Int, String>()
