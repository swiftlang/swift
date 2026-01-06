// RUN: %target-swift-emit-ir \
// RUN:     %s                \
// RUN:     -disable-availability-checking

func foo<each S>(_ s: repeat each S) async {}
await foo(true)

