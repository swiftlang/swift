// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

// https://github.com/swiftlang/swift/issues/75329

struct GPack1<each P, T> {}
struct GPack2<each P> {
  struct G<T> {}
}

func test<each T>(
  example1: repeat GPack1<repeat each T, each T>,
  example2: repeat GPack2<repeat each T>.G<each T>
) {
  let _ = (repeat each example1) // Boom
}
