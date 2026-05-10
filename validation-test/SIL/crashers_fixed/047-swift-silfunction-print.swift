// RUN: %target-swift-frontend -emit-sil %s

// https://github.com/apple/swift/issues/54666
// Use-after-free in `SILFunction::print`

func outer<C>(_ x: C) {
  func inner<C>(_ x: C) {}
}
