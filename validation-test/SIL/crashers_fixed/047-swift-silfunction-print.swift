// RUN: %target-swift-frontend -emit-sil %s

// SR-12239: use-after-free in `SILFunction::print`.

func outer<C>(_ x: C) {
  func inner<C>(_ x: C) {}
}
