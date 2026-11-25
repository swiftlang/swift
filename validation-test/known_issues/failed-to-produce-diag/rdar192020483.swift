// rdar://192020483
// Reported as a "failed to produce diagnostic" error, but currently crashes on asserts builds.
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts

struct Box<T> {}
struct Wrapper<T> {}
func make<T>(_ value: Wrapper<T>) -> Box<T> { Box() }
func run(_ body: () -> Void) {}
func test(w: Wrapper<Int>) {
  run {
    let x: Box<Int?>? = make(w)
  }
}
