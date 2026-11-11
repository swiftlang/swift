// rdar://192237592 (https://github.com/swiftlang/swift/issues/94111)
// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype State
}
@resultBuilder
enum Builder {
  static func buildBlock<R>(_ r: R) -> R { r }
}
struct Leaf<State, Action>: P {
  init(_ f: (Action) -> Void) {}
}
func cond<Child: P>(_ k: KeyPath<Child.State, Bool>, @Builder _ child: () -> Child) {}
cond(\.flag) { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  Leaf { action in
    switch action {}
  }
}
