// rdar://175192279 (https://github.com/swiftlang/swift/issues/88570)
// RUN: %target-typecheck-verify-swift

class Action {}
typealias ActionHandler = (Action) -> Void

func test() {
  let _: ActionHandler = { print("Hello") }
  // expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}}

  let _: [ActionHandler] = [{ print("Hello") }]
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}

  let _: (ActionHandler, Int, Int) = ({ print("Hello") }, 1, 2)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
