// RUN: not %target-swift-frontend %s -typecheck

func c<each This, each Argument, each Return>(_ fn: (repeat each This) -> (repeat each Argument) -> (repeat each Return)) -> (repeat each This, repeat each Argument) -> (repeat each Return) {
  return {
    return fn(repeat each $0)(repeat each $1)
  }
}
