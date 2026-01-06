// RUN: not %target-swift-frontend -typecheck %s

// N.B.: Requires a no-asserts build to reproduce, otherwise this hits an
// assertion in type check pattern.
// REQUIRES: no_asserts

indirect enum BadOverload {
case one(Bool, other: Void)
case one(Bool?)
}

func crash(_ x: BadOverload) {
  switch $0 {
  case .one(false?):
    break
  }
}

