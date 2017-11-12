// RUN: %target-typecheck-verify-swift

enum Bar {
  case Simple
  case Complex(Int)
}

func optEnumContext() -> Bar? {
  switch () {
  case ():
    return .Simple
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .Complex(0)
  }
}

func iuoEnumContext() -> Bar! {
  switch () {
  case ():
    return .Simple
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .Complex(0)
  }
}

