// RUN: %target-parse-verify-swift

enum Bar {
  case Simple
  case Complex(Int)
}

func optEnumContext() -> Bar? {
  switch () {
  case ():
    return .Simple
  case ():
    return .Complex(0)
  }
}

func iuoEnumContext() -> Bar! {
  switch () {
  case ():
    return .Simple
  case ():
    return .Complex(0)
  }
}

