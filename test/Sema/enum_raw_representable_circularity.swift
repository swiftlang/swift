// RUN: %target-typecheck-verify-swift

// This used to fail with "reference to invalid associated type 'RawValue' of type 'E'"
_ = E(rawValue: 123)

enum E : Int {
  case a = 123

  init?(rawValue: RawValue) {
    self = .a
  }
}

