// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/79474

enum SwitchResult {
  case ints([Int])
  case strings([String])
}

func diagnoseLaterBranchMemberError(_ value: SwitchResult) -> [Int]? {
  switch value {
  case .strings:
    nil

  case .ints(let values):
    if values.count > 1 {
      values.missingMember // expected-error {{value of type '[Int]' has no member 'missingMember'}}
    } else {
      nil
    }
  }
}

let _: Int? = if true {
  nil
} else {
  [0].missingMember // expected-error {{value of type '[Int]' has no member 'missingMember'}}
}

func typeMismatch() -> String? {
  return if .random() {
    nil
  } else {
    1.0 // expected-error {{cannot convert value of type 'Double' to specified type 'String'}}
  }
}
