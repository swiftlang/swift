// RUN: %target-typecheck-verify-swift

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
