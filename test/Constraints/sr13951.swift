// RUN: %target-typecheck-verify-swift

protocol TheProtocol {}
struct TheType1: TheProtocol {}

enum TheEnum: RawRepresentable {
  typealias RawValue = TheProtocol

  case case1
  case case2

  init?(rawValue: TheProtocol) {
    self = .case1
  }

  var rawValue: TheProtocol {
    return TheType1()
  }
}

func aTransformer(input: Int) -> TheEnum {
  if input % 2 == 0 {
    return .case1
  } else {
    return .case2
  }
}

func theProblem(input: Int?) {
  var enumValue: TheEnum?

  if let input = input {
    enumValue = aTransformer(input: input) // Ok
  }

  _ = enumValue // To silence the warning
}
