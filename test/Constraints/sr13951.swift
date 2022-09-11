// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/56348

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

  func test_arg_position(_: TheEnum?) {}

  if let input = input {
    enumValue = aTransformer(input: input) // Ok
    let _: TheEnum? = enumValue // Ok
    let _: TheEnum? = aTransformer(input: input)  // Ok
    let _: TheEnum?? = enumValue // Ok
    let _: TheEnum?? = aTransformer(input: input)  // Ok
    test_arg_position(aTransformer(input: input)) // Ok
    test_arg_position(enumValue) // Ok
  }

  _ = enumValue // To silence the warning
}
