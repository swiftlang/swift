// RUN: %target-typecheck-verify-swift -solver-disable-short-conversions
// RUN: %target-typecheck-verify-swift -solver-enable-short-conversions

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

// This was also broken.

class Base {}

class Derived: Base, RawRepresentable {
  typealias RawValue = any TheProtocol

  required init?(rawValue: any TheProtocol) {}

  var rawValue: any TheProtocol {
    fatalError()
  }
}

func f() {
  let d = Derived(rawValue: TheType1())
  let _: Base? = d
  let _: Base?? = d
  let _: Derived? = d
  let _: Derived?? = d
}