// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

public func expectEqualMethodsForDomain<
SelfType, ArgumentType, Result : Equatable
>(
  _ selfs: [SelfType], _ arguments: [ArgumentType],
  _ function1: (SelfType) -> (ArgumentType) -> Result,
  _ function2: (SelfType) -> (ArgumentType) -> Result
) { fatalError() }

func test(ascii0to126: [UnicodeScalar], ascii1to127: [UnicodeScalar]) {
  expectEqualMethodsForDomain(
    ascii0to126,
    ascii1to127,
    { x in { String(x) < String($0) } },
    { x in { String(Character(x)) < String(Character($0)) } })
}
