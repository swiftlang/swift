// RUN: %empty-directory(%t)
// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %s

func noArgs() {}
func oneArg(x: Int) {}
func oneUnlabeledArg(_ x: Int) {}

typealias FunctionAlias = (_ x: inout Int) -> Bool
typealias FunctionAliasNoLabel = (Int) -> Bool

func manyArgs(x: Int, y: Int, _ z: Bool, _ a: String) throws -> [Int] {
  return []
}

func rethrowing(_ f: (Bool) throws -> Int) rethrows -> Int {
  return try f(false)
}
