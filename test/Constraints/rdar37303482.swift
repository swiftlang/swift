// RUN: %target-typecheck-verify-swift

class P {}
class A : P {
  func foo() throws {}
}

class B : P {
  func foo() throws {}
}

typealias C = (P) throws -> Void
typealias E = (c: P.Type, arr: [(String, C)])

func foo<T: P>(_: [(String, (T) -> () throws -> Void)]) -> E { fatalError() }
func foo<T: P>(_: [(String, (T) -> () -> Void)]) -> E { fatalError() }

var arr = [E]()
arr.append(foo([("a", A.foo)]))
arr.append(foo([("b", B.foo)]))
