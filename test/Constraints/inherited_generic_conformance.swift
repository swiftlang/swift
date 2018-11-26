// RUN: %target-typecheck-verify-swift

protocol P {}

class GenericBase<T>: P {}

class Derived: GenericBase<Int> {}

func foo<T: P>(_ x: T) {}

foo(Derived())
