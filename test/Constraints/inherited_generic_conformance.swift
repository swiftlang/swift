// RUN: %target-swift-frontend -parse -verify %s

protocol P {}

class GenericBase<T>: P {}

class Derived: GenericBase<Int> {}

func foo<T: P>(_ x: T) {}

foo(Derived())
