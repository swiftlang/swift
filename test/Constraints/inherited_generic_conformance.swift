// RUN: %target-swift-frontend -parse -verify %s

protocol P {}

class GenericBase<T>: P {}

class Derived: GenericBase<Int> {}

func foo<T: P>(x: T) {}

foo(Derived())
