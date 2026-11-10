// RUN: %target-typecheck-verify-swift

func foo<T: _AnyReference>(_: T) {}

typealias Alias = _AnyReference

class C : _AnyReference {}

class D<T> where T: _AnyReference {}
