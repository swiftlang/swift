
extension Foo: RawRepresentable {}

enum Bar: Int { case A }

// expected-error@+1{{redundant conformance of 'Bas' to protocol 'RawRepresentable'}}
extension Bas: RawRepresentable {}

