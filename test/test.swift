func foo<T>(_: T, _: T) {}
foo(Foo.a, Foo.b) // Ok in Swift 4 because we strip labels from the arguments
