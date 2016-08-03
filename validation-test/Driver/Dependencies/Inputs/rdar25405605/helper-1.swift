enum Foo {
    case one
    case two
}

func doSomething(_ value: Foo) {
    switch value {
    case .one:
        print("Hello")
    case .two:
        print("Goodbye")
    }
}
