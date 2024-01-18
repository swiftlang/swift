enum Foo {
    case one
    case two
    case three
}

func doSomething(_ value: Foo) {
    switch value {
    case .one:
        print("Hello")
    case .two:
        print("Goodbye")
    case .three: break
    }
}
