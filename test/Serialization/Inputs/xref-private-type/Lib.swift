import LibCore

public let lazyFoo = Foo()
public func testFoo(_: Foo = lazyFoo) {}
public let lazyBar = Bar()
public func testBar(_: Bar = lazyBar) {}
public let lazyBaz = Baz()
public func testBaz(_: Baz = lazyBaz) {}
