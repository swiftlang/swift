
open class Foo {
  open func doSomething(_ f: Foo) {}
}

@inlinable public func callFoo(f: Foo) {
  f.doSomething(f)
}
