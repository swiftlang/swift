
open class Foo {
  open func doSomething(_ f: Foo) {}
}

@_inlineable public func callFoo(f: Foo) {
  f.doSomething(f)
}
