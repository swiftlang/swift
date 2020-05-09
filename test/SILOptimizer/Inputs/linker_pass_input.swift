
@_silgen_name("unknown")
public func unknown() -> ()

@inline(never)
@inlinable
public func doSomething() {
  unknown()
}

public func doSomething2() {
  unknown()
}

@inline(never)
public func doSomething3<T>(_ a:T) {
  unknown()
}

@usableFromInline struct A {
  @usableFromInline init() {}
}

@inline(never)
@inlinable
public func callDoSomething3() {
  doSomething3(A())
}
