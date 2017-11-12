
@_silgen_name("unknown")
public func unknown() -> ()

@_inlineable
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

@_versioned struct A {
  @_versioned init() {}
}

@inline(never)
@_inlineable
public func callDoSomething3() {
  doSomething3(A())
}
