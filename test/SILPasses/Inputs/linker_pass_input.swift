
@asmname("unknown")
public func unknown() -> ()

public func doSomething() {
  unknown()
}

@_semantics("stdlib_binary_only")
public func doSomething2() {
  unknown()
}

@inline(never)
@_semantics("stdlib_binary_only")
public func doSomething3<T>(a:T) {
  unknown()
}

struct A {}
@inline(never)
public func callDoSomething3() {
  doSomething3(A())
}
