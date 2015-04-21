
@asmname("unknown")
public func unknown() -> ()

public func doSomething() {
  unknown()
}

@_semantics("stdlib_binary_only")
public func doSomething2() {
  unknown()
}
