
@asmname("unknown")
public func unknown() -> ()

public func doSomething() {
  unknown()
}

@semantics("stdlib.noimport")
public func doSomething2() {
  unknown()
}
