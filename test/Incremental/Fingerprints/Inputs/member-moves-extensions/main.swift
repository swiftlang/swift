extension S {
  public func foo(_ parameter: Int = 42) {}
  public func bar(_ parameter: Int = 42) {}
}

S().foo()
S().bar()
