
public func incrementByOne(_ x: Int) -> Int {
  return x + 1
}

@_semantics("optimize.no.crossmodule")
public func incrementByOneNoCMO(_ x: Int) -> Int {
  return x + 1
}

