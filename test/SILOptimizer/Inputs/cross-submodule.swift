
@inline(never)
@_semantics("optimize.no.crossmodule")
private func printit(_ x: Any) {
  print(x)
}

@inline(never)
public func genericSubmoduleFunc<T>(_ t: T) {
  printit(t)
}

