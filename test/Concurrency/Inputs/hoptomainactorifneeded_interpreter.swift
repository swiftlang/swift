
public func useClosure(_ x: @escaping @MainActor () -> ()) {
  typealias Func = () -> ()
  let x2 = unsafeBitCast(x, to: Func.self)
  x2()
}
