public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if AFTER
@_weakLinked
public struct NewStruct<T> {
  var t: T

  public init(_ t: T) {
    self.t = t
  }
}
#endif
